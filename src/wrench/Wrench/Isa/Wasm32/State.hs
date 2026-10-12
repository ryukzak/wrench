{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
The machine's state and every primitive that touches it: where code, data and
the two stacks sit in memory, the counted word\/byte accessors, and the
operand and control stacks -- the latter including the binary format of a
control record, since the format and the LIFO discipline that gives it meaning
are one thing.

What is /not/ here is any instruction's behaviour. "Wrench.Isa.Wasm32.Execute"
builds on these primitives; this module only guarantees that each one leaves
the state consistent, or reports why it cannot.
-}
module Wrench.Isa.Wasm32.State (
    -- * The state
    Wasm32St (..),
    BranchPlan (..),
    ScopePlan (..),

    -- * Memory map
    memTop,
    operandFloor,
    stackRoot,

    -- * Memory access
    getWord,
    setWord,
    getByte,
    setByte,
    countMemAccess,
    setPc,
    nextPc,
    raiseInternalError,

    -- * Operand stack
    pushValue,
    popValue,
    popValues,
    localAddr,
    checkOperandRoom,

    -- * Control stack
    -- $layout
    Scope (..),
    ControlTag (..),
    scopeAddrs,
    ctrlTopOf,
    readScopeTagAt,
    checkOwnScopes,
    pushScope,
    popScope,
    checkControlRoom,
    decodeScopeTag,
    scopeRecordWords,
    scopeRecordBytes,
    scopeWordAddrs,
    encodeScope,
    decodeScope,
    nullAddr,
    describeScope,
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Default (def)
import Relude
import Relude.Extra (safeToEnum)
import Wrench.Isa.Wasm32.Instruction
import Wrench.Machine.Memory
import Wrench.Machine.Types

-- | What a @br@ does, worked out from where it is written.
--
-- Both halves are static: the scope @depth@ levels out is fixed by the
-- nesting the branch sits in, and so is whether reaching it means
-- re-entering a @loop@ or stepping out of a @block@. The machine needs
-- no stored link to follow and no record to read in order to decide.
data BranchPlan = BranchPlan
    { bpTarget :: Int
    -- ^ Where to jump: a @loop@'s first body instruction, or just past a
    -- @block@'s own matching @end@.
    , bpPop :: Int
    -- ^ Records to retire -- the ones nested between the branch and its
    -- target, plus the target itself when it is a @block@, since
    -- reaching a block closes it and reaching a loop does not.
    , bpCheck :: Int
    -- ^ Records whose kind the machine confirms before touching
    -- anything: everything it would pop, plus the one it lands on. See
    -- 'checkOwnScopes'.
    }
    deriving (Eq, Show)

-- | Everything about a loaded program's control flow that is fixed
-- before it runs, so that nothing has to be searched for while it does.
data ScopePlan = ScopePlan
    { spTargets :: IntMap Int
    -- ^ @block@\/@loop@ to its own matching @end@, @if@\/@else@ to where
    -- its skip path resumes. See 'resolveScopeTargets'.
    , spBranches :: IntMap BranchPlan
    -- ^ @br@\/@br_if@ to what it does. See 'resolveBranches'.
    , spEndPops :: IntSet
    -- ^ The @end@s that close a @block@ or @loop@, as opposed to an
    -- @if@\'s @end@, which closes nothing. Knowing which is which here
    -- is what replaced comparing a record's @csEnd@ against @pc@ at run
    -- time to find out.
    }
    deriving (Eq, Show)

instance Semigroup ScopePlan where
    a <> b =
        ScopePlan
            { spTargets = spTargets a <> spTargets b
            , spBranches = spBranches a <> spBranches b
            , spEndPops = spEndPops a <> spEndPops b
            }

instance Monoid ScopePlan where
    mempty = ScopePlan mempty mempty mempty

-- | The machine: a program counter, the two stack pointers and the frame
-- they describe, the loaded program's 'ScopePlan', and memory. Opaque on
-- purpose -- reports reach it through 'reprState' and the @stack@,
-- @locals@, @layout@ and @dump@ views, which is everything outside this
-- module ever needs from it. The \"The two stacks\" section below explains
-- what the pointers mean.
data Wasm32St w = Wasm32St
    { pc :: Int
    , sp :: Int
    -- ^ Top of the operand\/locals stack (descending)
    , ctrlBase :: Int
    -- ^ The root both stacks grow from: the operand stack descends from
    -- it, the control stack ascends from it. Set by 'initState' and
    -- relocated by @sp.init@ (see 'stackRoot').
    , ctrlSp :: Int
    -- ^ The control stack's frontier: one past the innermost live
    -- control record, ascending from 'ctrlBase' the way @sp@ descends
    -- from the top of memory. Every record is the same width and they
    -- close in strict LIFO order, so this one pointer locates all of
    -- them ('scopeAddrs') and closing one is a bump-pointer retreat.
    , frameBase :: Int
    -- ^ Base address of the *active* function's locals (param 0's
    -- address): carved out of the caller's already-pushed arguments by
    -- @call@, saved in the callee's 'CallScope', restored by @return@.
    , localCount :: Int
    -- ^ How many memory accesses the *last executed* instruction made,
    -- its own fetch included. Reset per step in 'instructionStep';
    -- exposed to reports as @memAccesses@.
    , scopePlan :: ScopePlan
    -- ^ Everything about the loaded program's control flow that is fixed
    -- before it runs -- where each scope instruction jumps to, what each
    -- @br@ does, which @end@s close a record. Built once by 'initState'
    -- (see 'resolveProgram'), read-only thereafter. A real engine keeps
    -- the same tables, for the same reason: the alternative is searching
    -- memory for an answer that was never going to change.
    , mem :: IoMem (Wasm32Isa w w) w
    , memAccessCount :: Int
    , stopped :: Bool
    , internalError :: Maybe Text
    }
    deriving (Show)

type instance MemOf (Wasm32St w) = IoMem (Wasm32Isa w w) w

-- | Where code+data end and the stacks' own region begins: the lower
-- half of memory holds code+data, the upper half the stacks. Code+data
-- spilling past it is not checked.
memTop :: IoMem (Wasm32Isa w w) w -> Int
memTop IoMem{mIoCells = Mem{memorySize}} = memorySize `div` 2

-- | The lowest address the operand stack may occupy: 'memTop', except
-- that a memory-mapped I\/O port mapped at or above it pushes the floor
-- up past the port.
--
-- A port is ordinary memory to every instruction, so without this a
-- deep-enough operand stack writes /through/ a port: with
-- @memory_size: 0x100@ the stack region starts at @0x80@, which is
-- exactly where the examples map theirs, and the 24th push would emit a
-- character instead of reporting an overflow. Every example that maps
-- ports gives itself at least @0x200@, keeping them in the code+data
-- half where they belong, so this only ever fires on a memory too small
-- for the ports it asks for -- and then it fires as a stack overflow
-- naming the port, instead of silently doing I\/O.
operandFloor :: forall w. (IsWord w) => IoMem (Wasm32Isa w w) w -> Int
operandFloor mem@IoMem{mIoKeys} =
    foldr max (memTop mem) [port + byteSizeT @w | port <- mIoKeys, port >= memTop mem]

-- | The root both stacks grow from, three quarters of the way up the
-- stack's half of memory: the operand stack descends from it toward
-- 'memTop', the control stack ascends from it toward the end of memory.
-- Neither can reach the other, and each has one fixed wall of its own.
--
-- Three quarters rather than half because a program pushes far more
-- operands than it opens scopes -- a @block@ or a call is eight bytes,
-- and the deepest nesting in the examples is four. A program wanting
-- the other balance moves the root with @sp.init@.
--
-- A push decrements before writing, so the root itself is never written
-- to, and @_start@'s @frameBase@ sits one word below it -- the relation
-- @call@ sets up for a frame with no parameters (see 'localAddr').
stackRoot :: IoMem (Wasm32Isa w w) w -> Int
stackRoot mem@IoMem{mIoCells = Mem{memorySize}} =
    let base = memTop mem in base + 3 * (memorySize - base) `div` 4

-- | Jump: the one way @pc@ ever moves other than 'nextPc'.
setPc :: Int -> State (Wasm32St w) ()
setPc addr = modify $ \st -> st{pc = addr}

-- | Fall through to the instruction after this one, whose length is the
-- only thing that decides where that is.
nextPc :: Wasm32Isa w w -> State (Wasm32St w) ()
nextPc instruction = do
    Wasm32St{pc} <- get
    setPc (pc + byteSize instruction)

-- | Trap: record why the machine cannot go on. The simulator stops at the
-- end of this step and reports the message.
raiseInternalError :: Text -> State (Wasm32St w) ()
raiseInternalError msg = modify $ \st -> st{internalError = Just msg}

-- | Counts every 'getWord'\/'setWord'\/'getByte'\/'setByte'\/
-- 'readInstruction' call -- operand pushes\/pops, control-record and
-- local access, and the instruction's own fetch -- toward
-- 'memAccessCount', reset per step in 'instructionStep'.
countMemAccess :: State (Wasm32St w) ()
countMemAccess = modify $ \st -> st{memAccessCount = memAccessCount st + 1}

-- | Read a word, counting the access. A failure traps and yields 'def', so
-- the step finishes and the error is reported once.
getWord :: (IsWord w) => Int -> State (Wasm32St w) w
getWord addr = do
    countMemAccess
    st@Wasm32St{mem} <- get
    case readWord mem addr of
        Right (mem', w) -> put st{mem = mem'} >> return w
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return def

-- | Write a word, counting the access.
setWord :: (IsWord w) => Int -> w -> State (Wasm32St w) ()
setWord addr w = do
    countMemAccess
    st@Wasm32St{mem} <- get
    case writeWord mem addr w of
        Right mem' -> put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err

-- | Read one byte, counting the access.
getByte :: (IsWord w) => Int -> State (Wasm32St w) Word8
getByte addr = do
    countMemAccess
    st@Wasm32St{mem} <- get
    case readByte mem addr of
        Right (mem', b) -> put st{mem = mem'} >> return b
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return 0

-- | Write one byte, counting the access.
setByte :: (IsWord w) => Int -> Word8 -> State (Wasm32St w) ()
setByte addr b = do
    countMemAccess
    st@Wasm32St{mem} <- get
    case writeByte mem addr b of
        Right mem' -> put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err

-- | The two stacks grow apart from 'ctrlBase', so neither can ever reach
-- the other; what each can reach is its own end of the stack region.
-- These two check a push against that wall before it happens, so an
-- overflow is reported where it starts rather than found later as
-- corrupted globals or a confusing memory-access error. The two get
-- different messages because the fixes differ: too much on the operand
-- stack, or too deep a nesting of scopes and calls. A real machine
-- draws these lines with a stack-limit register.
checkOperandRoom :: forall w. (IsWord w) => Int -> State (Wasm32St w) () -> State (Wasm32St w) ()
checkOperandRoom newSp act = do
    Wasm32St{mem} <- get
    let wall = operandFloor mem
    if newSp >= wall
        then act
        else
            raiseInternalError $
                "operand stack overflow: push to "
                    <> hexAddr 2 newSp
                    <> " would reach below the stack region at "
                    <> hexAddr 2 wall

-- | 'checkOperandRoom' for the other stack: its wall is the end of memory.
checkControlRoom :: (IsWord w) => Int -> State (Wasm32St w) () -> State (Wasm32St w) ()
checkControlRoom newCtrlSp act = do
    Wasm32St{mem} <- get
    let wall = memCapacity mem
    if newCtrlSp <= wall
        then act
        else
            raiseInternalError $
                "control stack overflow: scope record ending at "
                    <> hexAddr 2 newCtrlSp
                    <> " would reach past the end of memory at "
                    <> hexAddr 2 wall

-- | The operand stack grows downward, and @sp@ names the top word
-- rather than the next free slot -- so a push decrements, then writes.
pushValue :: forall w. (IsWord w) => w -> State (Wasm32St w) ()
pushValue value = do
    Wasm32St{sp} <- get
    let sp' = sp - byteSizeT @w
    checkOperandRoom sp' $ do
        modify $ \st -> st{sp = sp'}
        setWord sp' value

-- | No underflow guard: popping past the bottom of the active frame's
-- own operand stack reads whatever is there. Running off the top of
-- memory is caught, but as a plain memory-access error.
popValue :: forall w. (IsWord w) => State (Wasm32St w) w
popValue = do
    Wasm32St{sp} <- get
    value <- getWord sp
    modify $ \st -> st{sp = sp + byteSizeT @w}
    return value

-- | Address of local @i@: a fixed offset from @frameBase@, unaffected by
-- either stack pointer -- which is the point, since a value merely
-- sitting on a stack isn't. Locals count *down*, so param 0 sits
-- highest: the order the caller pushed them in.
localAddr :: forall w. (IsWord w) => Int -> State (Wasm32St w) Int
localAddr i = do
    Wasm32St{frameBase} <- get
    return $ frameBase - i * byteSizeT @w

-- | Addresses of every open control record, innermost first. The
-- control stack is a fixed-stride array growing up from 'ctrlBase', so
-- this is arithmetic -- no stored links, no memory read to find the
-- next record.
scopeAddrs :: forall w. (IsWord w) => Wasm32St w -> [Int]
scopeAddrs Wasm32St{ctrlSp, ctrlBase} =
    takeWhile (>= ctrlBase) [ctrlSp - stride, ctrlSp - 2 * stride ..]
    where
        stride = scopeRecordBytes @w

-- | Address of the innermost open record, or 'nullAddr' when nothing is
-- open -- derived from @ctrlSp@ rather than tracked alongside it.
ctrlTopOf :: forall w. (IsWord w) => Wasm32St w -> Int
ctrlTopOf = fromMaybe nullAddr . listToMaybe . scopeAddrs @w

-- | Which kind of record sits at @addr@, reading its first word only --
-- half of what decoding a whole record costs. Enough for a caller that
-- already knows what the record should be and only has to confirm it.
readScopeTagAt :: forall w. (IsWord w) => Int -> State (Wasm32St w) (Either Text ControlTag)
readScopeTagAt addr = do
    word0 <- getWord @w addr
    return $ first (\err -> "control stack corrupted at " <> hexAddr 2 addr <> ": " <> err) (decodeScopeTag word0)

-- | Confirm the innermost @n@ records are the @block@\/@loop@ records the
-- 'ScopePlan' says enclose this point, before a branch retires any of
-- them.
--
-- A branch's target and its pop count come from where it is written, so
-- the one thing the machine cannot know from that alone is whether
-- control /arrived/ the way the nesting implies. It cannot if a function
-- body sits inside another function's scope and is entered by @call@ --
-- and then the records the branch would pop include that call's own,
-- whose loss would strand the callee. One word per record is what it
-- costs to refuse instead: the tag, nothing else.
--
-- The branch that matters most pays least. @br 0@ back into a loop
-- retires nothing and checks the one record it re-enters.
checkOwnScopes :: forall w. (IsWord w) => Int -> State (Wasm32St w) (Either Text ())
checkOwnScopes n = do
    st <- get
    let addrs = take n (scopeAddrs @w st)
    if length addrs < n
        then
            return
                $ Left
                $ "br/br_if: depth reaches past the "
                    <> show (length addrs)
                    <> " scope(s) actually open"
        else go addrs
    where
        go [] = return $ Right ()
        go (addr : rest) =
            readScopeTagAt @w addr >>= \case
                Left err -> return $ Left err
                Right CallTag -> return $ Left "br/br_if: depth reaches past the enclosing function; use return to leave it"
                Right _ -> go rest

-- | Push a scope at the current frontier and advance it. Never touches
-- @sp@ -- the two stacks grow apart from 'ctrlBase'.
pushScope :: forall w. (IsWord w) => Scope -> State (Wasm32St w) ()
pushScope scope = do
    Wasm32St{ctrlSp} <- get
    let step = byteSizeT @w
        ctrlSp' = ctrlSp + scopeRecordBytes @w
    checkControlRoom ctrlSp' $ case encodeScope @w scope of
        Left err -> raiseInternalError $ "control record doesn't fit: " <> err
        Right words_ -> do
            forM_ (zip [0 ..] words_) $ \(i, word) -> setWord (ctrlSp + i * step) word
            modify $ \st -> st{ctrlSp = ctrlSp'}

-- | Pop the innermost record: a bump allocator's "pop" is nothing but
-- moving the frontier back, so this never touches memory.
popScope :: forall w. (IsWord w) => State (Wasm32St w) ()
popScope = modify $ \st -> st{ctrlSp = ctrlSp st - scopeRecordBytes @w}

-- | Pop @n@ values, oldest first -- the order a caller pushed them in, so
-- pushing the result back preserves it.
popValues :: forall w. (IsWord w) => Int -> State (Wasm32St w) [w]
popValues n = reverse <$> replicateM n popValue

-- | One control record's payload -- a typed shape per kind, so a
-- mismatch between kind and payload can't be constructed.
--
-- Every record is the same 'scopeRecordWords' words whatever its kind,
-- making the control stack a fixed-stride array: the record @n@ scopes
-- out starts exactly @n+1@ strides below the frontier. That is what lets
-- @br@ reach its target by arithmetic rather than a chain of stored
-- links, and why no record describes its own predecessor.
--
-- 'LoopScope' and 'BlockScope' both carry @csEnd@: a plain @end@
-- compares its own address against it to tell "closes this scope" from
-- "closes an @if@" (only the former pops).
data Scope
    = -- | Reaching this (via @br@\/@br_if@) leaves it open: jump to
      -- @csStart@ (just after @loop@) to run the body again.
      LoopScope {csStart :: Int, csEnd :: Int}
    | -- | Reaching this closes it: jump just past @csEnd@, popping it
      -- off the stack -- unlike a loop, nothing left to continue.
      BlockScope {csEnd :: Int}
    | -- | Pushed by @call@, closed by @return@ only. @csCallerFrameBase@\/
      -- @csCallerLocalCount@ are the caller's own values, restored on
      -- return; @csReturnPc@\/@csResultCount@ drive where to jump back
      -- to and how many values to carry across.
      CallScope {csCallerFrameBase :: Int, csCallerLocalCount :: Int, csReturnPc :: Int, csResultCount :: Int}
    deriving (Eq, Show)

-- | How many machine words one record occupies, whatever its kind. A
-- @block@ leaves its second word unused; paying that word buys a
-- fixed-stride control stack (see 'Scope').
scopeRecordWords :: Int
scopeRecordWords = 2

-- | How many bytes one record occupies.
scopeRecordBytes :: forall w. (IsWord w) => Int
scopeRecordBytes = scopeRecordWords * byteSizeT @w

-- | The word addresses one record at @addr@ occupies, in order -- what
-- both the interpreter and the report views read to decode it.
scopeWordAddrs :: forall w. (IsWord w) => Int -> [Int]
scopeWordAddrs addr = [addr + i * byteSizeT @w | i <- [0 .. scopeRecordWords - 1]]

-- | The persisted tag, kept separate from 'Scope' so the typed shape
-- never has to double as the stored representation. Three kinds in two
-- bits leaves one bit pattern unused, which 'decodeScope' rejects.
data ControlTag = LoopTag | BlockTag | CallTag
    deriving (Bounded, Enum, Eq, Show)

-- | Which kind of record a word pair starts, from its first word alone.
--
-- The tag sits in the top bits of word 0 ('tagShift'), so asking only
-- what kind of record is here costs half of what 'decodeScope' costs --
-- and a caller that has already worked out what a record /should/ say
-- needs nothing more than this to find out whether it was right.
decodeScopeTag :: forall w. (IsWord w) => w -> Either Text ControlTag
decodeScopeTag word0 =
    maybe (Left $ "no record kind has tag " <> show tag) Right (safeToEnum tag)
    where
        tag = unpackField tagBitWidth tagShift (bitsFromWord word0)

tagOfScope :: Scope -> ControlTag
tagOfScope LoopScope{} = LoopTag
tagOfScope BlockScope{} = BlockTag
tagOfScope CallScope{} = CallTag

-- | See the @Layout@ section at the foot of this module for the field
-- layout these widths produce.
--
-- Every address field is the same 'addrBitWidth' in the same bits, and
-- every count the same 'countBitWidth' in theirs: no part of the address
-- space deserves more trust than another, and a uniform layout is one
-- decoder in hardware rather than three.
tagBitWidth, addrBitWidth, countBitWidth :: Int
tagBitWidth = 2
addrBitWidth = 22
countBitWidth = 8

tagShift, countShift :: Int
countShift = addrBitWidth
tagShift = addrBitWidth + countBitWidth

bitMask :: Int -> Int
bitMask n = (1 `shiftL` n) - 1

-- | Pack one field of @width@ bits at bit offset @shiftAmt@, failing
-- rather than truncating a value that doesn't fit. Masking off a high
-- bit doesn't make a smaller number, it makes a different one: a
-- truncated @csReturnPc@ returns to the wrong address and a truncated
-- @csResultCount@ pops the wrong number of values, both silently.
packField :: Text -> Int -> Int -> Int -> Either Text Int
packField name width shiftAmt value
    | value < 0 || value > bitMask width =
        Left $ name <> " " <> show value <> " doesn't fit in " <> show width <> " bits"
    | otherwise = Right (value `shiftL` shiftAmt)

-- | The inverse of 'packField'. Total: anything a field's own bits can
-- hold is a value that field can mean.
unpackField :: Int -> Int -> Int -> Int
unpackField width shiftAmt word = (word `shiftR` shiftAmt) .&. bitMask width

packAddr, packCount :: Text -> Int -> Either Text Int
packAddr name = packField name addrBitWidth 0
packCount name = packField name countBitWidth countShift

unpackAddr, unpackCount :: Int -> Int
unpackAddr = unpackField addrBitWidth 0
unpackCount = unpackField countBitWidth countShift

packTag :: ControlTag -> Int
packTag = (`shiftL` tagShift) . fromEnum

-- | A record's header packs its tag into the top bits, so the raw bit
-- pattern often doesn't fit @w@'s *signed* range even though it's a
-- valid word -- plain 'toEnum'\/'fromEnum' would reject or corrupt it.
-- These two treat it as the unsigned bit pattern it actually is.
wordFromBits :: forall w. (IsWord w) => Int -> w
wordFromBits = toSign . fromIntegral

bitsFromWord :: forall w. (IsWord w) => w -> Int
bitsFromWord = fromIntegral . fromSign

-- | 'Left' when a field's value doesn't fit the bits the format gives
-- it -- see 'packField'.
encodeScope :: forall w. (IsWord w) => Scope -> Either Text [w]
encodeScope scope =
    map wordFromBits <$> case scope of
        BlockScope{csEnd} -> do
            end <- packAddr "csEnd" csEnd
            return [tag .|. end, 0]
        LoopScope{csStart, csEnd} -> do
            start <- packAddr "csStart" csStart
            end <- packAddr "csEnd" csEnd
            return [tag .|. start, end]
        CallScope{csCallerFrameBase, csCallerLocalCount, csReturnPc, csResultCount} -> do
            frameBase <- packAddr "csCallerFrameBase" csCallerFrameBase
            localCount <- packCount "csCallerLocalCount" csCallerLocalCount
            returnPc <- packAddr "csReturnPc" csReturnPc
            resultCount <- packCount "csResultCount" csResultCount
            return [tag .|. localCount .|. frameBase, resultCount .|. returnPc]
    where
        tag = packTag (tagOfScope scope)

-- | The inverse of 'encodeScope'. 'Left' for a word pair that doesn't
-- decode as any real record -- the one unused tag pattern, or the wrong
-- number of words -- rather than guessing at a shape, since the only way
-- to see one is a control stack something has corrupted.
decodeScope :: forall w. (IsWord w) => [w] -> Either Text Scope
decodeScope words_
    | [word0, word1] <- words_
    , let bits0 = bitsFromWord word0
    , let bits1 = bitsFromWord word1
    , let tag = unpackField tagBitWidth tagShift bits0 =
        -- Matched over the tag's own constructors rather than against
        -- 'fromEnum' of each: a fourth record kind is then a
        -- missing-pattern warning here, not a bit pattern this silently
        -- reports as corruption.
        case safeToEnum tag of
            Just BlockTag -> Right BlockScope{csEnd = unpackAddr bits0}
            Just LoopTag -> Right LoopScope{csStart = unpackAddr bits0, csEnd = unpackAddr bits1}
            Just CallTag ->
                Right
                    CallScope
                        { csCallerFrameBase = unpackAddr bits0
                        , csCallerLocalCount = unpackCount bits0
                        , csReturnPc = unpackAddr bits1
                        , csResultCount = unpackCount bits1
                        }
            Nothing -> Left $ "no record kind has tag " <> show tag
    | otherwise =
        Left $ "expected " <> show scopeRecordWords <> " words, got " <> show (length words_)

-- | Sentinel for "no enclosing control record" and "no active function
-- frame".
nullAddr :: Int
nullAddr = -1

-- | A Haskell-record-literal rendering of a scope, close to how 'Scope'
-- itself reads in source -- addresses follow @showAddr@'s format, plain
-- counts stay decimal.
describeScope :: (Int -> Text) -> Scope -> Text
describeScope showAddr scope = case scope of
    LoopScope{csStart, csEnd} ->
        "LoopScope { csStart = " <> showAddr csStart <> ", csEnd = " <> showAddr csEnd <> " }"
    BlockScope{csEnd} ->
        "BlockScope { csEnd = " <> showAddr csEnd <> " }"
    CallScope{csCallerFrameBase, csCallerLocalCount, csReturnPc, csResultCount} ->
        "CallScope { csCallerFrameBase = "
            <> showAddr csCallerFrameBase
            <> ", csCallerLocalCount = "
            <> show csCallerLocalCount
            <> ", csReturnPc = "
            <> showAddr csReturnPc
            <> ", csResultCount = "
            <> show csResultCount
            <> " }"

-- $layout
--
-- The exact byte layout, in Erlang bit syntax -- illustrative notation
-- only, the project is Haskell, but a precise and standard way to say
-- exactly which bits are which. Most-significant field first:
--
-- > %% block -- 8 bytes
-- > <<Tag:2, 0:8, CsEnd:22>>, <<0:32>>
-- >
-- > %% loop -- 8 bytes
-- > <<Tag:2, 0:8, CsStart:22>>, <<0:10, CsEnd:22>>
-- >
-- > %% call -- 8 bytes
-- > <<Tag:2, CsCallerLocalCount:8, CsCallerFrameBase:22>>, <<0:2, CsResultCount:8, CsReturnPc:22>>
--
-- Every address-shaped field -- @CsStart@, @CsEnd@ and @CsReturnPc@ (code
-- addresses) and @CsCallerFrameBase@ (a stack address) -- gets the same
-- uniform 'addrBitWidth' of 22 bits, and every count the same
-- 'countBitWidth' of 8, in the same bit positions. There is no ISA-level
-- reason to trust one part of the address space more than another, and a
-- uniform layout means one decoder rather than three. 22 bits covers 4 MiB,
-- far past the 64 KiB memory limit; 8 bits matches the one-byte counts
-- @call@ itself carries, so what a call site can declare and what its
-- record can hold agree exactly.
--
-- Nothing is silently truncated. A field value that does not fit stops the
-- program naming the field ('packField'), and an instruction immediate out
-- of range for its own encoding is rejected where it is written, by the
-- field guard in @Wrench.Isa.Wasm32@'s 'derefMnemonic'.
--
-- @Tag@ needs only 2 bits for three kinds. The one unused pattern is not a
-- valid record, and reading it reports a corrupted control stack rather
-- than guessing at a shape -- see 'decodeScope'.

-- $layout
--
-- The exact byte layout, in Erlang bit syntax -- illustrative notation
-- only, the project is Haskell, but a precise and standard way to say
-- exactly which bits are which. Most-significant field first:
--
-- > %% block -- 8 bytes
-- > <<Tag:2, 0:8, CsEnd:22>>, <<0:32>>
-- >
-- > %% loop -- 8 bytes
-- > <<Tag:2, 0:8, CsStart:22>>, <<0:10, CsEnd:22>>
-- >
-- > %% call -- 8 bytes
-- > <<Tag:2, CsCallerLocalCount:8, CsCallerFrameBase:22>>, <<0:2, CsResultCount:8, CsReturnPc:22>>
--
-- Every address-shaped field -- @CsStart@, @CsEnd@ and @CsReturnPc@ (code
-- addresses) and @CsCallerFrameBase@ (a stack address) -- gets the same
-- uniform 'addrBitWidth' of 22 bits, and every count the same
-- 'countBitWidth' of 8, in the same bit positions. There is no ISA-level
-- reason to trust one part of the address space more than another, and a
-- uniform layout means one decoder rather than three. 22 bits covers 4 MiB,
-- far past the 64 KiB memory limit; 8 bits matches the one-byte counts
-- @call@ itself carries, so what a call site can declare and what its
-- record can hold agree exactly.
--
-- Nothing is silently truncated. A field value that does not fit stops the
-- program naming the field ('packField'), and an instruction immediate out
-- of range for its own encoding is rejected where it is written, by the
-- field guard in @Wrench.Isa.Wasm32@'s 'derefMnemonic'.
--
-- @Tag@ needs only 2 bits for three kinds. The one unused pattern is not a
-- valid record, and reading it reports a corrupted control stack rather
-- than guessing at a shape -- see 'decodeScope'.
