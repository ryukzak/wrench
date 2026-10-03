{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | A WebAssembly-inspired 32-bit ISA: push a constant, combine values
with arithmetic\/comparison ops, structured control flow, locals, and function
calls.
-}
module Wrench.Isa.Wasm32 (
    Wasm32Isa (..),
    Wasm32St (..),
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Default (def)
import Data.Text qualified as T
import Relude
import Relude.Extra ((!?))
import Text.Megaparsec (choice, try)
import Text.Megaparsec.Char (char, hspace, hspace1, string)
import Wrench.Isa.Wasm32.ControlRecord
import Wrench.Isa.Wasm32.Layout
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Report
import Wrench.Translator.Parser.Misc
import Wrench.Translator.Parser.Types
import Wrench.Translator.Types

data Wasm32Isa w l
    = I32Const l
    | I32Add
    | I32Sub
    | I32Mul
    | I32DivS
    | I32RemS
    | I32And
    | I32Or
    | I32Xor
    | I32Shl
    | I32ShrS
    | I32ShrU
    | I32Eq
    | I32LtS
    | I32LeS
    | I32GtS
    | I32GeS
    | I32LtU
    | I32LeU
    | I32GtU
    | I32GeU
    | -- Each memory instruction carries a static offset, added to the
      -- popped address; 'immediateBitWidth' bounds it. A store takes
      -- the value on top of the address, matching real WebAssembly.

      -- | Pop an address; push the word at address + offset.
      I32Load Int
    | -- | Pop a value then an address; write the word at address + offset.
      I32Store Int
    | I32Load8U Int
    | I32Load8S Int
    | I32Store8 Int
    | -- Each scope instruction carries a byte distance resolved at
      -- translate time (see 'resolveScopeTargets'): for `if`\/`else`,
      -- where to resume on the skip path; for `block`\/`loop`, where
      -- their own matching `end` is. `if` pushes no control record --
      -- nothing branches out of a taken branch early.

      -- | Pop a condition; skip forward if it is zero.
      If Int
    | -- | Skip past the matching `end`; reached by a taken `if` body
      -- falling through.
      Else Int
    | End
    | -- | A `br` target that jumps backward, to just after here.
      Loop Int
    | -- | A `br` target that jumps forward, past its matching `end` --
      -- what makes "break" possible, which `loop` alone does not.
      Block Int
    | -- | Branch to the enclosing scope @depth@ levels out (0 =
      -- innermost): a `loop` re-enters and stays open, a `block` closes
      -- along with everything nested inside it. See 'branchTo'.
      Br Int
    | -- | 'Br' behind a popped condition.
      BrIf Int
    | -- | Duplicate the top of the operand stack.
      Dup
    | -- | Discard the top of the operand stack.
      Drop
    | -- | Pop a condition, then two values; push the first-pushed of the
      -- two if the condition is non-zero, the second otherwise.
      Select
    | -- | Trap: a point the program believes it can never reach.
      Unreachable
    | -- | Push local @i@'s value.
      LocalGet Int
    | -- | Pop a value into local @i@.
      LocalSet Int
    | -- | 'LocalSet' that pushes the value back, leaving stack depth
      -- unchanged.
      LocalTee Int
    | -- | Claim @n@ more locals, zeroed, at indices @localCount@
      -- onward: the operand stack descends, so the words directly below
      -- the current frame's locals are exactly where the next ones
      -- belong. Written as a function's first instruction, it is what
      -- gives a function scratch space of its own -- private per
      -- activation, so unlike a `.data` cell it survives recursion.
      LocalsReserve Int
    | -- | Pop the call target -- an ordinary value, so a known target and
      -- one computed at run time are the same instruction -- with
      -- @paramCount@ values already pushed below it (they become the
      -- callee's locals) and @resultCount@ expected back. Neither count
      -- is checked against the callee.
      Call Int Int
    | -- | Return from the nearest enclosing 'Call', forwarding its
      -- declared results and closing any scope left open along the way.
      -- See 'closeScope'.
      Return
    | -- | Pop an address and make it the root both stacks grow from:
      -- operands descend below it, control records ascend above it. This
      -- is how a program trades operand space against scope and call
      -- depth; 'stackRoot' is the default split. Only meaningful before
      -- anything has been pushed, and nothing checks the address leaves
      -- either stack enough room.
      SpInit
    | Halt
    deriving (Eq, Functor, Show)

instance CommentStart (Wasm32Isa w l) where
    commentStart = ";"

instance (IsWord w) => MnemonicParser (Wasm32Isa w (Ref w)) where
    mnemonic =
        hspace *> cmd <* (hspace1 <|> eol' (commentStart @(Wasm32Isa _ _)))
        where
            cmd =
                choice
                    [ I32Const <$> cmd1 "i32.const" referenceWithDirective
                    , cmd0 "i32.add" I32Add
                    , cmd0 "i32.sub" I32Sub
                    , cmd0 "i32.mul" I32Mul
                    , cmd0 "i32.div_s" I32DivS
                    , cmd0 "i32.rem_s" I32RemS
                    , cmd0 "i32.and" I32And
                    , cmd0 "i32.or" I32Or
                    , cmd0 "i32.xor" I32Xor
                    , cmd0 "i32.shl" I32Shl
                    , cmd0 "i32.shr_s" I32ShrS
                    , cmd0 "i32.shr_u" I32ShrU
                    , cmd0 "i32.eq" I32Eq
                    , cmd0 "i32.lt_s" I32LtS
                    , cmd0 "i32.le_s" I32LeS
                    , cmd0 "i32.gt_s" I32GtS
                    , cmd0 "i32.ge_s" I32GeS
                    , cmd0 "i32.lt_u" I32LtU
                    , cmd0 "i32.le_u" I32LeU
                    , cmd0 "i32.gt_u" I32GtU
                    , cmd0 "i32.ge_u" I32GeU
                    , -- Longer alternative first: a bare-word `choice`
                      -- alternative doesn't backtrack once it has
                      -- consumed input, so "i32.load" ahead of
                      -- "i32.load8_u" would swallow the prefix and then
                      -- fail on "8_u". Same for "i32.store"/"i32.store8".
                      -- The `try`-wrapped alternatives below are immune.
                      memOp "i32.load8_u" I32Load8U
                    , memOp "i32.load8_s" I32Load8S
                    , memOp "i32.load" I32Load
                    , memOp "i32.store8" I32Store8
                    , memOp "i32.store" I32Store
                    , -- The four scope instructions take no operand in
                      -- source: their target is their own matching `end`,
                      -- which 'resolveStructure' fills in at translate
                      -- time. 'unresolvedTarget' marks the placeholder.
                      cmd0 "if" (If unresolvedTarget)
                    , cmd0 "else" (Else unresolvedTarget)
                    , cmd0 "end" End
                    , cmd0 "loop" (Loop unresolvedTarget)
                    , cmd0 "block" (Block unresolvedTarget)
                    , try (Br <$> cmd1 "br" intLit)
                    , try (BrIf <$> cmd1 "br_if" intLit)
                    , cmd0 "dup" Dup
                    , cmd0 "drop" Drop
                    , cmd0 "select" Select
                    , cmd0 "unreachable" Unreachable
                    , try (LocalGet <$> cmd1 "local.get" intLit)
                    , try (LocalSet <$> cmd1 "local.set" intLit)
                    , try (LocalTee <$> cmd1 "local.tee" intLit)
                    , try (LocalsReserve <$> cmd1 "locals.reserve" intLit)
                    , try
                        ( do
                            void $ string "call"
                            hspace1
                            params <- intLit
                            comma
                            Call params <$> intLit
                        )
                    , cmd0 "return" Return
                    , cmd0 "sp.init" SpInit
                    , cmd0 "halt" Halt
                    ]

cmd0 :: String -> a -> Parser a
cmd0 mnemonic constructor = string mnemonic >> return constructor

cmd1 :: String -> Parser a -> Parser a
cmd1 mnemonic arg = string mnemonic >> hspace1 >> arg

-- | A load\/store, whose static offset may be left off when it is zero:
-- @i32.load@ and @i32.load 0@ assemble to the same thing.
memOp :: String -> (Int -> a) -> Parser a
memOp mnemonic constructor =
    string mnemonic >> (constructor <$> (try (hspace1 >> intLit) <|> pure 0))

-- | An integer immediate -- a branch depth, a local index, one of
-- `call`'s counts, a load\/store offset -- in decimal or @0x@ hex.
-- Fails as a parser rather than handing an empty string to 'read':
-- 'num' is built from 'many', so it matches nothing happily, and an
-- optional immediate (see 'memOp') needs to find that out by
-- backtracking.
intLit :: Parser Int
intLit = do
    digits <- try hexNum <|> num
    maybe (fail $ "expected an integer literal, got " <> show digits) return (readMaybe digits)

-- | Separates `call`'s two operands (paramCount, resultCount).
comma :: Parser ()
comma = hspace >> void (char ',') >> hspace

instance (IsWord w) => DerefMnemonic (Wasm32Isa w) w where
    resolveStructure = resolveScopeTargets

    -- Exactly one constructor carries a label, and @l@ is this type's
    -- last parameter, so the derived 'Functor' already is this
    -- traversal: the longhand version was 44 lines of @X -> X@ around
    -- the one line that does anything.
    derefMnemonic f _offset = fmap (deref' f)

-- | Match every `block`\/`loop`\/`if`\/`else` with its own `end` and
-- write the byte distance between them into the instruction, once, at
-- translate time -- so the interpreter only ever adds it to @pc@, and an
-- unbalanced `end` is a translation error rather than something found
-- when a run-time forward scan falls off the end of memory.
resolveScopeTargets :: forall w. (IsWord w) => [(w, Wasm32Isa w w)] -> Either Text [Wasm32Isa w w]
resolveScopeTargets marked = do
    mapM_ checkImmediate marked
    resolved <- matchScopes marked
    mapM_ checkScopeTarget resolved
    return (map snd resolved)

-- | Walk the stream pairing each scope instruction with its own `end`,
-- and write the byte distance between them into it. Unbalanced nesting
-- is a 'Left' naming the offending address.
matchScopes :: forall w. (IsWord w) => [(w, Wasm32Isa w w)] -> Either Text [(w, Wasm32Isa w w)]
matchScopes marked = do
    patches <- go [] (zip [0 ..] marked)
    let patched = fromList patches :: IntMap Int
        resolve i (addr, instruction) = (addr, maybe instruction (retarget instruction) (patched !? i))
    return (zipWith resolve [0 ..] marked)
    where
        -- \| One entry per scope still waiting for its `end`: the index of
        -- the instruction to patch, its own address, and whether that
        -- instruction wants the `end`'s own address ('AtEnd', for
        -- `block`\/`loop`, which store it as @csEnd@) or the address just
        -- past it ('PastEnd', for the `if`\/`else` that skip over it).
        go open [] = case open of
            [] -> Right []
            (_, addr, _) : _ -> Left $ "block/loop/if at " <> hexAddr 2 addr <> " has no matching end"
        go open ((i, (addr, instruction)) : rest) = case instruction of
            Block{} -> go ((i, fromEnum addr, AtEnd) : open) rest
            Loop{} -> go ((i, fromEnum addr, AtEnd) : open) rest
            If{} -> go ((i, fromEnum addr, PastEnd) : open) rest
            Else{} -> case open of
                -- The `if` resumes at the first instruction of this
                -- `else`'s body; this `else` takes the `if`'s place in the
                -- open list, to be closed by the same `end`.
                (j, ifAddr, PastEnd) : outer ->
                    ((j, fromEnum addr + byteSize instruction - ifAddr) :)
                        <$> go ((i, fromEnum addr, PastEnd) : outer) rest
                _ -> Left $ "else at " <> hexAddr 2 (fromEnum addr) <> " has no matching if"
            End -> case open of
                (j, openAddr, want) : outer ->
                    let target = case want of
                            AtEnd -> fromEnum addr
                            PastEnd -> fromEnum addr + byteSize End
                     in ((j, target - openAddr) :) <$> go outer rest
                [] -> Left $ "end at " <> hexAddr 2 (fromEnum addr) <> " has no matching block/loop/if"
            _ -> go open rest

        retarget instruction d = case instruction of
            Block{} -> Block d
            Loop{} -> Loop d
            If{} -> If d
            Else{} -> Else d
            other -> other

-- | An immediate that doesn't fit the field the instruction encodes it in
-- is a source error, not something to mask off at run time -- see
-- 'immediateBitWidth'.
checkImmediate :: forall w. (IsWord w) => (w, Wasm32Isa w w) -> Either Text ()
checkImmediate (addr, instruction) = case instruction of
    Br d -> fits "br depth" immediateBitWidth d
    BrIf d -> fits "br_if depth" immediateBitWidth d
    LocalGet i -> fits "local.get index" immediateBitWidth i
    LocalSet i -> fits "local.set index" immediateBitWidth i
    LocalTee i -> fits "local.tee index" immediateBitWidth i
    LocalsReserve n -> fits "locals.reserve count" immediateBitWidth n
    Call p r -> do
        fits "call paramCount" immediateBitWidth p
        fits "call resultCount" immediateBitWidth r
    I32Load o -> memOffset o
    I32Store o -> memOffset o
    I32Load8U o -> memOffset o
    I32Load8S o -> memOffset o
    I32Store8 o -> memOffset o
    _ -> Right ()
    where
        fits = fieldFits addr
        memOffset = fits "load/store offset" immediateBitWidth

-- | A body too long for the forward distance its own scope instruction
-- can carry.
checkScopeTarget :: forall w. (IsWord w) => (w, Wasm32Isa w w) -> Either Text ()
checkScopeTarget (addr, instruction) = case instruction of
    Block d -> fits "block body" d
    Loop d -> fits "loop body" d
    If d -> fits "if body" d
    Else d -> fits "else body" d
    _ -> Right ()
    where
        fits what = fieldFits addr what wideImmediateBitWidth

-- | Reject a @width@-bit field's value, naming the field and the
-- instruction's own address.
fieldFits :: (IsWord w) => w -> Text -> Int -> Int -> Either Text ()
fieldFits addr what width value
    | value >= 0 && value < 1 `shiftL` width = Right ()
    | otherwise =
        Left $
            what
                <> " "
                <> show value
                <> " at "
                <> hexAddr 2 (fromEnum addr)
                <> " doesn't fit in "
                <> show width
                <> " bits"

-- | Which address a scope instruction wants out of its own matching
-- `end` -- see 'resolveScopeTargets'.
data ScopeTargetKind = AtEnd | PastEnd

instance ByteSize (Wasm32Isa w l) where
    byteSize I32Const{} = 1 + byteSizeT @Int32
    byteSize Br{} = 1 + immediateBytes
    byteSize BrIf{} = 1 + immediateBytes
    byteSize LocalGet{} = 1 + immediateBytes
    byteSize LocalSet{} = 1 + immediateBytes
    byteSize LocalTee{} = 1 + immediateBytes
    byteSize LocalsReserve{} = 1 + immediateBytes
    byteSize Call{} = 1 + 2 * immediateBytes
    -- One opcode byte, one byte of table length, then one byte per entry
    -- (the default included).
    byteSize I32Load{} = memOpBytes
    byteSize I32Store{} = memOpBytes
    byteSize I32Load8U{} = memOpBytes
    byteSize I32Load8S{} = memOpBytes
    byteSize I32Store8{} = memOpBytes
    -- One opcode byte plus a 'wideImmediateBitWidth'-wide forward distance,
    -- the same shape as `call`'s two trailing counts.
    byteSize If{} = scopeInstructionBytes
    byteSize Else{} = scopeInstructionBytes
    byteSize Loop{} = scopeInstructionBytes
    byteSize Block{} = scopeInstructionBytes
    byteSize _ = 1

scopeInstructionBytes, memOpBytes, immediateBytes, wideImmediateBytes :: Int
scopeInstructionBytes = 1 + wideImmediateBytes
memOpBytes = 1 + immediateBytes
immediateBytes = immediateBitWidth `div` 8
wideImmediateBytes = wideImmediateBitWidth `div` 8

-- | A scope instruction's forward distance to its own matching `end`.
-- Two bytes, so any body that fits in the largest configurable memory
-- can be jumped over.
wideImmediateBitWidth :: Int
wideImmediateBitWidth = 16

-- | Every one-byte instruction immediate: a branch depth, a local index,
-- one of `call`'s counts, a load\/store offset. A 'CallScope' stores the
-- counts at this same width, so what the source can say and what a call
-- record can hold agree. A 0-255 offset covers a struct field or a small
-- array index; further is still reachable with `i32.add`.
immediateBitWidth :: Int
immediateBitWidth = 8

-- | What the parser puts in a scope instruction's target slot before
-- 'resolveStructure' matches it with its own `end`. Out of range on
-- purpose, so an unresolved target can't pass for a real distance.
unresolvedTarget :: Int
unresolvedTarget = -1

data Wasm32St w = Wasm32St
    { pc :: Int
    , sp :: Int
    -- ^ Top of the operand\/locals stack (descending)
    , ctrlBase :: Int
    -- ^ The root both stacks grow from: the operand stack descends from
    -- it, the control stack ascends from it. Set by 'initState' and
    -- relocated by `sp.init` (see 'stackRoot').
    , ctrlSp :: Int
    -- ^ The control stack's frontier: one past the innermost live
    -- control record, ascending from 'ctrlBase' the way 'sp' descends
    -- from the top of memory. Every record is the same width and they
    -- close in strict LIFO order, so this one pointer locates all of
    -- them ('scopeAddrs') and closing one is a bump-pointer retreat.
    , frameBase :: Int
    -- ^ Base address of the *active* function's locals (param 0's
    -- address): carved out of the caller's already-pushed arguments by
    -- `call`, saved in the callee's 'CallScope', restored by `return`.
    , localCount :: Int
    -- ^ How many words at 'frameBase' are locals rather than operand
    -- stack -- the active function's own paramCount, saved and restored
    -- alongside 'frameBase' so the report views know where this frame's
    -- stack really starts.
    , memAccessCount :: Int
    -- ^ How many memory accesses the *last executed* instruction made,
    -- its own fetch included. Reset per step in 'instructionStep';
    -- exposed to reports as @memAccesses@.
    , mem :: IoMem (Wasm32Isa w w) w
    , stopped :: Bool
    , internalError :: Maybe Text
    }
    deriving (Show)

type instance MemOf (Wasm32St w) = IoMem (Wasm32Isa w w) w

instance (IsWord w) => InitState (Wasm32St w) where
    initState pc dump _randomStream =
        Wasm32St
            { pc
            , sp = stackRoot dump
            , ctrlBase = stackRoot dump
            , ctrlSp = stackRoot dump
            , frameBase = stackRoot dump - byteSizeT @w
            , localCount = 0
            , memAccessCount = 0
            , mem = dump
            , stopped = False
            , internalError = Nothing
            }

-- | Where code+data end and the stacks' own region begins: the lower
-- half of memory holds code+data, the upper half the stacks. Also the
-- operand stack's floor -- pushing past it is a reported overflow,
-- though code+data spilling past it in the first place is not checked.
memTop :: IoMem (Wasm32Isa w w) w -> Int
memTop IoMem{mIoCells = Mem{memorySize}} = memorySize `div` 2

-- | The root both stacks grow from, three quarters of the way up the
-- stack's half of memory: the operand stack descends from it toward
-- 'memTop', the control stack ascends from it toward the end of memory.
-- Neither can reach the other, and each has one fixed wall of its own.
--
-- Three quarters rather than half because a program pushes far more
-- operands than it opens scopes -- a `block` or a call is eight bytes,
-- and the deepest nesting in the examples is four. A program wanting
-- the other balance moves the root with `sp.init`.
--
-- A push decrements before writing, so the root itself is never written
-- to, and @_start@'s 'frameBase' sits one word below it -- the relation
-- `call` sets up for a frame with no parameters (see 'localAddr').
stackRoot :: IoMem (Wasm32Isa w w) w -> Int
stackRoot mem@IoMem{mIoCells = Mem{memorySize}} =
    let base = memTop mem in base + 3 * (memorySize - base) `div` 4

setPc :: Int -> State (Wasm32St w) ()
setPc addr = modify $ \st -> st{pc = addr}

nextPc :: Wasm32Isa w w -> State (Wasm32St w) ()
nextPc instruction = do
    Wasm32St{pc} <- get
    setPc (pc + byteSize instruction)

raiseInternalError :: Text -> State (Wasm32St w) ()
raiseInternalError msg = modify $ \st -> st{internalError = Just msg}

-- | Counts every 'getWord'\/'setWord'\/'getByte'\/'setByte'\/
-- 'readInstruction' call -- operand pushes\/pops, control-record and
-- local access, and the instruction's own fetch -- toward
-- 'memAccessCount', reset per step in 'instructionStep'.
countMemAccess :: State (Wasm32St w) ()
countMemAccess = modify $ \st -> st{memAccessCount = memAccessCount st + 1}

getWord :: (IsWord w) => Int -> State (Wasm32St w) w
getWord addr = do
    countMemAccess
    st@Wasm32St{mem} <- get
    case readWord mem addr of
        Right (mem', w) -> put st{mem = mem'} >> return w
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return def

setWord :: (IsWord w) => Int -> w -> State (Wasm32St w) ()
setWord addr w = do
    countMemAccess
    st@Wasm32St{mem} <- get
    case writeWord mem addr w of
        Right mem' -> put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err

getByte :: (IsWord w) => Int -> State (Wasm32St w) Word8
getByte addr = do
    countMemAccess
    st@Wasm32St{mem} <- get
    case readByte mem addr of
        Right (mem', b) -> put st{mem = mem'} >> return b
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return 0

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
checkOperandRoom :: Int -> State (Wasm32St w) () -> State (Wasm32St w) ()
checkOperandRoom newSp act = do
    Wasm32St{mem} <- get
    let wall = memTop mem
    if newSp >= wall
        then act
        else
            raiseInternalError $
                "operand stack overflow: push to "
                    <> hexAddr 2 newSp
                    <> " would reach below the stack region at "
                    <> hexAddr 2 wall

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

-- | The operand stack grows downward, and `sp` names the top word
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

-- | Address of local @i@: a fixed offset from 'frameBase', unaffected by
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
-- open -- derived from 'ctrlSp' rather than tracked alongside it.
ctrlTopOf :: forall w. (IsWord w) => Wasm32St w -> Int
ctrlTopOf = fromMaybe nullAddr . listToMaybe . scopeAddrs @w

-- | Read and decode the control record at @addr@. 'Left' when the words
-- there don't decode as a record at all, which can only mean the control
-- stack has been corrupted -- reported rather than guessed at.
readScopeAt :: forall w. (IsWord w) => Int -> State (Wasm32St w) (Either Text Scope)
readScopeAt addr = do
    let step = byteSizeT @w
    words_ <- mapM (\i -> getWord (addr + i * step)) [0 .. scopeRecordWords - 1]
    return $ first (\err -> "control stack corrupted at " <> hexAddr 2 addr <> ": " <> err) (decodeScope @w words_)

-- | Read up to @n@ records from the innermost outward, each with its own
-- address -- fewer if the stack is shallower, and stopping at the first
-- record that fails to decode.
readScope :: forall w. (IsWord w) => Int -> State (Wasm32St w) (Either Text [(Int, Scope)])
readScope n = do
    st <- get
    go (take n (scopeAddrs st))
    where
        go [] = return $ Right []
        go (addr : rest) =
            readScopeAt addr >>= \case
                Left err -> return $ Left err
                Right scope -> fmap ((addr, scope) :) <$> go rest

-- | Read the innermost open record. 'Nothing' when nothing is open,
-- which some callers treat as "nothing to do" rather than an error.
readTopScope :: forall w. (IsWord w) => State (Wasm32St w) (Either Text (Maybe (Int, Scope)))
readTopScope = fmap listToMaybe <$> readScope 1

-- | Push a scope at the current frontier and advance it. Never touches
-- 'sp' -- the two stacks grow apart from 'ctrlBase'.
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

-- | Find the control record @depth@ levels out from the innermost open
-- one (0 = innermost). Errors at a 'CallScope' rather than reaching
-- through it: a `br` is scoped to its own function's nesting, and
-- leaving a function is `return`'s job. The two failures get different
-- messages -- a reader told only "out of range" tries a different depth
-- when the fix is a `return`.
findBranchTarget :: forall w. (IsWord w) => Int -> State (Wasm32St w) (Either Text Int)
findBranchTarget depth = do
    records <- readScope (depth + 1)
    return $ do
        records' <- records
        case drop depth records' of
            _
                | any (isCallScope . snd) records' -> Left "br/br_if: depth reaches past the enclosing function; use return to leave it"
            (target, _) : _ -> Right target
            [] -> Left $ "br/br_if: depth " <> show depth <> " is out of range, only " <> show (length records') <> " scope(s) open"

isCallScope :: Scope -> Bool
isCallScope CallScope{} = True
isCallScope _ = False

-- | Find the nearest enclosing 'CallScope' -- what `return` closes.
-- Unlike 'findBranchTarget', this reaches *through* any `block`\/`loop`
-- in the way: a `return` inside a loop must still close it.
findCall :: forall w. (IsWord w) => State (Wasm32St w) (Either Text Int)
findCall = do
    st <- get
    go (scopeAddrs st)
    where
        go [] = return $ Left "return without active function frame"
        go (addr : outer) =
            readScopeAt addr >>= \case
                Left err -> return $ Left err
                Right CallScope{} -> return $ Right addr
                Right _ -> go outer

-- | Close the innermost open record (always a LIFO pop). If @keepOpen@
-- (a taken branch back into a loop), just jump to its start. A
-- 'CallScope' is never @keepOpen@; everything else retreats 'ctrlSp'
-- past it and jumps past its `end`.
closeScope :: forall w. (IsWord w) => Bool -> State (Wasm32St w) ()
closeScope keepOpen =
    readTopScope >>= \case
        Left err -> raiseInternalError err
        Right Nothing -> raiseInternalError "closeScope: no open control record"
        Right (Just (_, scope)) -> case scope of
            LoopScope{csStart} | keepOpen -> setPc csStart
            LoopScope{csEnd} -> popScope @w >> setPc (csEnd + byteSize End)
            BlockScope{csEnd} -> popScope @w >> setPc (csEnd + byteSize End)
            CallScope{csCallerFrameBase, csCallerLocalCount, csReturnPc, csResultCount} -> do
                results <- popValues csResultCount
                Wasm32St{frameBase} <- get
                modify $ \st -> st{sp = frameBase + byteSizeT @w}
                mapM_ pushValue results
                popScope @w
                modify $ \st -> st{frameBase = csCallerFrameBase, localCount = csCallerLocalCount}
                setPc csReturnPc

popValues :: forall w. (IsWord w) => Int -> State (Wasm32St w) [w]
popValues n = reverse <$> replicateM n popValue

-- | Close every open record from the innermost down to and including
-- @r@, keeping @r@ open only if @keepOpen@ (branching back into a loop).
-- Anything nested inside @r@ is closed whatever its kind.
unwindTo :: forall w. (IsWord w) => Int -> Bool -> State (Wasm32St w) ()
unwindTo r keepOpen = do
    st <- get
    if ctrlTopOf @w st == r
        then closeScope keepOpen
        else do
            closeScope False
            unwindTo r keepOpen

-- | Branch to the `block`\/`loop` @depth@ levels out (0 = innermost): a
-- `loop` stays open (continue), a `block` closes along with everything
-- nested inside it (break). 'findBranchTarget' already guarantees the
-- target is never a 'CallScope'.
branchTo :: forall w. (IsWord w) => Int -> State (Wasm32St w) ()
branchTo depth = do
    result <- findBranchTarget depth
    case result of
        Left err -> raiseInternalError err
        Right target ->
            readScopeAt target >>= \case
                Left err -> raiseInternalError err
                Right LoopScope{} -> unwindTo target True
                Right _ -> unwindTo target False

instance (IsWord w) => Inspectable (Wasm32St w) where
    type WordOf (Wasm32St w) = w
    type IsaOf (Wasm32St w) = Wasm32Isa w w

    programCounter Wasm32St{pc} = pc
    memoryDump Wasm32St{mem} = mem
    ioStreams Wasm32St{mem = IoMem{mIoStreams}} = mIoStreams
    isHalted Wasm32St{stopped} = stopped
    reprState labels st v
        | Just v' <- defaultView labels st v = v'
    reprState labels st@Wasm32St{mem, memAccessCount} v =
        case T.splitOn ":" v of
            ["stack", f] -> formatValues f (stackWords view)
            ["locals", f] -> formatValues f (localWords view)
            ["layout", f] -> withShow f (renderLayout labels view) Nothing
            ["layout", f, n] -> withFrameLimit n (withShow f (renderLayout labels view))
            ["dump", f] -> withShow f (renderDump labels view) Nothing
            ["dump", f, n] -> withFrameLimit n (withShow f (renderDump labels view))
            ["memAccesses", "dec"] -> show memAccessCount
            ["memAccesses", f] -> unknownFormat f
            [r] -> reprState labels st (r <> ":dec")
            [r, _] -> unknownView r
            _ -> errorView v
        where
            view = stackView st

            formatValues "dec" vs = toText $ intercalate ":" $ map show vs
            formatValues "hex" vs = T.intercalate ":" $ map (toText . word32ToHex) vs
            formatValues f _ = unknownFormat f

            -- Both formats already exist for the word *values* a span
            -- shows; extend the same choice to every address it mentions
            -- rather than leaving addresses permanently decimal.
            -- 'word32ToHex's full 8 digits stays for word values, which
            -- are arbitrary 32-bit data however small the memory is;
            -- addresses get only the digits this memory could need.
            withShow "dec" render = render show show
            withShow "hex" render = render (toText . word32ToHex) (hexAddr (hexAddrWidth (memCapacity mem)))
            withShow f _ = const (unknownFormat f)

            -- \| Parse @layout:hex:N@\/@dump:hex:N@'s trailing @N@ -- how
            -- many of the most recent frames (the live one, then its
            -- nearest callers) to show in full; @N = 1@ means the live
            -- frame only.
            withFrameLimit :: Text -> (Maybe Int -> Text) -> Text
            withFrameLimit n k = maybe (unknownFormat n) (k . Just) (readMaybe (toString n))

-- | Hand the report views what they need: the memory, the pointers
-- bounding the live stacks, and where the open control records are.
stackView :: forall w. (IsWord w) => Wasm32St w -> StackView (Wasm32Isa w w) w
stackView st@Wasm32St{mem, pc, sp, ctrlSp, ctrlBase, frameBase, localCount} =
    StackView
        { svMem = mem
        , svPc = pc
        , svSp = sp
        , svFrameBase = frameBase
        , svLocalCount = localCount
        , svCtrlSp = ctrlSp
        , svCtrlBase = ctrlBase
        , svDataTop = memTop mem
        , svScopeAddrs = scopeAddrs st
        }

instance (IsWord w) => Machine (Wasm32St w) (Wasm32Isa w w) w where
    instructionFetch = do
        st <- get
        case st of
            Wasm32St{stopped = True} -> return $ Left halted
            Wasm32St{internalError = Just err} -> return $ Left err
            Wasm32St{pc, mem} ->
                case readInstruction mem pc of
                    Left err -> return $ Left err
                    Right (mem', instruction) -> do
                        put st{mem = mem'}
                        countMemAccess
                        return $ Right (pc, instruction)

    -- Resets 'memAccessCount' before the fetch, not just before the
    -- execute, so the instruction's own fetch counts toward the total.
    instructionStep = do
        modify $ \st -> st{memAccessCount = 0}
        (pc, instruction) <- either (error . ("internal error: " <>)) id <$> instructionFetch
        instructionExecute pc instruction

    instructionExecute pc instruction =
        case instruction of
            I32Const value -> pushValue value >> nextPc instruction
            I32Add -> binary (+) >> nextPc instruction
            I32Sub -> binary (-) >> nextPc instruction
            I32Mul -> binary (*) >> nextPc instruction
            I32DivS -> signedDiv >> nextPc instruction
            I32RemS -> signedRem >> nextPc instruction
            I32And -> binary (.&.) >> nextPc instruction
            I32Or -> binary (.|.) >> nextPc instruction
            I32Xor -> binary xor >> nextPc instruction
            I32Shl -> binary (\x y -> x `shiftL` (fromEnum y .&. 0x1F)) >> nextPc instruction
            I32ShrS -> binary (\x y -> x `shiftR` (fromEnum y .&. 0x1F)) >> nextPc instruction
            I32ShrU -> unsignedBinary (\x y -> toSign $ x `shiftR` (fromEnum y .&. 0x1F)) >> nextPc instruction
            I32Eq -> compareS (==) >> nextPc instruction
            I32LtS -> compareS (<) >> nextPc instruction
            I32LeS -> compareS (<=) >> nextPc instruction
            I32GtS -> compareS (>) >> nextPc instruction
            I32GeS -> compareS (>=) >> nextPc instruction
            I32LtU -> compareU (<) >> nextPc instruction
            I32LeU -> compareU (<=) >> nextPc instruction
            I32GtU -> compareU (>) >> nextPc instruction
            I32GeU -> compareU (>=) >> nextPc instruction
            I32Load offset -> load offset getWord id
            I32Load8U offset -> load offset getByte fromIntegral
            I32Load8S offset -> load offset getByte (fromIntegral . (fromIntegral :: Word8 -> Int8))
            I32Store offset -> store offset setWord id
            I32Store8 offset -> store offset setByte fromIntegral
            If skip -> do
                condition <- popValue
                if condition /= 0
                    then nextPc instruction
                    else skipForward skip
            Else skip -> skipForward skip
            End -> do
                -- A plain `end` closes a `block`\/`loop` only when it is
                -- that scope's own matching `end`; an `if`'s `end` finds
                -- some enclosing scope on top instead, whose own `end`
                -- is elsewhere. It never closes a call -- `return` does.
                top <- readTopScope
                case top of
                    Left err -> raiseInternalError err
                    Right (Just (_, LoopScope{csEnd})) | csEnd == pc -> popScope @w
                    Right (Just (_, BlockScope{csEnd})) | csEnd == pc -> popScope @w
                    Right _ -> return ()
                nextPc instruction
            Loop toEnd ->
                withScopeEnd toEnd $ \endPc -> do
                    pushScope LoopScope{csStart = pc + byteSize instruction, csEnd = endPc}
                    nextPc instruction
            Block toEnd ->
                withScopeEnd toEnd $ \endPc -> do
                    pushScope BlockScope{csEnd = endPc}
                    nextPc instruction
            Br depth -> branchTo depth
            BrIf depth -> do
                condition <- popValue
                if condition == 0
                    then nextPc instruction
                    else branchTo depth
            Dup -> do
                top <- popValue
                pushValue top
                pushValue top
                nextPc instruction
            Drop -> popValue @w >> nextPc instruction
            Select -> do
                condition <- popValue
                onFalse <- popValue
                onTrue <- popValue
                pushValue (if condition /= 0 then onTrue else onFalse)
                nextPc instruction
            Unreachable -> raiseInternalError "unreachable"
            LocalGet i -> do
                addr <- localAddr i
                getWord addr >>= pushValue
                nextPc instruction
            LocalSet i -> do
                addr <- localAddr i
                popValue >>= setWord addr
                nextPc instruction
            LocalTee i -> do
                addr <- localAddr i
                value <- popValue
                setWord addr value
                pushValue value
                nextPc instruction
            LocalsReserve n -> do
                Wasm32St{sp} <- get
                let step = byteSizeT @w
                    sp' = sp - n * step
                checkOperandRoom sp' $ do
                    -- Zeroed, like real WebAssembly's locals: memory
                    -- starts out randomised by default, so an unwritten
                    -- slot would otherwise read as noise, not 0.
                    forM_ [sp - step, sp - 2 * step .. sp'] $ \addr -> setWord addr 0
                    modify $ \st -> st{sp = sp', localCount = localCount st + n}
                nextPc instruction
            Call paramCount resultCount -> do
                target <- popValue
                Wasm32St{sp, frameBase, localCount} <- get
                let calleeFrameBase = sp + (paramCount - 1) * byteSizeT @w
                pushScope
                    CallScope
                        { csCallerFrameBase = frameBase
                        , csCallerLocalCount = localCount
                        , csReturnPc = pc + byteSize instruction
                        , csResultCount = resultCount
                        }
                modify $ \st -> st{frameBase = calleeFrameBase, localCount = paramCount}
                setPc (fromEnum target)
            Return -> do
                result <- findCall
                case result of
                    Left err -> raiseInternalError err
                    Right r -> unwindTo r False
            SpInit -> do
                addr <- fromEnum <$> popValue
                -- 'frameBase' keeps the same relation to `sp` that
                -- 'initState' sets up: one word below, where the first
                -- push lands (see 'localAddr').
                modify $ \st ->
                    st
                        { sp = addr
                        , ctrlBase = addr
                        , ctrlSp = addr
                        , frameBase = addr - byteSizeT @w
                        }
                nextPc instruction
            Halt -> modify $ \st -> st{stopped = True}
        where
            -- \| A load: pop an address, add this instruction's own static
            -- offset, read the bytes there with @readAt@, and push them
            -- widened to a full value by @extend@.
            load :: forall a. Int -> (Int -> State (Wasm32St w) a) -> (a -> w) -> State (Wasm32St w) ()
            load offset readAt extend = do
                addr <- popValue
                raw <- readAt (fromEnum addr + offset)
                pushValue (extend raw)
                nextPc instruction
            -- \| A store, mirroring @load@: the value is on top, the
            -- address under it, and @narrow@ picks the bytes to write.
            store :: forall a. Int -> (Int -> a -> State (Wasm32St w) ()) -> (w -> a) -> State (Wasm32St w) ()
            store offset writeAt narrow = do
                value <- popValue
                addr <- popValue
                writeAt (fromEnum addr + offset) (narrow value)
                nextPc instruction
            -- \| Resolve a scope instruction's own forward distance, which
            -- 'resolveScopeTargets' already computed at translate time.
            -- Only an instruction that never went through it can be
            -- unresolved, so this is a sanity check, not a code path a
            -- program can reach.
            withScopeEnd d k
                | d < 0 = raiseInternalError "control flow error: unresolved scope target"
                | otherwise = k (pc + d)
            skipForward d = withScopeEnd d setPc
            binary op = do
                y <- popValue
                x <- popValue
                pushValue (x `op` y)
            unsignedBinary op = do
                y <- fromSign <$> popValue
                x <- fromSign <$> popValue
                pushValue (op x y)
            -- \| wasm's @idiv_s@: truncates toward zero ('quot'), not toward
            -- negative infinity ('div'), and traps both on a zero divisor
            -- and on the single signed overflow (@minBound \/ -1@).
            signedDiv = do
                y <- popValue
                x <- popValue
                if y == 0
                    then raiseInternalError "integer divide by zero"
                    else
                        if x == minBound && y == -1
                            then raiseInternalError "integer overflow"
                            else pushValue (x `quot` y)
            -- \| wasm's @irem_s@: traps only on a zero divisor. Unlike
            -- @idiv_s@, @minBound % -1@ is defined (as 0) and must not
            -- trap -- 'rem' already answers 0 there without dividing.
            signedRem = do
                y <- popValue
                x <- popValue
                if y == 0
                    then raiseInternalError "integer divide by zero"
                    else pushValue (x `rem` y)
            compareS op = do
                y <- popValue
                x <- popValue
                pushValue $ if x `op` y then 1 else 0
            compareU op = do
                y <- fromSign <$> popValue
                x <- fromSign <$> popValue
                pushValue $ if x `op` y then 1 else 0
