{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

{- | A small, from-scratch WebAssembly-inspired 32-bit ISA: push a
constant, combine values with arithmetic\/comparison ops, structured
control flow, locals, and function calls -- no closures yet -- using the
same generic translator pipeline as e.g. F32a/Acc32 rather than any
bespoke lowering. A callee has no self-describing header: `call` states
its own paramCount\/resultCount as plain literal operands, the same way
`br`'s depth is a caller-known literal rather than looked up from the
target. A function's only locals are its parameters -- there's no
instruction to declare more -- so anything a function needs beyond its
own params\/return values (a loop counter, say) lives in `.data` instead
(see 'localAddr's haddock).

There's also no separate direct\/indirect call split: a function address
is just an ordinary `i32` value (`i32.const some_function` produces one
the same way any other label reference does), so `call` always pops its
target off the stack rather than embedding it as an operand. A
statically-known call site is simply @i32.const target@ followed by
`call` -- one mechanism, not two, at the cost of that being two
instructions instead of one for the common case, and the target no
longer appearing inline on `call`'s own line in a trace\/dump (it's on
the preceding `i32.const` instead).
-}
module Wrench.Isa.Wasm32 (
    Wasm32Isa (..),
    Wasm32St (..),
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Default (def)
import Data.Text qualified as T
import Numeric (showHex)
import Relude
import Relude.Extra (toPairs)
import Relude.Unsafe qualified as Unsafe
import Text.Megaparsec (choice, try)
import Text.Megaparsec.Char (char, hspace, hspace1, string)
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
    | I32DivU
    | I32RemS
    | I32RemU
    | I32And
    | I32Or
    | I32Xor
    | I32Shl
    | I32ShrS
    | I32ShrU
    | I32Eqz
    | I32Eq
    | I32Ne
    | I32LtS
    | I32LeS
    | I32GtS
    | I32GeS
    | I32LtU
    | I32LeU
    | I32GtU
    | I32GeU
    | -- | Pop an address, push the 4-byte word stored there. No static
      -- offset immediate: a hand-written example that needs
      -- base+offset addressing can just do the add explicitly with
      -- `i32.add` before the load.
      I32Load
    | -- | Pop a value then an address (value on top -- the value being
      -- stored is computed\/pushed last, right before the address it's
      -- going to, same as real WebAssembly's stack order), and write the
      -- value's 4 bytes there.
      I32Store
    | -- | Like 'I32Load', but reads one byte and zero-extends it.
      I32Load8U
    | -- | Like 'I32Load8U', but sign-extends instead.
      I32Load8S
    | -- | Like 'I32Store', but writes only the value's low byte.
      I32Store8
    | -- | No label: `if`\/`else`\/`end` targets are found by scanning
      -- forward from `if` (see 'findIfTargets') rather than stored
      -- anywhere -- no runtime frame, since nothing branches out of a
      -- taken branch early.
      If
    | Else
    | End
    | -- | Marks a re-entry point (a valid `br`\/`br_if` target that jumps
      -- backward, to just after here); on its own, runs the body exactly
      -- once, same as `Block`.
      Loop
    | -- | Marks a valid `br`\/`br_if` target that jumps forward, to just
      -- after its matching `end` -- the thing `loop` doesn't give you, and
      -- what makes a real "break" possible (branching out of a loop
      -- instead of only ever back into it).
      Block
    | -- | Branch to the enclosing `block`\/`loop` @depth@ levels out (0 =
      -- innermost), unconditionally. Reaching a `loop` jumps to its start
      -- and leaves it open (this is how you continue); reaching a `block`
      -- jumps past its `end` and closes it, along with everything nested
      -- between here and there (this is how you break out of more than
      -- one level at once). See 'branchTo'.
      Br Int
    | -- | Same as 'Br', but pops a condition first and only branches if
      -- it's non-zero; otherwise falls through.
      BrIf Int
    | -- | Duplicate the top of the operand stack. Without this (or
      -- locals), a value consumed to check a loop condition is gone --
      -- nothing survives to feed the next iteration, so `loop` can only
      -- ever run a fixed, pre-supplied number of times. `dup` is the
      -- minimal fix: peek the top instead of only ever being able to pop
      -- it.
      Dup
    | -- | Push local @i@'s value.
      LocalGet Int
    | -- | Pop a value into local @i@.
      LocalSet Int
    | -- | Like 'LocalSet', but also pushes the value back -- writes the
      -- local without changing stack depth.
      LocalTee Int
    | -- | Pop the call target (a code address, just an ordinary `i32`
      -- value -- see the module haddock) off the stack, with
      -- @paramCount@ more values already pushed below it (they become
      -- the callee's locals 0..paramCount-1), and expect @resultCount@
      -- values back. There's no separate "direct call" instruction: a
      -- statically-known target is just @i32.const target@ immediately
      -- before this, no different in kind from a target computed at
      -- runtime (a function passed as a parameter, say) -- one
      -- mechanism either way. No callee-side header either: the caller
      -- states both counts itself, so the whole thing resolves without
      -- any bespoke lowering pass, just two literal operands. A
      -- mismatch between what's declared here and what the callee
      -- actually expects\/produces isn't caught -- same "trust the
      -- source" posture as everything else in this file.
      Call Int Int
    | -- | Return from the nearest enclosing 'Call', popping its declared
      -- @resultCount@ values (silently trusting exactly that many are on
      -- top of the operand stack) and discarding the whole callee frame in
      -- one step, closing any `block`\/`loop`\/`if` left open along the
      -- way first (a `return` inside a loop must still unwind it). See
      -- 'collapseControl'.
      Return
    | -- | Pop an address and set both `sp` and `frameBase` to it,
      -- relocating where the one shared stack starts. Only meaningful
      -- before anything has been pushed -- typically the very first
      -- thing `_start` does -- since it discards no state of its own,
      -- it just moves where new pushes land. `sp` becomes exactly the
      -- given address, but a push decrements *before* writing (see
      -- 'pushValue'), so that address itself is never written to -- the
      -- value given here must leave room *below* it (lower addresses)
      -- for the deepest this run's stack will ever get -- without this,
      -- the stack always starts at 'initialSp' (the very top of
      -- memory), which a program with enough `.data` to spill past
      -- 'memTop' has no way to override; nothing checks that the new
      -- address doesn't overlap `.text`\/`.data` either, same "trust
      -- the source" posture as everything else here.
      SpInit
    | Halt
    deriving (Eq, Show)

instance CommentStart (Wasm32Isa w l) where
    commentStart = ";"

instance (IsWord w) => MnemonicParser (Wasm32Isa w (Ref w)) where
    mnemonic =
        hspace *> cmd <* eol' (commentStart @(Wasm32Isa _ _))
        where
            cmd =
                choice
                    [ I32Const <$> cmd1 "i32.const" referenceWithDirective
                    , cmd0 "i32.add" I32Add
                    , cmd0 "i32.sub" I32Sub
                    , cmd0 "i32.mul" I32Mul
                    , cmd0 "i32.div_s" I32DivS
                    , cmd0 "i32.div_u" I32DivU
                    , cmd0 "i32.rem_s" I32RemS
                    , cmd0 "i32.rem_u" I32RemU
                    , cmd0 "i32.and" I32And
                    , cmd0 "i32.or" I32Or
                    , cmd0 "i32.xor" I32Xor
                    , cmd0 "i32.shl" I32Shl
                    , cmd0 "i32.shr_s" I32ShrS
                    , cmd0 "i32.shr_u" I32ShrU
                    , cmd0 "i32.eqz" I32Eqz
                    , cmd0 "i32.eq" I32Eq
                    , cmd0 "i32.ne" I32Ne
                    , cmd0 "i32.lt_s" I32LtS
                    , cmd0 "i32.le_s" I32LeS
                    , cmd0 "i32.gt_s" I32GtS
                    , cmd0 "i32.ge_s" I32GeS
                    , cmd0 "i32.lt_u" I32LtU
                    , cmd0 "i32.le_u" I32LeU
                    , cmd0 "i32.gt_u" I32GtU
                    , cmd0 "i32.ge_u" I32GeU
                    , -- Longer alternative first: "i32.load" is a strict
                      -- prefix of "i32.load8_u"/"i32.load8_s", and this
                      -- `choice` list doesn't backtrack once one of its
                      -- bare-word alternatives has consumed input (see
                      -- "i32.eqz" ahead of "i32.eq" above for the same
                      -- reason) -- tried the other way round, "i32.load"
                      -- would swallow the first 8 characters of
                      -- "i32.load8_u" and then fail at the trailing "8_u"
                      -- instead of falling through to try the longer
                      -- mnemonic.
                      cmd0 "i32.load8_u" I32Load8U
                    , cmd0 "i32.load8_s" I32Load8S
                    , cmd0 "i32.load" I32Load
                    , cmd0 "i32.store8" I32Store8
                    , cmd0 "i32.store" I32Store
                    , cmd0 "if" If
                    , cmd0 "else" Else
                    , cmd0 "end" End
                    , cmd0 "loop" Loop
                    , cmd0 "block" Block
                    , try (Br <$> cmd1 "br" intLit)
                    , try (BrIf <$> cmd1 "br_if" intLit)
                    , cmd0 "dup" Dup
                    , try (LocalGet <$> cmd1 "local.get" intLit)
                    , try (LocalSet <$> cmd1 "local.set" intLit)
                    , try (LocalTee <$> cmd1 "local.tee" intLit)
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

-- | A small non-negative integer literal: a branch's target depth (how
-- many enclosing `block`\/`loop` scopes out to reach, 0 = innermost), a
-- local's index, or one of `call`'s two trailing counts.
intLit :: Parser Int
intLit = Unsafe.read <$> num

-- | Separates `call`'s two operands (paramCount, resultCount).
comma :: Parser ()
comma = hspace >> void (char ',') >> hspace

instance DerefMnemonic (Wasm32Isa w) w where
    derefMnemonic f _offset i = case i of
        I32Const l -> I32Const (deref' f l)
        I32Add -> I32Add
        I32Sub -> I32Sub
        I32Mul -> I32Mul
        I32DivS -> I32DivS
        I32DivU -> I32DivU
        I32RemS -> I32RemS
        I32RemU -> I32RemU
        I32And -> I32And
        I32Or -> I32Or
        I32Xor -> I32Xor
        I32Shl -> I32Shl
        I32ShrS -> I32ShrS
        I32ShrU -> I32ShrU
        I32Eqz -> I32Eqz
        I32Eq -> I32Eq
        I32Ne -> I32Ne
        I32LtS -> I32LtS
        I32LeS -> I32LeS
        I32GtS -> I32GtS
        I32GeS -> I32GeS
        I32LtU -> I32LtU
        I32LeU -> I32LeU
        I32GtU -> I32GtU
        I32GeU -> I32GeU
        I32Load -> I32Load
        I32Store -> I32Store
        I32Load8U -> I32Load8U
        I32Load8S -> I32Load8S
        I32Store8 -> I32Store8
        If -> If
        Else -> Else
        End -> End
        Loop -> Loop
        Block -> Block
        Br d -> Br d
        BrIf d -> BrIf d
        Dup -> Dup
        LocalGet i -> LocalGet i
        LocalSet i -> LocalSet i
        LocalTee i -> LocalTee i
        Call p r -> Call p r
        Return -> Return
        SpInit -> SpInit
        Halt -> Halt

instance ByteSize (Wasm32Isa w l) where
    byteSize I32Const{} = 5
    byteSize Br{} = 2
    byteSize BrIf{} = 2
    byteSize LocalGet{} = 2
    byteSize LocalSet{} = 2
    byteSize LocalTee{} = 2
    byteSize Call{} = 3
    byteSize _ = 1

-- | One control-record's payload, carrying exactly the fields each shape
-- needs rather than a bare tag alongside untyped fields: a mismatch
-- between kind and payload can't be constructed in the first place.
-- 'LoopScope' and
-- 'BlockScope' both carry @csEnd@ -- a plain `end`'s own address, compared
-- against it to tell "this end closes the innermost open scope" apart from
-- "this end closes a plain if" (only the former pops, and only that shared
-- field is needed to decide it -- see the `End` case in
-- 'instructionExecute'). Never itself stored raw: 'pushControl' derives
-- the persisted, untyped 'ControlTag' + fields from it, and
-- 'readControlAt' reconstructs it on the way back -- construction-time
-- only, exactly like @RecordExtra@.
data Scope
    = -- | Reaching this (via `br`\/`br_if`) leaves it open: jump to
      -- @csStart@ (just after `loop`) to run the body again.
      LoopScope {csStart :: Int, csEnd :: Int}
    | -- | Reaching this closes it: jump just past @csEnd@ (see
      -- 'collapseControl'), popping it off the control chain too, since
      -- unlike a loop there's nothing left to continue.
      BlockScope {csEnd :: Int}
    | -- | Pushed by `call`, closed by `return` (never by `br`\/`br_if`\/
      -- `end` -- see 'findControlTarget's guard). Unlike the structured
      -- scopes, closing this one doesn't splice a fixed-width gap out of
      -- the stack: 'csResultCount' values get popped, then 'sp' is
      -- reset straight to one word above the *callee's* live
      -- 'frameBase' -- exactly where 'sp' was before the caller pushed
      -- this call's first argument -- reclaiming the record and every
      -- one of the callee's locals (its params -- the only kind there
      -- are) in one step, correct because 'pushControl' always leaves
      -- this record sitting right below them, never in between.
      -- @csSavedFrameBase@\/@csSavedLocalCount@ are what the *caller*
      -- had, restored on return.
      CallScope {csSavedFrameBase :: Int, csSavedLocalCount :: Int, csReturnPc :: Int, csResultCount :: Int}
    deriving (Show)

-- | The persisted tag for a control record -- the untyped memory word
-- 'pushControl' writes and 'readControlAt' reads to know which
-- 'Scope' shape to reconstruct. Kept separate from 'Scope'
-- itself so the typed, construction-checked shape (above) never has to
-- be the on-disk representation too.
data ControlTag = LoopTag | BlockTag | CallTag
    deriving (Bounded, Enum, Eq, Show)

-- | Width, in words, of a control record tagged @tag@: 1 for a
-- 'BlockScope' (tag, link offset and @csEnd@ all share one word), 2 for
-- a 'LoopScope' or 'CallScope' (both need a second word for their own
-- extra address-shaped fields). Variable instead of one size fits all:
-- chain-walking (@readControlAt@) and closing a record (@spliceOut@'s
-- width argument) both need to know a record's actual footprint, so
-- this has to agree exactly with 'serializeControlRecord's own output
-- length for the same kind.
controlRecordWidth :: ControlTag -> Int
controlRecordWidth BlockTag = 1
controlRecordWidth _ = 2

tagOfScope :: Scope -> ControlTag
tagOfScope LoopScope{} = LoopTag
tagOfScope BlockScope{} = BlockTag
tagOfScope CallScope{} = CallTag

-- | Bit widths shared by every control record's own first word:
-- @Tag:2@ (3 tags fit in 2 bits), then @LinkOffset:14@ (a *byte*
-- distance back to the previous record -- see 'packWord0' -- 14 bits
-- comfortably covers the configured memory even well past the default
-- 'maxMemoryLimit'), leaving exactly 16 bits for whichever pair of
-- smaller fields that kind's own low half holds (a single address for
-- 'BlockScope'\/'LoopScope', or two 8-bit counts for 'CallScope'). Any
-- second word a kind needs packs two more 16-bit address-shaped fields
-- the same way -- see 'serializeControlRecord'. There's no longer a
-- separate raw @link@ word: it's folded into this same header as an
-- offset instead of stored as its own absolute address.
tagBitWidth, linkOffsetBitWidth, addrBitWidth, countBitWidth :: Int
tagBitWidth = 2
linkOffsetBitWidth = 14
addrBitWidth = 16
countBitWidth = 8

bitMask :: Int -> Int
bitMask n = (1 `shiftL` n) - 1

-- | Pack a record's shared header -- its tag and its link offset --
-- together with whichever 16-bit low half is specific to that tag,
-- into one word: @Tag:2, LinkOffset:14, Low:16@, most-significant
-- field first (see "Control records, precisely" in @docs/wasm32.md@).
packWord0 :: ControlTag -> Int -> Int -> Int
packWord0 tag linkOffset low16 =
    (fromEnum tag `shiftL` (linkOffsetBitWidth + addrBitWidth))
        .|. (linkOffset `shiftL` addrBitWidth)
        .|. (low16 .&. bitMask addrBitWidth)

-- | The inverse of 'packWord0': a record's tag, link offset, and low
-- 16-bit half, read back out of its first word.
unpackWord0 :: Int -> (ControlTag, Int, Int)
unpackWord0 word0 =
    ( toEnum ((word0 `shiftR` (linkOffsetBitWidth + addrBitWidth)) .&. bitMask tagBitWidth)
    , (word0 `shiftR` addrBitWidth) .&. bitMask linkOffsetBitWidth
    , word0 .&. bitMask addrBitWidth
    )

-- | A record's own header\/second word packs its tag into the top
-- bits (see 'packWord0'), so the raw bit pattern often doesn't fit
-- @w@'s *signed* range even though it's a perfectly good 32-bit
-- word -- plain 'toEnum'\/'fromEnum' would reject or corrupt it.
-- These two go through 'FromSign' instead, treating the word as
-- what it actually is here: an unsigned bit pattern, not a number.
wordFromBits :: forall w. (IsWord w) => Int -> w
wordFromBits = toSign . fromIntegral

bitsFromWord :: forall w. (IsWord w) => w -> Int
bitsFromWord = fromIntegral . fromSign

-- | Serialize a control record -- @link@ plus the 'Scope' being pushed
-- at @recordAddr@ -- to exactly 'controlRecordWidth'-many words for
-- that kind's own tag. @link@ is never stored as its own absolute
-- address: it's folded into the first word as @LinkOffset = link -
-- recordAddr@ (unsigned, since a record only ever points backward to a
-- strictly higher address -- the stack descends, so whatever was pushed
-- earlier sits above whatever's pushed now), with @0@ -- otherwise
-- unreachable, since the previous record always has some nonzero width
-- of its own -- standing in for 'nullAddr' (no enclosing record). The
-- only two representations that matter outside this pair of functions
-- are this one ([w], what's actually in memory) and 'Scope' (the
-- algebraic type code elsewhere works with).
serializeControlRecord :: forall w. (IsWord w) => Int -> Int -> Scope -> [w]
serializeControlRecord recordAddr link scope =
    let linkOffset = if link == nullAddr then 0 else link - recordAddr
     in case scope of
            BlockScope{csEnd} ->
                [wordFromBits (packWord0 BlockTag linkOffset csEnd)]
            LoopScope{csStart, csEnd} ->
                [ wordFromBits (packWord0 LoopTag linkOffset csStart)
                , wordFromBits ((csEnd - csStart) `shiftL` addrBitWidth)
                ]
            CallScope{csSavedFrameBase, csSavedLocalCount, csReturnPc, csResultCount} ->
                [ wordFromBits (packWord0 CallTag linkOffset ((csSavedLocalCount `shiftL` countBitWidth) .|. csResultCount))
                , wordFromBits ((csReturnPc `shiftL` addrBitWidth) .|. csSavedFrameBase)
                ]

-- | The inverse of 'serializeControlRecord'. The caller reads exactly
-- 'controlRecordWidth'-many words for the tag found in @recordAddr@'s
-- own first word (see 'readControlAt') before calling this, so a list
-- whose length doesn't match its own decoded tag means something
-- upstream is already broken.
deserializeControlRecord :: forall w. (IsWord w) => Int -> [w] -> (Scope, Int)
deserializeControlRecord recordAddr (word0 : rest) =
    (,link) $ case (tag, rest) of
        (BlockTag, []) -> BlockScope{csEnd = low16}
        (LoopTag, [word1]) ->
            let csStart = low16
             in LoopScope{csStart, csEnd = csStart + (bitsFromWord word1 `shiftR` addrBitWidth)}
        (CallTag, [word1]) ->
            CallScope
                { csSavedFrameBase = bitsFromWord word1 .&. bitMask addrBitWidth
                , csSavedLocalCount = low16 `shiftR` countBitWidth
                , csReturnPc = bitsFromWord word1 `shiftR` addrBitWidth
                , csResultCount = low16 .&. bitMask countBitWidth
                }
        (t, ws) -> error $ "deserializeControlRecord: " <> show (length ws) <> " extra words doesn't match tag " <> show t
    where
        (tag, linkOffset, low16) = unpackWord0 (bitsFromWord word0)
        link = if linkOffset == 0 then nullAddr else recordAddr + linkOffset
deserializeControlRecord _ ws =
    error $ "deserializeControlRecord: expected at least a header word, got " <> show (length ws) <> " words"

-- | Sentinel for "no enclosing control record" ('ctrlTop', or a record's
-- own `link`), and for "no active function frame" when 'findCall' walks
-- past the bottom of the chain without finding a 'CallScope'.
nullAddr :: Int
nullAddr = -1

data Wasm32St w = Wasm32St
    { pc :: Int
    , sp :: Int
    -- ^ Top of the *one* stack: locals, operand values, and control
    -- records all live here, interleaved in whatever order they were
    -- pushed (a `loop`\/`block`\/`call` pushes its own record right on top
    -- of the operand values that happen to be there already) -- no
    -- separate control-stack region, no separate call stack, no second
    -- stack pointer.
    , ctrlTop :: Int
    -- ^ Address of the innermost open `block`\/`loop`\/`call`'s control
    -- record, or 'nullAddr'. Since records are scattered through the
    -- shared stack at unpredictable intervals (arbitrarily many operand
    -- pushes can sit between two successive records), finding the record
    -- @depth@ levels out -- or the nearest 'CallScope' -- can't be done by
    -- arithmetic alone: each record stores `link`, the previous record's
    -- own address, and 'readControlAt'\/'branchTo'\/'findCall' walk that
    -- chain.
    , frameBase :: Int
    -- ^ Base address of the *active* function's locals (param 0's
    -- address). Starts at 'initialSp' -- so a program that never calls
    -- anything behaves exactly as before functions existed -- and is
    -- carved out of the caller's already-pushed arguments by `call`,
    -- saved in the callee's own 'CallScope' record, and restored by
    -- `return`.
    , localCount :: Int
    -- ^ How many words at 'frameBase' are locals (i.e. params -- the
    -- active function's own paramCount) rather than operand-stack
    -- content -- set once by `call` and saved\/restored across nested
    -- calls exactly like 'frameBase' -- so report views know where the
    -- *stack* actually starts for whichever frame is currently active.
    , mem :: IoMem (Wasm32Isa w w) w
    , stopped :: Bool
    , internalError :: Maybe Text
    }
    deriving (Show)

type instance MemOf (Wasm32St w) = IoMem (Wasm32Isa w w) w

instance InitState (Wasm32St w) where
    initState pc dump _randomStream =
        Wasm32St
            { pc
            , sp = initialSp dump
            , ctrlTop = nullAddr
            , frameBase = initialSp dump
            , localCount = 0
            , mem = dump
            , stopped = False
            , internalError = Nothing
            }

-- | Where code+data end and the stack's own region begins: the lower
-- half of the configured memory holds code+data, the upper half is the
-- stack's -- used only to mark that boundary in the @dump@ report view
-- (see 'renderDump'), not by the stack itself, which starts at
-- 'initialSp' and is otherwise free to use as much of its half as it
-- needs.
memTop :: IoMem (Wasm32Isa w w) w -> Int
memTop IoMem{mIoCells = Mem{memorySize}} = memorySize `div` 2

-- | Where the stack starts: the very top of memory. The classic
-- descending-stack convention -- a push decrements 'sp' *then* writes
-- (see 'pushValue') -- so this is one past the first word actually
-- written, exactly like a real machine's initial stack pointer.
initialSp :: IoMem (Wasm32Isa w w) w -> Int
initialSp IoMem{mIoCells = Mem{memorySize}} = memorySize

setPc :: Int -> State (Wasm32St w) ()
setPc addr = modify $ \st -> st{pc = addr}

nextPc :: Wasm32Isa w w -> State (Wasm32St w) ()
nextPc instruction = do
    Wasm32St{pc} <- get
    setPc (pc + byteSize instruction)

raiseInternalError :: Text -> State (Wasm32St w) ()
raiseInternalError msg = modify $ \st -> st{internalError = Just msg}

getWord :: (IsWord w) => Int -> State (Wasm32St w) w
getWord addr = do
    st@Wasm32St{mem} <- get
    case readWord mem addr of
        Right (mem', w) -> put st{mem = mem'} >> return w
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return def

setWord :: (IsWord w) => Int -> w -> State (Wasm32St w) ()
setWord addr w = do
    st@Wasm32St{mem} <- get
    case writeWord mem addr w of
        Right mem' -> put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err

getByte :: (IsWord w) => Int -> State (Wasm32St w) Word8
getByte addr = do
    st@Wasm32St{mem} <- get
    case readByte mem addr of
        Right (mem', b) -> put st{mem = mem'} >> return b
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return 0

setByte :: (IsWord w) => Int -> Word8 -> State (Wasm32St w) ()
setByte addr b = do
    st@Wasm32St{mem} <- get
    case writeByte mem addr b of
        Right mem' -> put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err

-- | The stack grows downward: `sp` is the address of the top (most
-- recently pushed) word, not the next free slot above it. A push
-- decrements first, then writes at the new `sp` -- the classic
-- descending-stack convention (matching real hardware stacks), chosen
-- over the alternative (write-then-decrement, leaving `sp` pointing at
-- the next free slot) so `sp` always names an actual live value.
pushValue :: forall w. (IsWord w) => w -> State (Wasm32St w) ()
pushValue value = do
    Wasm32St{sp} <- get
    let sp' = sp - byteSizeT @w
    modify $ \st -> st{sp = sp'}
    setWord sp' value

-- | No overflow guard: popping past the top of the stack region reads
-- whatever is physically above wherever the stack happens to start
-- (uninitialized memory, or straight into a memory error once
-- addresses run out) -- a program error this ISA doesn't validate
-- against ahead of time.
popValue :: forall w. (IsWord w) => State (Wasm32St w) w
popValue = do
    Wasm32St{sp} <- get
    value <- getWord sp
    modify $ \st -> st{sp = sp + byteSizeT @w}
    return value

-- | Address of local @i@ (one of the active function's params): a fixed
-- offset from 'frameBase', unaffected by `sp`\/`ctrlTop` -- the whole
-- point, since a value merely sitting on the stack isn't (see the module
-- haddock). Locals count *down* from 'frameBase' (param 0 is the
-- highest address, matching the order they were pushed in before `call`
-- ran -- the first-pushed value ends up highest in a descending stack).
localAddr :: forall w. (IsWord w) => Int -> State (Wasm32St w) Int
localAddr i = do
    Wasm32St{frameBase} <- get
    return $ frameBase - i * byteSizeT @w

-- | Remove the @widthWords@-word record at @r@, shifting everything
-- below it (down to @bottom@, the current `sp`, more-recently-pushed
-- since the stack descends) up to close the gap, and growing `sp` to
-- match -- closes a block\/loop record without disturbing whatever its
-- body pushed below it. Processes from the address closest to @r@
-- first: each destination is exactly the previous iteration's source,
-- so shifting high-to-low never overwrites a word before it's read.
spliceOut :: forall w. (IsWord w) => Int -> Int -> Int -> State (Wasm32St w) ()
spliceOut r widthWords bottom = do
    let step = byteSizeT @w
        width = widthWords * step
    forM_ (reverse [0, step .. r - bottom - step]) $ \i -> getWord (bottom + i) >>= setWord (bottom + width + i)
    modify $ \st -> st{sp = bottom + width}

-- | Push a control record: reserve its words just below the current
-- 'sp' (right below whatever operand values are already there,
-- descending like any other push), link it to the previously innermost
-- record, and make it the new innermost one. 'ctrlTop' names a record
-- by its lowest address (its first/tag word), so the whole block is
-- reserved upfront and its words laid out ascending within it -- the
-- same layout 'readControlAt' expects.
pushControl :: forall w. (IsWord w) => Scope -> State (Wasm32St w) ()
pushControl scope = do
    Wasm32St{sp, ctrlTop} <- get
    let step = byteSizeT @w
        width = controlRecordWidth (tagOfScope scope) * step
        base = sp - width
        ws = serializeControlRecord @w base ctrlTop scope
    forM_ (zip [0 ..] ws) $ \(i, word) -> setWord (base + i * step) word
    modify $ \st -> st{ctrlTop = base, sp = base}

-- | Read the control record based at @addr@ (not necessarily 'ctrlTop'
-- itself -- 'branchTo'\/'unwindTo'\/'findCall' walk the chain via `link`
-- without disturbing anything), returning its payload and its `link`.
-- Two-phase, since a record's own width (how many more words to read)
-- depends on the tag inside its own first word -- see
-- 'controlRecordWidth'.
readControlAt :: forall w. (IsWord w) => Int -> State (Wasm32St w) (Scope, Int)
readControlAt addr = do
    let step = byteSizeT @w
    word0 <- getWord addr
    let (tag, _, _) = unpackWord0 (bitsFromWord word0)
        width = controlRecordWidth tag
    restWords <- mapM (\i -> getWord (addr + i * step)) [1 .. width - 1]
    return $ deserializeControlRecord @w addr (word0 : restWords)

-- | One active call's own slice of the shared stack, as address ranges
-- only -- no memory reads here, so this stays usable from a pure
-- 'reprState' via 'readWord' directly, rather than forcing every caller
-- through the control-record 'Wasm32St' machinery just to print a value.
data FrameLayout = FrameLayout
    { flPc :: Int
    -- ^ Where this frame is: the live 'pc' for the innermost\/current
    -- frame, or the paused return address (a shallower 'CallScope's own
    -- 'csReturnPc') for every frame below it.
    , flFrameBase :: Int
    , flLocalCount :: Int
    , flScanLower :: Int
    -- ^ Inclusive lower bound of this frame's own visible region --
    -- 'sp' for the innermost frame, or one word above the next (deeper)
    -- frame's own 'flFrameBase' otherwise, since everything at or below
    -- that address belongs to the call this frame made, not to this
    -- frame itself (the stack descends, so a deeper call's own region
    -- sits at lower addresses).
    , flControls :: [(Int, Int, Scope, Int)]
    -- ^ This frame's own open records -- any 'block'\/'loop' it has
    -- open, plus (last) the 'CallScope' that entered it, if any --
    -- address-descending (push order: oldest, highest address, first),
    -- each as @(start, end, scope, link)@, 'link' kept alongside purely
    -- so the layout view can show where each record points, not because
    -- anything downstream still needs it.
    }

-- | Split the one shared stack (see 'sp'\/'ctrlTop'\/'frameBase'\/
-- 'localCount's own haddocks for why there's only one) into per-call
-- frames, innermost\/live one first. A frame's own control chain can't
-- be told apart from its neighbours' by address arithmetic alone --
-- 'link' is the only thing connecting them -- so this walks it exactly
-- like 'findCall'\/'branchTo' do, just without ever unwinding anything.
walkFrames :: forall w. (IsWord w) => Wasm32St w -> [FrameLayout]
walkFrames st@Wasm32St{sp, ctrlTop, frameBase, localCount, pc} = go frameBase localCount pc sp ctrlTop
    where
        step = byteSizeT @w

        go fb lc p scanLower addr =
            let (controls, next) = collectControls addr
                frame =
                    FrameLayout
                        { flPc = p
                        , flFrameBase = fb
                        , flLocalCount = lc
                        , flScanLower = scanLower
                        , flControls = sortOn (Down . \(s, _, _, _) -> s) controls
                        }
             in frame : case next of
                    Nothing -> []
                    Just (fb', lc', p', link) -> go fb' lc' p' (fb + step) link

        -- \| Walk from @addr@, collecting every open `block`\/`loop` as
        -- this frame's own, stopping at (and including) the first
        -- 'CallScope' -- the record that entered this very frame, whose
        -- fields are exactly what the next, shallower frame needs.
        -- 'Nothing' only once the chain truly ends: the outermost frame,
        -- which was never itself entered by a `call`.
        collectControls addr
            | addr == nullAddr = ([], Nothing)
            | otherwise =
                let (scope, link) = evalState (readControlAt @w addr) st
                    span_ = (addr, addr + controlRecordWidth (tagOfScope scope) * step, scope, link)
                 in case scope of
                        CallScope{csSavedFrameBase, csSavedLocalCount, csReturnPc} ->
                            ([span_], Just (csSavedFrameBase, csSavedLocalCount, csReturnPc, link))
                        _ ->
                            let (rest, result) = collectControls link
                             in (span_ : rest, result)

-- | Find the `if` at @start@'s branch targets by scanning forward,
-- tracking nested `if`\/`loop`\/`block` scopes so a nested `else`\/`end`
-- doesn't get mistaken for this one's. Returns the matching `else`'s
-- address (if there is one) and the matching `end`'s address.
findIfTargets :: (IsWord w) => IoMem (Wasm32Isa w w) w -> Int -> Either Text (Maybe Int, Int)
findIfTargets memory start = go start (0 :: Int) Nothing
    where
        go addr depth elsePc = do
            (_, instruction) <- readInstruction memory addr
            let next = addr + byteSize instruction
            case instruction of
                If -> go next (depth + 1) elsePc
                Loop -> go next (depth + 1) elsePc
                Block -> go next (depth + 1) elsePc
                Else
                    | depth == 0 -> go next depth (Just addr)
                    | otherwise -> go next depth elsePc
                End
                    | depth == 0 -> Right (elsePc, addr)
                    | otherwise -> go next (depth - 1) elsePc
                _ -> go next depth elsePc

-- | Find the `end` matching the scope (an `if`'s `else`, or a `loop`\/
-- `block`) starting right after @start@ -- shared by 'Else' (skipping the
-- else-branch body after a taken then-branch) and 'Loop'\/'Block' (finding
-- their own end up front, to remember in 'controlStack').
findEndPc :: (IsWord w) => IoMem (Wasm32Isa w w) w -> Int -> Either Text Int
findEndPc memory start = go start (0 :: Int)
    where
        go addr depth = do
            (_, instruction) <- readInstruction memory addr
            let next = addr + byteSize instruction
            case instruction of
                If -> go next (depth + 1)
                Loop -> go next (depth + 1)
                Block -> go next (depth + 1)
                End
                    | depth == 0 -> Right addr
                    | otherwise -> go next (depth - 1)
                _ -> go next depth

-- | Find the control record @depth@ levels out from `ctrlTop` (0 =
-- innermost) by walking `link`. Stops with an error at a 'CallScope'
-- rather than walking through it -- a `br`\/`br_if` is scoped to its own
-- function's `block`\/`loop` nesting and must never reach into (or past)
-- the caller's, matching how a `return` (not `br`) is the only thing
-- that closes a call.
findControlTarget :: forall w. (IsWord w) => Int -> State (Wasm32St w) (Either Text Int)
findControlTarget depth = do
    Wasm32St{ctrlTop} <- get
    go depth ctrlTop
    where
        go _ addr | addr == nullAddr = return $ Left "br/br_if: depth out of range"
        go n addr = do
            (scope, link) <- readControlAt addr
            case scope of
                CallScope{} -> return $ Left "br/br_if: depth out of range"
                _
                    | n <= (0 :: Int) -> return $ Right addr
                    | otherwise -> go (n - 1) link

-- | Walk the chain from `ctrlTop` for the nearest enclosing 'CallScope' --
-- what `return` closes. Unlike 'findControlTarget', this is meant to walk
-- *through* any `block`\/`loop`\/`if` in the way (a `return` inside a loop
-- must still close it) and only stops -- successfully -- at a call.
findCall :: forall w. (IsWord w) => State (Wasm32St w) (Either Text Int)
findCall = do
    Wasm32St{ctrlTop} <- get
    go ctrlTop
    where
        go addr
            | addr == nullAddr = return $ Left "return without active function frame"
            | otherwise = do
                (scope, link) <- readControlAt addr
                case scope of
                    CallScope{} -> return $ Right addr
                    _ -> go link

-- | Collapse the record at @r@: if @keepOpen@ (a taken branch back into
-- a loop), just jump to its start -- the record and everything its body
-- has pushed below it (more recent, since the stack descends) stay
-- exactly as they are, since the next iteration needs them. A
-- 'CallScope' is never @keepOpen@ (only `return` closes one, and it
-- always exits); everything else splices the record's own words out of
-- the stack (preserving everything below them), unlinks it, and jumps
-- past its `end`.
collapseControl :: forall w. (IsWord w) => Int -> Bool -> State (Wasm32St w) ()
collapseControl r keepOpen = do
    (scope, link) <- readControlAt r
    case scope of
        LoopScope{csStart} | keepOpen -> setPc csStart
        CallScope{csSavedFrameBase, csSavedLocalCount, csReturnPc, csResultCount} -> do
            -- Read every field before touching the stack below: popping
            -- results and truncating overwrite this record's own bytes.
            results <- popValues csResultCount
            Wasm32St{frameBase} <- get
            modify $ \st -> st{sp = frameBase + byteSizeT @w}
            mapM_ pushValue results
            modify $ \st -> st{frameBase = csSavedFrameBase, localCount = csSavedLocalCount, ctrlTop = link}
            setPc csReturnPc
        _ -> do
            Wasm32St{sp} <- get
            spliceOut r (controlRecordWidth (tagOfScope scope)) sp
            modify $ \st -> st{ctrlTop = link}
            setPc (csEnd scope + byteSize End)

popValues :: forall w. (IsWord w) => Int -> State (Wasm32St w) [w]
popValues n = reverse <$> replicateM n popValue

-- | Collapse every open record from `ctrlTop` down to and including @r@,
-- keeping @r@ itself open only if @keepOpen@ (branching back into a
-- loop). Anything strictly above @r@ is always fully closed along the
-- way, regardless of its own kind -- branching past a scope exits it
-- unconditionally.
unwindTo :: forall w. (IsWord w) => Int -> Bool -> State (Wasm32St w) ()
unwindTo r keepOpen = do
    Wasm32St{ctrlTop} <- get
    if ctrlTop == r
        then collapseControl r keepOpen
        else do
            collapseControl ctrlTop False
            unwindTo r keepOpen

-- | Branch to the `block`\/`loop` @depth@ levels out (0 = innermost).
-- Reaching a `loop` leaves it open (this is how you continue); reaching
-- a `block` closes it, along with everything nested between here and
-- there (this is how you break out of more than one level at once).
-- 'findControlTarget' already guarantees @target@ is never a 'CallScope'.
branchTo :: forall w. (IsWord w) => Int -> State (Wasm32St w) ()
branchTo depth = do
    result <- findControlTarget depth
    case result of
        Left err -> raiseInternalError err
        Right target -> do
            (scope, _) <- readControlAt target
            let keepOpen = case scope of
                    LoopScope{} -> True
                    _ -> False
            unwindTo target keepOpen

instance (IsWord w) => Inspectable (Wasm32St w) where
    type WordOf (Wasm32St w) = w
    type IsaOf (Wasm32St w) = Wasm32Isa w w

    programCounter Wasm32St{pc} = pc
    memoryDump Wasm32St{mem} = mem
    ioStreams Wasm32St{mem = IoMem{mIoStreams}} = mIoStreams
    isHalted Wasm32St{stopped} = stopped
    reprState labels st v
        | Just v' <- defaultView labels st v = v'
    reprState labels st@Wasm32St{mem, sp, frameBase, localCount} v =
        case T.splitOn ":" v of
            ["stack", f] -> formatValues f values
            ["locals", f] -> formatValues f localValues
            ["layout", f] -> formatLayout f
            ["dump", f] -> formatDump f
            [r] -> reprState labels st (r <> ":dec")
            [r, _] -> unknownView r
            _ -> errorView v
        where
            step = byteSizeT @w
            -- Where the *active* frame's locals end and its own operand
            -- stack begins -- everything from here down to `sp` is
            -- "stack" for report-view purposes, which, same as before
            -- functions existed, includes this frame's own control
            -- records (and any calls it in turn made) as raw words, not
            -- just plain operand values -- this view has never
            -- distinguished the two (see "layout" below for a view that
            -- does). Locals count down from 'frameBase' (see 'localAddr'),
            -- so this boundary is below it, not above.
            frameStackBase = frameBase - localCount * step
            -- Top first, matching the old list's head-is-top convention:
            -- `sp` itself already is the top under the descending-stack
            -- convention (see 'pushValue'), so no offset is needed here.
            values = mapMaybe (\a -> eitherToMaybe (readWord mem a) <&> snd) [sp, sp + step .. frameStackBase - step]
            localValues =
                mapMaybe
                    (\a -> eitherToMaybe (readWord mem a) <&> snd)
                    [frameBase, frameBase - step .. frameStackBase + step]

            formatValues "dec" vs = toText $ intercalate ":" $ map show vs
            formatValues "hex" vs = T.intercalate ":" $ map (toText . word32ToHex) vs
            formatValues f _ = unknownFormat f

            eitherToMaybe = either (const Nothing) Just

            wordAt showWord a = maybe "?" (showWord . snd) (eitherToMaybe (readWord mem a))

            offset2label :: HashMap Int Text
            offset2label = fromList $ map (\(l, a) -> (fromEnum a, l)) $ toPairs labels

            -- \| The innermost label at or before @addr@ -- "which
            -- function" a frame paused (or currently sitting) at @addr@
            -- is in, on the assumption that every function starts right
            -- at its own label and nothing straddles two functions'
            -- worth of code.
            funcNameAt addr =
                fromMaybe "?"
                    $ viaNonEmpty last
                    $ map snd
                    $ sortOn fst
                    $ filter ((<= addr) . fst)
                    $ toPairs offset2label

            -- Both formats already exist for the word *values* a span
            -- shows; extend the same choice to every address a span
            -- mentions (its own mem[a..b] range, a frame's pc, a control
            -- record's return\/start\/end\/savedFrameBase) rather than
            -- leaving addresses permanently decimal regardless of which
            -- format was asked for. Addresses get only as many hex
            -- digits as this run's own configured memory could ever
            -- need (see 'hexAddrWidth') -- 'word32ToHex's full 8 digits
            -- stays for word *values*, which are arbitrary 32-bit data
            -- regardless of how small the memory is.
            formatLayout "dec" = renderLayout show show
            formatLayout "hex" = renderLayout (toText . word32ToHex) (hexAddr (hexAddrWidth (memCapacity mem)))
            formatLayout f = unknownFormat f

            -- \| The whole configured memory as three contiguous chunks,
            -- nothing left unaccounted for, address-ascending overall:
            -- `.text`\/`.data` (address 0 up to 'memTop') dumped exactly
            -- the way the static translation dump does -- 'prettyDump'
            -- itself, reused directly, not reimplemented -- then the
            -- still-unused tail (`memTop` up to `sp`, the stack hasn't
            -- reached down this far yet), then the live stack itself
            -- (`sp` up to the top of memory, decoded) -- the one chunk
            -- that reads push-order (highest\/oldest address first)
            -- internally rather than ascending, same as 'layout'.
            formatDump "dec" = renderDump show show
            formatDump "hex" = renderDump (toText . word32ToHex) (hexAddr (hexAddrWidth (memCapacity mem)))
            formatDump f = unknownFormat f

            renderDump :: (w -> Text) -> (Int -> Text) -> Text
            renderDump showWord showAddr =
                T.intercalate "\n" $
                    filter
                        (not . T.null)
                        [ dumpRange 0 (memTop mem)
                        , dumpRange (memTop mem) sp
                        , renderLayout showWord showAddr
                        ]
                where
                    dumpRange lo hi = prettyDump labels (fromList (sliceMem [lo .. hi - 1] (dumpCells mem)))

            -- \| One line per contiguous span of a frame's own visible
            -- range, push-order (highest\/oldest address first) within
            -- each frame and across frames from the outermost down to
            -- the innermost\/live one -- a plain memory dump (same shape
            -- as the static one 'prettyDump' produces for
            -- `.text`\/`.data`), annotated with what each span actually
            -- is: a frame's locals, one of its own open control records
            -- (decoded, not raw words), or a run of genuine operand
            -- values.
            renderLayout :: (w -> Text) -> (Int -> Text) -> Text
            renderLayout showWord showAddr =
                T.intercalate "\n" $ concatMap (renderFrame showWord showAddr) tagged
                where
                    frames = reverse (walkFrames st)
                    liveIndex = length frames - 1
                    tagged = zipWith (\i frame -> (i, i == liveIndex, frame)) [0 :: Int ..] frames

            -- \| A suspended (caller) frame gets a @#N funcName (pc=...)@
            -- header -- @funcName@\/@pc@ genuinely describe something in
            -- memory, since that @pc@ is the very 'csReturnPc' sitting in
            -- the 'CallScope' that suspended it (see 'describeControl').
            -- The live\/innermost frame gets none: neither its @pc@ (held
            -- only in 'Wasm32St' itself, see 'pc') nor the function name
            -- derived from it are backed by anything in memory, and
            -- 'reprState' already surfaces the live @pc@ separately via
            -- @{pc}@\/@{pc:label}@ -- repeating it here would just be the
            -- same non-memory value shown twice. Its own spans (already
            -- self-labeled \"locals\"\/\"(operands)\") print with no
            -- wrapping header at all.
            renderFrame showWord showAddr (i, isLive, FrameLayout{flPc, flFrameBase, flLocalCount, flScanLower, flControls}) =
                header <> localsLines <> segmentLines
                where
                    header
                        | isLive = []
                        | otherwise = ["#" <> show i <> " " <> funcNameAt flPc <> " (pc=" <> showAddr flPc <> ")"]
                    -- Exclusive upper bound of the operand region below
                    -- the locals -- *not* itself a local's address (local
                    -- 0 is at 'flFrameBase' itself, one word higher; see
                    -- 'localAddr').
                    opTop = flFrameBase - flLocalCount * step
                    localsLines
                        | flLocalCount == 0 = []
                        | otherwise = indexedSpan (opTop + step) (flFrameBase + step) "locals"
                    segmentLines = go opTop flControls
                    -- The trailing gap (nearest `sp`, what's on top of the
                    -- operand stack *right now*) says so explicitly when
                    -- there's nothing there -- silence read as "did this
                    -- even get checked?" often enough to be worth a line.
                    -- An interior gap between two of this frame's own
                    -- control records being empty is unremarkable by
                    -- comparison (blocks/loops with nothing pushed between
                    -- them are the common case, not a surprise), so it
                    -- stays silent like before.
                    go hi [] = trailingSpan flScanLower hi
                    go hi ((s, e, scope, link) : rest) = valueSpan e hi <> controlSpan s e scope link <> go s rest
                    valueSpan lo hi
                        | lo >= hi = []
                        | otherwise = indexedSpan lo hi "(operands)"
                    trailingSpan lo hi
                        | lo >= hi = ["  (operands): empty"]
                        | otherwise = indexedSpan lo hi "(operands)"
                    -- Just one line: the raw words a control record's
                    -- own bits pack into aren't independently readable
                    -- the way a local's or an operand's own word is (see
                    -- 'packMeta') -- 'describeControl's rendering
                    -- already says everything they hold, so showing both
                    -- would just be the same information twice, once
                    -- decoded and once not.
                    controlSpan lo hi scope link =
                        ["  mem[" <> showAddr lo <> ".." <> showAddr (hi - 1) <> "]: " <> describeControl showAddr lo link scope]
                    -- \| This span's address range, on one line, followed
                    -- by one indented "index: value" line per word,
                    -- highest\/oldest address (index 0) first -- for a
                    -- frame's locals, this also happens to be exactly
                    -- local\/param index order (see 'localAddr'), and for
                    -- plain operand values it's push order, where every
                    -- word is its own independent thing worth a line,
                    -- unlike a control record's fields (see
                    -- 'controlSpan'\/'describeControl').
                    indexedSpan lo hi tag =
                        header_
                            : zipWith
                                (\idx a -> "    " <> show (idx :: Int) <> ": " <> wordAt showWord a)
                                [0 ..]
                                [hi - step, hi - 2 * step .. lo]
                        where
                            header_ = "  mem[" <> showAddr lo <> ".." <> showAddr (hi - 1) <> "]: " <> tag

            -- \| A Haskell-record-literal rendering of a control
            -- record's fields -- close to how 'Scope' itself reads
            -- in source, rather than a bespoke key=value format -- with
            -- addresses (@csStart@, @csEnd@, @csReturnPc@,
            -- @csSavedFrameBase@) following the requested format and
            -- plain counts (@csResultCount@, @csSavedLocalCount@) always
            -- decimal, since hex doesn't make a count more readable.
            --
            -- 'link' itself isn't a 'Scope' field -- it's stored
            -- alongside the record, not in it -- so it's shown as a
            -- trailing @linkOffset@: the same byte distance
            -- 'serializeControlRecord' actually packs into the
            -- record's own first word (see 'packWord0'), not a display
            -- computed value.
            describeControl :: (Int -> Text) -> Int -> Int -> Scope -> Text
            describeControl showAddr recordAddr link scope =
                describeFields showAddr scope <> ", linkOffset = " <> linkOffsetText <> " }"
                where
                    linkOffsetText
                        | link == nullAddr = "none"
                        | otherwise = show (link - recordAddr)

            describeFields :: (Int -> Text) -> Scope -> Text
            describeFields showAddr LoopScope{csStart, csEnd} =
                "LoopScope { csStart = " <> showAddr csStart <> ", csEnd = " <> showAddr csEnd
            describeFields showAddr BlockScope{csEnd} =
                "BlockScope { csEnd = " <> showAddr csEnd
            describeFields showAddr CallScope{csSavedFrameBase, csSavedLocalCount, csReturnPc, csResultCount} =
                "CallScope { csSavedFrameBase = "
                    <> showAddr csSavedFrameBase
                    <> ", csSavedLocalCount = "
                    <> show csSavedLocalCount
                    <> ", csReturnPc = "
                    <> showAddr csReturnPc
                    <> ", csResultCount = "
                    <> show csResultCount

            -- \| Right-pad an address's hex digits to just as many as
            -- 'capacity' could ever need -- a 512-byte memory only ever
            -- needs 3 (up to 0x1ff), so a `layout:hex` address dump has
            -- no reason to carry 'word32ToHex's full 8-digit width, the
            -- right choice for an arbitrary 32-bit *value* but overkill
            -- for an address bounded by this run's own configured
            -- memory size.
            hexAddrWidth :: Int -> Int
            hexAddrWidth capacity = max 1 (length (showHex (max 0 (capacity - 1)) ""))

            hexAddr :: Int -> Int -> Text
            hexAddr width a =
                let hex = showHex a ""
                 in "0x" <> toText (replicate (max 0 (width - length hex)) '0') <> toText hex

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
                        return $ Right (pc, instruction)

    instructionExecute _pc instruction =
        case instruction of
            I32Const value -> pushValue value >> nextPc instruction
            I32Add -> binary (+) >> nextPc instruction
            I32Sub -> binary (-) >> nextPc instruction
            I32Mul -> binary (*) >> nextPc instruction
            I32DivS -> signedDiv div >> nextPc instruction
            I32DivU -> unsignedDiv div >> nextPc instruction
            I32RemS -> signedDiv rem >> nextPc instruction
            I32RemU -> unsignedDiv rem >> nextPc instruction
            I32And -> binary (.&.) >> nextPc instruction
            I32Or -> binary (.|.) >> nextPc instruction
            I32Xor -> binary xor >> nextPc instruction
            I32Shl -> binary (\x y -> x `shiftL` (fromEnum y .&. 0x1F)) >> nextPc instruction
            I32ShrS -> binary (\x y -> x `shiftR` (fromEnum y .&. 0x1F)) >> nextPc instruction
            I32ShrU -> unsignedBinary (\x y -> toSign $ x `shiftR` (fromEnum y .&. 0x1F)) >> nextPc instruction
            I32Eqz -> unary (\x -> if x == 0 then 1 else 0) >> nextPc instruction
            I32Eq -> compareS (==) >> nextPc instruction
            I32Ne -> compareS (/=) >> nextPc instruction
            I32LtS -> compareS (<) >> nextPc instruction
            I32LeS -> compareS (<=) >> nextPc instruction
            I32GtS -> compareS (>) >> nextPc instruction
            I32GeS -> compareS (>=) >> nextPc instruction
            I32LtU -> compareU (<) >> nextPc instruction
            I32LeU -> compareU (<=) >> nextPc instruction
            I32GtU -> compareU (>) >> nextPc instruction
            I32GeU -> compareU (>=) >> nextPc instruction
            I32Load -> do
                addr <- popValue
                getWord (fromEnum addr) >>= pushValue
                nextPc instruction
            I32Store -> do
                value <- popValue
                addr <- popValue
                setWord (fromEnum addr) value
                nextPc instruction
            I32Load8U -> do
                addr <- popValue
                byte <- getByte (fromEnum addr)
                pushValue (fromIntegral byte)
                nextPc instruction
            I32Load8S -> do
                addr <- popValue
                byte <- getByte (fromEnum addr)
                pushValue (fromIntegral (fromIntegral byte :: Int8))
                nextPc instruction
            I32Store8 -> do
                value <- popValue
                addr <- popValue
                setByte (fromEnum addr) (fromIntegral value)
                nextPc instruction
            If -> do
                condition <- popValue
                if condition /= 0
                    then nextPc instruction
                    else do
                        Wasm32St{pc, mem} <- get
                        case findIfTargets mem (pc + byteSize instruction) of
                            Right (Just elseAddr, _) -> setPc (elseAddr + byteSize Else)
                            Right (Nothing, endPc) -> setPc (endPc + byteSize End)
                            Left err -> raiseInternalError $ "control flow error: " <> err
            Else -> do
                Wasm32St{pc, mem} <- get
                case findEndPc mem (pc + byteSize instruction) of
                    Right endPc -> setPc (endPc + byteSize End)
                    Left err -> raiseInternalError $ "control flow error: " <> err
            End -> do
                Wasm32St{pc, ctrlTop} <- get
                unless (ctrlTop == nullAddr) $ do
                    (scope, link) <- readControlAt ctrlTop
                    case scope of
                        -- A plain `end` never closes a call (`return`
                        -- does); if `ctrlTop` is one, this `end` belongs
                        -- to something already closed, not this record.
                        CallScope{} -> return ()
                        _
                            | csEnd scope == pc -> do
                                Wasm32St{sp} <- get
                                spliceOut ctrlTop (controlRecordWidth (tagOfScope scope)) sp
                                modify $ \st -> st{ctrlTop = link}
                            | otherwise -> return ()
                nextPc instruction
            Loop -> do
                Wasm32St{pc, mem} <- get
                case findEndPc mem (pc + byteSize instruction) of
                    Right endPc -> do
                        pushControl LoopScope{csStart = pc + byteSize instruction, csEnd = endPc}
                        nextPc instruction
                    Left err -> raiseInternalError $ "control flow error: " <> err
            Block -> do
                Wasm32St{pc, mem} <- get
                case findEndPc mem (pc + byteSize instruction) of
                    Right endPc -> do
                        pushControl BlockScope{csEnd = endPc}
                        nextPc instruction
                    Left err -> raiseInternalError $ "control flow error: " <> err
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
            Call paramCount resultCount -> do
                target <- popValue
                Wasm32St{sp, frameBase, localCount, pc} <- get
                let calleeFrameBase = sp + (paramCount - 1) * byteSizeT @w
                pushControl
                    CallScope
                        { csSavedFrameBase = frameBase
                        , csSavedLocalCount = localCount
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
                modify $ \st -> st{sp = addr, frameBase = addr}
                nextPc instruction
            Halt -> modify $ \st -> st{stopped = True}
        where
            unary f = popValue >>= pushValue . f
            binary op = do
                y <- popValue
                x <- popValue
                pushValue (x `op` y)
            unsignedBinary op = do
                y <- fromSign <$> popValue
                x <- fromSign <$> popValue
                pushValue (op x y)
            signedDiv op = do
                y <- popValue
                x <- popValue
                if y == 0
                    then raiseInternalError "integer divide by zero"
                    else
                        if x == minBound && y == -1
                            then raiseInternalError "integer overflow"
                            else pushValue (x `op` y)
            unsignedDiv op = do
                y <- fromSign <$> popValue
                x <- fromSign <$> popValue
                if y == 0
                    then raiseInternalError "integer divide by zero"
                    else pushValue (toSign (x `op` y))
            compareS op = do
                y <- popValue
                x <- popValue
                pushValue $ if x `op` y then 1 else 0
            compareU op = do
                y <- fromSign <$> popValue
                x <- fromSign <$> popValue
                pushValue $ if x `op` y then 1 else 0
