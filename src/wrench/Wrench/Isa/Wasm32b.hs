{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

{- | A from-scratch, minimal WebAssembly-inspired 32-bit ISA. Unlike
"Wrench.Isa.Wasm32", this one is deliberately scoped down: push a
constant, combine values with arithmetic\/comparison ops, structured
control flow, locals, and function calls -- no closures yet -- using the
same generic translator pipeline as e.g. F32a/Acc32 rather than any
bespoke lowering (in particular, unlike "Wrench.Isa.Wasm32"'s
@FuncEnter@, a callee has no self-describing header: `call` states its
own paramCount\/resultCount as plain literal operands, the same way
`br`'s depth or `locals`'s count are caller-known literals rather than
looked up from the target).

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
module Wrench.Isa.Wasm32b (
    Isa (..),
    Wasm32bState,
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Default (def)
import Data.Text qualified as T
import Relude
import Relude.Unsafe qualified as Unsafe
import Text.Megaparsec (choice, try)
import Text.Megaparsec.Char (char, hspace, hspace1, string)
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Report
import Wrench.Translator.Parser.Misc
import Wrench.Translator.Parser.Types
import Wrench.Translator.Types

data Isa w l
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
    | -- | Pop an address, push the 4-byte word stored there. Real
      -- WebAssembly's @i32.load@ also takes a static offset immediate
      -- (added to the popped address) -- omitted here, matching
      -- "Wrench.Isa.Wasm32"'s own simplification: a hand-written example
      -- that needs base+offset addressing can just do the add explicitly
      -- with `i32.add` before the load.
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
    | -- | Reserve and zero-fill @n@ *extra* locals right after whatever
      -- params the active function already has (indices
      -- @paramCount..paramCount+n-1@), at a fixed offset from 'frameBase'
      -- -- immune to whatever `sp` does afterward (control records,
      -- operand churn), unlike a value merely sitting on the stack.
      -- Optional: a function that needs no extra locals can just omit it.
      -- Must be the very first instruction in a function body if present
      -- -- see 'spliceIn's haddock for why.
      Locals Int
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
    | Halt
    deriving (Eq, Show)

instance CommentStart (Isa w l) where
    commentStart = ";"

instance (MachineWord w) => MnemonicParser (Isa w (Ref w)) where
    mnemonic =
        hspace *> cmd <* eol' (commentStart @(Isa _ _))
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
                    , try (Locals <$> cmd1 "locals" intLit)
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
                    , cmd0 "halt" Halt
                    ]

cmd0 :: String -> a -> Parser a
cmd0 mnemonic constructor = string mnemonic >> return constructor

cmd1 :: String -> Parser a -> Parser a
cmd1 mnemonic arg = string mnemonic >> hspace1 >> arg

-- | A small non-negative integer literal: a branch's target depth (how
-- many enclosing `block`\/`loop` scopes out to reach, 0 = innermost), a
-- local's index, a `locals` count, or one of `call`'s two trailing counts.
intLit :: Parser Int
intLit = Unsafe.read <$> num

-- | Separates `call`'s two operands (paramCount, resultCount) --
-- comma-separated the same way the original "Wrench.Isa.Wasm32"'s numeric
-- @.func@ form is (@func 2, 0, 1@).
comma :: Parser ()
comma = hspace >> void (char ',') >> hspace

instance DerefMnemonic (Isa w) w where
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
        Locals n -> Locals n
        LocalGet i -> LocalGet i
        LocalSet i -> LocalSet i
        LocalTee i -> LocalTee i
        Call p r -> Call p r
        Return -> Return
        Halt -> Halt

instance ByteSize (Isa w l) where
    byteSize I32Const{} = 5
    byteSize Br{} = 2
    byteSize BrIf{} = 2
    byteSize Locals{} = 2
    byteSize LocalGet{} = 2
    byteSize LocalSet{} = 2
    byteSize LocalTee{} = 2
    byteSize Call{} = 3
    byteSize _ = 1

type Wasm32bState w = MachineState (IoMem (Isa w w) w) w

-- | One control-record's payload, carrying exactly the fields each shape
-- needs rather than a bare tag alongside untyped fields (same reasoning
-- as the original wasm32's @RecordExtra@: a mismatch between kind and
-- payload can't be constructed in the first place). 'LoopScope' and
-- 'BlockScope' both carry @csEnd@ -- a plain `end`'s own address, compared
-- against it to tell "this end closes the innermost open scope" apart from
-- "this end closes a plain if" (only the former pops, and only that shared
-- field is needed to decide it -- see the `End` case in
-- 'instructionExecute'). Never itself stored raw: 'pushControl' derives
-- the persisted, untyped 'ControlTag' + fields from it, and
-- 'readControlAt' reconstructs it on the way back -- construction-time
-- only, exactly like @RecordExtra@.
data ControlKind
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
      -- truncated straight to the *callee's* live 'frameBase', reclaiming
      -- the record and every one of the callee's locals in one step
      -- (correct only because 'pushControl'\/'Locals' always keep this
      -- record sitting right above every local, never in between -- see
      -- 'Locals'\/'spliceIn'). @csSavedFrameBase@\/@csSavedLocalCount@ are
      -- what the *caller* had, restored on return.
      CallScope {csSavedFrameBase :: Int, csSavedLocalCount :: Int, csReturnPc :: Int, csResultCount :: Int}
    deriving (Show)

-- | The persisted tag for a control record -- the untyped memory word
-- 'pushControl' writes and 'readControlAt' reads to know which
-- 'ControlKind' shape to reconstruct. Kept separate from 'ControlKind'
-- the same way the original wasm32 keeps @RecordKind@ separate from
-- @RecordExtra@.
data ControlTag = LoopTag | BlockTag | CallTag
    deriving (Bounded, Enum, Eq, Show)

-- | Width, in words, of one control record: a link plus a tag plus all
-- four of 'ControlKind's possible fields, always present regardless of
-- shape (a 'BlockScope' just leaves two slots as 0 and never reads them
-- back) -- fixed width keeps the chain-walk arithmetic in
-- 'readControlAt'\/'pushControl' uniform, the same tradeoff the original
-- wasm32's control records make. 'CallScope' happens to need all four,
-- which is why this grew from the structured-only shapes' three.
controlEntryWidth :: Int
controlEntryWidth = 6

controlLinkOffset
    , controlTagOffset
    , controlField1Offset
    , controlField2Offset
    , controlField3Offset
    , controlField4Offset ::
        Int
controlLinkOffset = 0
controlTagOffset = 1
controlField1Offset = 2
controlField2Offset = 3
controlField3Offset = 4
controlField4Offset = 5

-- | Sentinel for "no enclosing control record" ('ctrlTop', or a record's
-- own `link`), and for "no active function frame" when 'findCall' walks
-- past the bottom of the chain without finding a 'CallScope'.
nullAddr :: Int
nullAddr = -1

data MachineState mem w = State
    { pc :: Int
    , sp :: Int
    -- ^ Top of the *one* stack: locals, operand values, and control
    -- records all live here, interleaved in whatever order they were
    -- pushed (a `loop`\/`block`\/`call` pushes its own record right on top
    -- of the operand values that happen to be there already, exactly like
    -- the original wasm32's unified stack) -- no separate control-stack
    -- region, no separate call stack, no second stack pointer.
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
    -- address). Starts at 'memTop' -- so a program that never calls
    -- anything behaves exactly as before functions existed -- and is
    -- carved out of the caller's already-pushed arguments by `call`,
    -- saved in the callee's own 'CallScope' record, and restored by
    -- `return`.
    , localCount :: Int
    -- ^ How many words at 'frameBase' are locals (params, plus however
    -- many extra 'Locals' declared) rather than operand-stack content --
    -- set to @paramCount@ by `call`, bumped by 'Locals', saved\/restored
    -- across calls exactly like 'frameBase' -- so report views know where
    -- the *stack* actually starts for whichever frame is currently active.
    , mem :: mem
    , stopped :: Bool
    , internalError :: Maybe Text
    }
    deriving (Show)

instance InitState (IoMem (Isa w w) w) (MachineState (IoMem (Isa w w) w) w) where
    initState pc dump _randomStream =
        State
            { pc
            , sp = memTop dump
            , ctrlTop = nullAddr
            , frameBase = memTop dump
            , localCount = 0
            , mem = dump
            , stopped = False
            , internalError = Nothing
            }

-- | Where locals (if any) start: the upper half of the configured
-- memory, code+data occupying the lower half.
memTop :: IoMem (Isa w w) w -> Int
memTop IoMem{mIoCells = Mem{memorySize}} = memorySize `div` 2

setPc :: Int -> State (MachineState (IoMem (Isa w w) w) w) ()
setPc addr = modify $ \st -> st{pc = addr}

nextPc :: Isa w w -> State (MachineState (IoMem (Isa w w) w) w) ()
nextPc instruction = do
    State{pc} <- get
    setPc (pc + byteSize instruction)

raiseInternalError :: Text -> State (MachineState (IoMem (Isa w w) w) w) ()
raiseInternalError msg = modify $ \st -> st{internalError = Just msg}

getWord :: (MachineWord w) => Int -> State (MachineState (IoMem (Isa w w) w) w) w
getWord addr = do
    st@State{mem} <- get
    case readWord mem addr of
        Right (mem', w) -> put st{mem = mem'} >> return w
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return def

setWord :: (MachineWord w) => Int -> w -> State (MachineState (IoMem (Isa w w) w) w) ()
setWord addr w = do
    st@State{mem} <- get
    case writeWord mem addr w of
        Right mem' -> put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err

getByte :: (MachineWord w) => Int -> State (MachineState (IoMem (Isa w w) w) w) Word8
getByte addr = do
    st@State{mem} <- get
    case readByte mem addr of
        Right (mem', b) -> put st{mem = mem'} >> return b
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return 0

setByte :: (MachineWord w) => Int -> Word8 -> State (MachineState (IoMem (Isa w w) w) w) ()
setByte addr b = do
    st@State{mem} <- get
    case writeByte mem addr b of
        Right mem' -> put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err

-- | This memory is byte-addressed and a `w` occupies 'byteSizeT' bytes,
-- not one address, so `sp` must step by that width, not by 1.
pushValue :: forall w. (MachineWord w) => w -> State (MachineState (IoMem (Isa w w) w) w) ()
pushValue value = do
    State{sp} <- get
    setWord sp value
    modify $ \st -> st{sp = sp + byteSizeT @w}

-- | No underflow guard: popping past the bottom of the stack region reads
-- whatever is physically below it (code/data memory, or a memory error at
-- the very bottom) -- a program error this ISA doesn't validate against
-- ahead of time.
popValue :: forall w. (MachineWord w) => State (MachineState (IoMem (Isa w w) w) w) w
popValue = do
    State{sp} <- get
    let sp' = sp - byteSizeT @w
    modify $ \st -> st{sp = sp'}
    getWord sp'

-- | Address of local @i@: a fixed offset from the *active* function's
-- 'frameBase', unaffected by `sp`\/`ctrlTop` -- the whole point, since a
-- value merely sitting on the stack isn't (see 'Locals'\/the module
-- haddock). Uniform across params and extra locals alike -- no branch on
-- @i@ -- because 'Locals' relocates the frame's own control record above
-- every local it declares (see 'spliceIn'), so nothing ever sits between
-- them.
localAddr :: forall w. (MachineWord w) => Int -> State (MachineState (IoMem (Isa w w) w) w) Int
localAddr i = do
    State{frameBase} <- get
    return $ frameBase + i * byteSizeT @w

-- | Remove the @widthWords@-word record at @r@, shifting everything
-- above it (up to @top@) down to close the gap, and shrinking `sp` to
-- match -- the same splice the original wasm32 uses to close a
-- block\/loop record without disturbing whatever its body pushed above
-- it.
spliceOut :: forall w. (MachineWord w) => Int -> Int -> Int -> State (MachineState (IoMem (Isa w w) w) w) ()
spliceOut r widthWords top = do
    let step = byteSizeT @w
        width = widthWords * step
    forM_ [0, step .. top - r - width - 1] $ \i -> getWord (r + width + i) >>= setWord (r + i)
    modify $ \st -> st{sp = top - width}

-- | The mirror of 'spliceOut': insert @n@ zero-filled words at @at@,
-- shifting everything from @at@ up to @top@ (exclusive) up by that many
-- words, and growing `sp` to match. 'Locals' uses this to grow the active
-- frame's locals region while keeping every local contiguous: a `call`
-- always leaves its own 'CallScope' sitting right above the params (the
-- only locals that exist yet), so inserting @n@ new words right at that
-- boundary slides the record up out of the way, making room for the
-- extra locals *underneath* it rather than above -- see 'localAddr's
-- haddock. Safe to call with @at == top@ (nothing above to shift, and no
-- enclosing `call` at all -- the pre-functions, single-frame case):
-- degenerates to a plain reservation, identical to what 'Locals' used to
-- do directly.
spliceIn :: forall w. (MachineWord w) => Int -> Int -> Int -> State (MachineState (IoMem (Isa w w) w) w) ()
spliceIn at n top = do
    let step = byteSizeT @w
        width = n * step
    forM_ [top - step, top - 2 * step .. at] $ \i -> getWord i >>= setWord (i + width)
    forM_ [0, step .. width - step] $ \i -> setWord (at + i) def
    modify $ \st -> st{sp = top + width}

-- | Push a control record: write it at the current 'sp' (right on top of
-- whatever operand values are already there), link it to the previously
-- innermost record, and make it the new innermost one.
pushControl :: forall w. (MachineWord w) => ControlKind -> State (MachineState (IoMem (Isa w w) w) w) ()
pushControl scope = do
    State{sp, ctrlTop} <- get
    let step = byteSizeT @w
        (tag, f1, f2, f3, f4) = case scope of
            LoopScope{csStart, csEnd} -> (LoopTag, csStart, csEnd, 0, 0)
            BlockScope{csEnd} -> (BlockTag, 0, csEnd, 0, 0)
            CallScope{csSavedFrameBase, csSavedLocalCount, csReturnPc, csResultCount} ->
                (CallTag, csSavedFrameBase, csSavedLocalCount, csReturnPc, csResultCount)
    setWord (sp + controlLinkOffset * step) (toEnum ctrlTop)
    setWord (sp + controlTagOffset * step) (toEnum (fromEnum tag))
    setWord (sp + controlField1Offset * step) (toEnum f1)
    setWord (sp + controlField2Offset * step) (toEnum f2)
    setWord (sp + controlField3Offset * step) (toEnum f3)
    setWord (sp + controlField4Offset * step) (toEnum f4)
    modify $ \st -> st{ctrlTop = sp, sp = sp + controlEntryWidth * step}

-- | Read the control record based at @addr@ (not necessarily 'ctrlTop'
-- itself -- 'branchTo'\/'unwindTo'\/'findCall' walk the chain via `link`
-- without disturbing anything), returning its payload and its `link`.
readControlAt :: forall w. (MachineWord w) => Int -> State (MachineState (IoMem (Isa w w) w) w) (ControlKind, Int)
readControlAt addr = do
    let step = byteSizeT @w
    link <- fromEnum <$> getWord (addr + controlLinkOffset * step)
    tag <- toEnum . fromEnum <$> getWord (addr + controlTagOffset * step)
    f1 <- fromEnum <$> getWord (addr + controlField1Offset * step)
    f2 <- fromEnum <$> getWord (addr + controlField2Offset * step)
    f3 <- fromEnum <$> getWord (addr + controlField3Offset * step)
    f4 <- fromEnum <$> getWord (addr + controlField4Offset * step)
    let scope = case (tag :: ControlTag) of
            LoopTag -> LoopScope{csStart = f1, csEnd = f2}
            BlockTag -> BlockScope{csEnd = f2}
            CallTag -> CallScope{csSavedFrameBase = f1, csSavedLocalCount = f2, csReturnPc = f3, csResultCount = f4}
    return (scope, link)

-- | Find the `if` at @start@'s branch targets by scanning forward,
-- tracking nested `if`\/`loop`\/`block` scopes so a nested `else`\/`end`
-- doesn't get mistaken for this one's. Returns the matching `else`'s
-- address (if there is one) and the matching `end`'s address.
findIfTargets :: (MachineWord w) => IoMem (Isa w w) w -> Int -> Either Text (Maybe Int, Int)
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
findEndPc :: (MachineWord w) => IoMem (Isa w w) w -> Int -> Either Text Int
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
findControlTarget :: forall w. (MachineWord w) => Int -> State (MachineState (IoMem (Isa w w) w) w) (Either Text Int)
findControlTarget depth = do
    State{ctrlTop} <- get
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
findCall :: forall w. (MachineWord w) => State (MachineState (IoMem (Isa w w) w) w) (Either Text Int)
findCall = do
    State{ctrlTop} <- get
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
-- has pushed above it stay exactly as they are, since the next iteration
-- needs them. A 'CallScope' is never @keepOpen@ (only `return` closes
-- one, and it always exits); everything else splices the record's own
-- words out of the stack (preserving everything above them), unlinks it,
-- and jumps past its `end`.
collapseControl :: forall w. (MachineWord w) => Int -> Bool -> State (MachineState (IoMem (Isa w w) w) w) ()
collapseControl r keepOpen = do
    (scope, link) <- readControlAt r
    case scope of
        LoopScope{csStart} | keepOpen -> setPc csStart
        CallScope{csSavedFrameBase, csSavedLocalCount, csReturnPc, csResultCount} -> do
            -- Read every field before touching the stack below: popping
            -- results and truncating overwrite this record's own bytes.
            results <- popValues csResultCount
            State{frameBase} <- get
            modify $ \st -> st{sp = frameBase}
            mapM_ pushValue results
            modify $ \st -> st{frameBase = csSavedFrameBase, localCount = csSavedLocalCount, ctrlTop = link}
            setPc csReturnPc
        _ -> do
            State{sp} <- get
            spliceOut r controlEntryWidth sp
            modify $ \st -> st{ctrlTop = link}
            setPc (csEnd scope + byteSize End)

popValues :: forall w. (MachineWord w) => Int -> State (MachineState (IoMem (Isa w w) w) w) [w]
popValues n = reverse <$> replicateM n popValue

-- | Collapse every open record from `ctrlTop` down to and including @r@,
-- keeping @r@ itself open only if @keepOpen@ (branching back into a
-- loop). Anything strictly above @r@ is always fully closed along the
-- way, regardless of its own kind -- branching past a scope exits it
-- unconditionally.
unwindTo :: forall w. (MachineWord w) => Int -> Bool -> State (MachineState (IoMem (Isa w w) w) w) ()
unwindTo r keepOpen = do
    State{ctrlTop} <- get
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
branchTo :: forall w. (MachineWord w) => Int -> State (MachineState (IoMem (Isa w w) w) w) ()
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

instance (MachineWord w) => StateInterspector (MachineState (IoMem (Isa w w) w) w) (IoMem (Isa w w) w) (Isa w w) w where
    programCounter State{pc} = pc
    memoryDump State{mem} = mem
    ioStreams State{mem = IoMem{mIoStreams}} = mIoStreams
    isHalted State{stopped} = stopped
    reprState labels st v
        | Just v' <- defaultView labels st v = v'
    reprState labels st@State{mem, sp, frameBase, localCount} v =
        case T.splitOn ":" v of
            ["stack", f] -> formatValues f values
            ["locals", f] -> formatValues f localValues
            [r] -> reprState labels st (r <> ":dec")
            [r, _] -> unknownView r
            _ -> errorView v
        where
            step = byteSizeT @w
            -- Where the *active* frame's locals end and its own operand
            -- stack begins -- everything from here up to `sp` is "stack"
            -- for report-view purposes, which, same as before functions
            -- existed, includes this frame's own control records (and any
            -- calls it in turn made) as raw words, not just plain operand
            -- values -- this view has never distinguished the two.
            frameStackBase = frameBase + localCount * step
            -- Top first, matching the old list's head-is-top convention.
            values = mapMaybe (\a -> eitherToMaybe (readWord mem a) <&> snd) [sp - step, sp - 2 * step .. frameStackBase]
            localValues =
                mapMaybe
                    (\a -> eitherToMaybe (readWord mem a) <&> snd)
                    [frameBase, frameBase + step .. frameStackBase - step]

            formatValues "dec" vs = toText $ intercalate ":" $ map show vs
            formatValues "hex" vs = T.intercalate ":" $ map (toText . word32ToHex) vs
            formatValues f _ = unknownFormat f

            eitherToMaybe = either (const Nothing) Just

instance (MachineWord w) => Machine (MachineState (IoMem (Isa w w) w) w) (Isa w w) w where
    instructionFetch = do
        st <- get
        case st of
            State{stopped = True} -> return $ Left halted
            State{internalError = Just err} -> return $ Left err
            State{pc, mem} ->
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
                        State{pc, mem} <- get
                        case findIfTargets mem (pc + byteSize instruction) of
                            Right (Just elseAddr, _) -> setPc (elseAddr + byteSize Else)
                            Right (Nothing, endPc) -> setPc (endPc + byteSize End)
                            Left err -> raiseInternalError $ "control flow error: " <> err
            Else -> do
                State{pc, mem} <- get
                case findEndPc mem (pc + byteSize instruction) of
                    Right endPc -> setPc (endPc + byteSize End)
                    Left err -> raiseInternalError $ "control flow error: " <> err
            End -> do
                State{pc, ctrlTop} <- get
                unless (ctrlTop == nullAddr) $ do
                    (scope, link) <- readControlAt ctrlTop
                    case scope of
                        -- A plain `end` never closes a call (`return`
                        -- does); if `ctrlTop` is one, this `end` belongs
                        -- to something already closed, not this record.
                        CallScope{} -> return ()
                        _
                            | csEnd scope == pc -> do
                                State{sp} <- get
                                spliceOut ctrlTop controlEntryWidth sp
                                modify $ \st -> st{ctrlTop = link}
                            | otherwise -> return ()
                nextPc instruction
            Loop -> do
                State{pc, mem} <- get
                case findEndPc mem (pc + byteSize instruction) of
                    Right endPc -> do
                        pushControl LoopScope{csStart = pc + byteSize instruction, csEnd = endPc}
                        nextPc instruction
                    Left err -> raiseInternalError $ "control flow error: " <> err
            Block -> do
                State{pc, mem} <- get
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
            Locals n -> do
                State{frameBase, localCount, sp, ctrlTop} <- get
                spliceIn (frameBase + localCount * byteSizeT @w) n sp
                -- 'spliceIn' physically relocated whatever sat above the
                -- inserted words -- if that's this frame's own 'CallScope'
                -- (see 'localAddr's haddock: it's guaranteed to be, if
                -- anything, since 'Locals' must run before any nested
                -- `block`\/`loop`\/`call` could push above it), 'ctrlTop'
                -- must move with it or it'll point at stale, now
                -- zero-filled bytes.
                let ctrlTop' = if ctrlTop == nullAddr then nullAddr else ctrlTop + n * byteSizeT @w
                modify $ \st -> st{localCount = localCount + n, ctrlTop = ctrlTop'}
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
                State{sp, frameBase, localCount, pc} <- get
                let calleeFrameBase = sp - paramCount * byteSizeT @w
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
