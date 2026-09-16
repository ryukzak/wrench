{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | A from-scratch, minimal WebAssembly-inspired 32-bit ISA. Unlike
"Wrench.Isa.Wasm32", this one is deliberately scoped down for now: push
a constant, combine values with arithmetic\/comparison ops, halt.
No locals, no structured control flow, no calls -- just a plain stack
machine, using the same generic translator pipeline as e.g. F32a/Acc32
rather than any bespoke lowering.
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
import Text.Megaparsec.Char (hspace, hspace1, string)
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
                    , cmd0 "if" If
                    , cmd0 "else" Else
                    , cmd0 "end" End
                    , cmd0 "loop" Loop
                    , cmd0 "block" Block
                    , try (Br <$> cmd1 "br" depth)
                    , try (BrIf <$> cmd1 "br_if" depth)
                    , cmd0 "dup" Dup
                    , cmd0 "halt" Halt
                    ]

cmd0 :: String -> a -> Parser a
cmd0 mnemonic constructor = string mnemonic >> return constructor

cmd1 :: String -> Parser a -> Parser a
cmd1 mnemonic arg = string mnemonic >> hspace1 >> arg

-- | A branch's target depth: how many enclosing `block`\/`loop` scopes out
-- to reach, 0 = innermost.
depth :: Parser Int
depth = Unsafe.read <$> num

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
        If -> If
        Else -> Else
        End -> End
        Loop -> Loop
        Block -> Block
        Br d -> Br d
        BrIf d -> BrIf d
        Dup -> Dup
        Halt -> Halt

instance ByteSize (Isa w l) where
    byteSize I32Const{} = 5
    byteSize Br{} = 2
    byteSize BrIf{} = 2
    byteSize _ = 1

type Wasm32bState w = MachineState (IoMem (Isa w w) w) w

-- | One entry in 'controlStack', carrying exactly the fields each shape
-- needs rather than a bare tag alongside untyped fields (same reasoning
-- as the original wasm32's @RecordExtra@: a mismatch between kind and
-- payload can't be constructed in the first place). Both shapes carry
-- @csEnd@ -- a plain `end`'s own address, compared against it to tell
-- "this end closes the innermost open scope" apart from "this end closes
-- a plain if" (only the former pops, and only that shared field is
-- needed to decide it -- see the `End` case in 'instructionExecute').
data ControlKind
    = -- | Reaching this (via `br`\/`br_if`) leaves it open: jump to
      -- @csStart@ (just after `loop`) to run the body again.
      LoopScope {csStart :: Int, csEnd :: Int}
    | -- | Reaching this closes it: jump just past @csEnd@ (see
      -- 'branchTo'), removing it from 'controlStack' too, since unlike a
      -- loop there's nothing left to continue.
      BlockScope {csEnd :: Int}
    deriving (Show)

data MachineState mem w = State
    { pc :: Int
    , sp :: Int
    -- ^ Top of the operand stack, which lives in ordinary byte-addressed
    -- memory (see 'memTop') rather than as a separate Haskell structure --
    -- no locals, no control stack, no call frames, just this one region.
    , mem :: mem
    , controlStack :: [ControlKind]
    -- ^ One entry per currently-open `block`\/`loop`, innermost first --
    -- the one piece of runtime state `if`\/`else`\/`end` don't need but
    -- `loop`\/`block`\/`br`\/`br_if` do (see the module haddock's note on
    -- why).
    , stopped :: Bool
    , internalError :: Maybe Text
    }
    deriving (Show)

instance InitState (IoMem (Isa w w) w) (MachineState (IoMem (Isa w w) w) w) where
    initState pc dump _randomStream =
        State
            { pc
            , sp = memTop dump
            , mem = dump
            , controlStack = []
            , stopped = False
            , internalError = Nothing
            }

-- | Where the operand stack starts: the upper half of the configured
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

-- | Branch to the `block`\/`loop` @depth@ levels out (0 = innermost),
-- unconditionally: drop the @depth@ scopes strictly between here and the
-- target (they're being exited, regardless of kind), then act on the
-- target itself according to its own shape.
branchTo :: Int -> State (MachineState (IoMem (Isa w w) w) w) ()
branchTo depth = do
    State{controlStack} <- get
    case drop depth controlStack of
        [] -> raiseInternalError "br/br_if: depth out of range"
        found@(scope : rest) -> case scope of
            LoopScope{csStart} -> do
                modify $ \st -> st{controlStack = found}
                setPc csStart
            BlockScope{csEnd} -> do
                modify $ \st -> st{controlStack = rest}
                setPc (csEnd + byteSize End)

instance (MachineWord w) => StateInterspector (MachineState (IoMem (Isa w w) w) w) (IoMem (Isa w w) w) (Isa w w) w where
    programCounter State{pc} = pc
    memoryDump State{mem} = mem
    ioStreams State{mem = IoMem{mIoStreams}} = mIoStreams
    isHalted State{stopped} = stopped
    reprState labels st v
        | Just v' <- defaultView labels st v = v'
    reprState labels st@State{mem, sp} v =
        case T.splitOn ":" v of
            ["stack", f] -> formatValues f values
            [r] -> reprState labels st (r <> ":dec")
            [r, _] -> unknownView r
            _ -> errorView v
        where
            step = byteSizeT @w
            -- Top first, matching the old list's head-is-top convention.
            values = mapMaybe (\a -> eitherToMaybe (readWord mem a) <&> snd) [sp - step, sp - 2 * step .. memTop mem]

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
                State{pc, controlStack} <- get
                case controlStack of
                    (scope : rest) | csEnd scope == pc -> modify $ \st -> st{controlStack = rest}
                    _ -> return ()
                nextPc instruction
            Loop -> do
                State{pc, mem} <- get
                case findEndPc mem (pc + byteSize instruction) of
                    Right endPc -> do
                        let scope = LoopScope{csStart = pc + byteSize instruction, csEnd = endPc}
                        modify $ \st -> st{controlStack = scope : controlStack st}
                        nextPc instruction
                    Left err -> raiseInternalError $ "control flow error: " <> err
            Block -> do
                State{pc, mem} <- get
                case findEndPc mem (pc + byteSize instruction) of
                    Right endPc -> do
                        modify $ \st -> st{controlStack = BlockScope{csEnd = endPc} : controlStack st}
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
