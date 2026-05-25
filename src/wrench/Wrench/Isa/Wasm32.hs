{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | A small WebAssembly-inspired 32-bit virtual ISA for Wrench.
module Wrench.Isa.Wasm32 (
    Isa (..),
    MachineState (..),
    Wasm32State,
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Default (def)
import Data.Text qualified as T
import Relude
import Relude.Unsafe qualified as Unsafe
import Text.Megaparsec (anySingle, choice, try)
import Text.Megaparsec.Char (char, hspace, hspace1, string)
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Report
import Wrench.Translator.Parser.Misc
import Wrench.Translator.Parser.Types
import Wrench.Translator.Types

data Isa w l
    = Func {funcParams :: [String], funcLocals :: [String], funcResults :: Int}
    | EndFunc
    | I32Const l
    | Drop
    | Select
    | LocalGet String
    | LocalSet String
    | LocalTee String
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
    | I32Load
    | I32Store
    | I32Load8S
    | I32Load8U
    | I32Store8
    | Block String
    | Loop String
    | If String
    | Else
    | End
    | Br String
    | BrIf String
    | Call l
    | Return
    | Halt
    | Unreachable
    | Nop
    deriving (Show)

instance CommentStart (Isa w l) where
    commentStart = ";"

instance (MachineWord w) => MnemonicParser (Isa w (Ref w)) where
    mnemonic =
        hspace *> cmd <* eol' (commentStart @(Isa _ _))
        where
            cmd =
                choice
                    [ try func
                    , try endFunc
                    , I32Const <$> cmd1 "i32.const" referenceWithDirective
                    , cmd0 "drop" Drop
                    , cmd0 "select" Select
                    , LocalGet <$> cmd1 "local.get" localId
                    , LocalSet <$> cmd1 "local.set" localId
                    , LocalTee <$> cmd1 "local.tee" localId
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
                    , cmd0 "i32.load8_s" I32Load8S
                    , cmd0 "i32.load8_u" I32Load8U
                    , cmd0 "i32.load" I32Load
                    , cmd0 "i32.store8" I32Store8
                    , cmd0 "i32.store" I32Store
                    , Block <$> cmd1 "block" controlLabel
                    , Loop <$> cmd1 "loop" controlLabel
                    , If <$> cmd1 "if" controlLabel
                    , cmd0 "else" Else
                    , cmd0 "end" End
                    , BrIf <$> cmd1 "br_if" controlLabel
                    , Br <$> cmd1 "br" controlLabel
                    , Call <$> cmd1 "call" reference
                    , cmd0 "return" Return
                    , cmd0 "halt" Halt
                    , cmd0 "unreachable" Unreachable
                    , cmd0 "nop" Nop
                    ]

func :: Parser (Isa w (Ref w))
func = try $ do
    optionalDot
    void $ string "func"
    hspace
    choice [try numericFunc, try keywordFunc, return Func{funcParams = [], funcLocals = [], funcResults = 0}]

endFunc :: Parser (Isa w (Ref w))
endFunc = try $ do
    optionalDot
    void $ string "endfunc"
    return EndFunc

numericFunc :: Parser (Isa w (Ref w))
numericFunc = do
    params <- number
    comma
    locals <- number
    comma
    results <- number
    return
        Func
            { funcParams = map show [0 .. params - 1]
            , funcLocals = map show [params .. params + locals - 1]
            , funcResults = results
            }

keywordFunc :: Parser (Isa w (Ref w))
keywordFunc = buildFunc <$> some funcWord

buildFunc :: [String] -> Isa w l
buildFunc tokens =
    let params = collectAfter "params" ["locals", "result", "results"] tokens
        locals = collectAfter "locals" ["params", "result", "results"] tokens
        results =
            case dropWhile (/= "result") tokens of
                ("result" : "i32" : _) -> 1
                ("result" : "none" : _) -> 0
                ("result" : n : _) -> Unsafe.read n
                _ -> case dropWhile (/= "results") tokens of
                    ("results" : n : _) -> Unsafe.read n
                    _ -> 0
     in Func{funcParams = params, funcLocals = locals, funcResults = results}

collectAfter :: String -> [String] -> [String] -> [String]
collectAfter key stops tokens =
    case dropWhile (/= key) tokens of
        [] -> []
        (_ : rest) -> takeWhile (`notElem` stops) rest

funcWord :: Parser String
funcWord = do
    hspace
    void $ optional (char ',')
    hspace
    some $ try $ do
        c <- anySingle
        guard (c `notElem` [' ', '\t', '\n', '\r', ',', ';'])
        return c

number :: Parser Int
number = Unsafe.read <$> num

comma :: Parser ()
comma = hspace >> void (char ',') >> hspace

cmd0 :: String -> a -> Parser a
cmd0 mnemonic constructor = string mnemonic >> return constructor

cmd1 :: String -> Parser a -> Parser a
cmd1 mnemonic arg = string mnemonic >> hspace1 >> arg

optionalDot :: Parser ()
optionalDot = void (optional (char '.'))

localId :: Parser String
localId = some $ try $ do
    c <- anySingle
    guard (c `notElem` [' ', '\t', '\n', '\r', ',', ';'])
    return c

controlLabel :: Parser String
controlLabel = localId

instance DerefMnemonic (Isa w) w where
    derefMnemonic f _offset i =
        case i of
            I32Const l -> I32Const (deref' f l)
            Call l -> Call (deref' f l)
            Func{funcParams, funcLocals, funcResults} -> Func{funcParams, funcLocals, funcResults}
            EndFunc -> EndFunc
            Drop -> Drop
            Select -> Select
            LocalGet n -> LocalGet n
            LocalSet n -> LocalSet n
            LocalTee n -> LocalTee n
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
            I32Load8S -> I32Load8S
            I32Load8U -> I32Load8U
            I32Store8 -> I32Store8
            Block l -> Block l
            Loop l -> Loop l
            If l -> If l
            Else -> Else
            End -> End
            Br l -> Br l
            BrIf l -> BrIf l
            Return -> Return
            Halt -> Halt
            Unreachable -> Unreachable
            Nop -> Nop

instance ByteSize (Isa w l) where
    byteSize I32Const{} = 5
    byteSize Call{} = 5
    byteSize LocalGet{} = 2
    byteSize LocalSet{} = 2
    byteSize LocalTee{} = 2
    byteSize Func{} = 4
    byteSize Block{} = 2
    byteSize Loop{} = 2
    byteSize If{} = 2
    byteSize Br{} = 2
    byteSize BrIf{} = 2
    byteSize _ = 1

type Wasm32State w = MachineState (IoMem (Isa w w) w) w

data ControlKind = ControlBlock | ControlLoop | ControlIf
    deriving (Eq, Show)

data ControlFrame = ControlFrame
    { cfLabel :: String
    , cfKind :: ControlKind
    , cfStartPc :: Int
    , cfEndPc :: Int
    , cfFrameDepth :: Int
    }
    deriving (Show)

data Frame w = Frame
    { frReturnPc :: Maybe Int
    , frLocals :: [(String, w)]
    , frResults :: Int
    }
    deriving (Show)

data PendingCall w = PendingCall
    { pcReturnPc :: Maybe Int
    , pcArgs :: [w]
    }
    deriving (Show)

data MachineState mem w = State
    { pc :: Int
    , mem :: mem
    , operandStack :: [w]
    , frames :: [Frame w]
    , controlStack :: [ControlFrame]
    , pendingCall :: Maybe (PendingCall w)
    , stopped :: Bool
    , internalError :: Maybe Text
    }
    deriving (Show)

instance InitState (IoMem (Isa w w) w) (MachineState (IoMem (Isa w w) w) w) where
    initState pc dump _randomStream =
        State
            { pc
            , mem = dump
            , operandStack = []
            , frames = []
            , controlStack = []
            , pendingCall = Nothing
            , stopped = False
            , internalError = Nothing
            }

setPc :: Int -> State (MachineState (IoMem (Isa w w) w) w) ()
setPc addr = modify $ \st -> st{pc = addr}

nextPc :: Isa w w -> State (MachineState (IoMem (Isa w w) w) w) ()
nextPc instruction = do
    State{pc} <- get
    setPc (pc + byteSize instruction)

raiseInternalError :: Text -> State (MachineState (IoMem (Isa w w) w) w) ()
raiseInternalError msg = modify $ \st -> st{internalError = Just msg}

pushValue :: w -> State (MachineState (IoMem (Isa w w) w) w) ()
pushValue value = modify $ \st@State{operandStack} -> st{operandStack = value : operandStack}

popValue :: (MachineWord w) => State (MachineState (IoMem (Isa w w) w) w) w
popValue = do
    st@State{operandStack} <- get
    case operandStack of
        [] -> do
            raiseInternalError "operand stack underflow"
            return def
        (x : xs) -> do
            put st{operandStack = xs}
            return x

popValues :: (MachineWord w) => Int -> State (MachineState (IoMem (Isa w w) w) w) [w]
popValues n = reverse <$> replicateM n popValue

getWord :: (MachineWord w) => Int -> State (MachineState (IoMem (Isa w w) w) w) w
getWord addr = do
    st@State{mem} <- get
    case readWord mem addr of
        Right (mem', w) -> do
            put st{mem = mem'}
            return w
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
        Right (mem', b) -> do
            put st{mem = mem'}
            return b
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return 0

setByte :: (MachineWord w) => Int -> Word8 -> State (MachineState (IoMem (Isa w w) w) w) ()
setByte addr byte = do
    st@State{mem} <- get
    case writeByte mem addr byte of
        Right mem' -> put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err

currentFrame :: State (MachineState (IoMem (Isa w w) w) w) (Maybe (Frame w))
currentFrame = get <&> listToMaybe . frames

currentFrameDepth :: State (MachineState (IoMem (Isa w w) w) w) Int
currentFrameDepth = get <&> length . frames

lookupLocal :: (MachineWord w) => String -> State (MachineState (IoMem (Isa w w) w) w) w
lookupLocal name = do
    currentFrame >>= \case
        Nothing -> do
            raiseInternalError "no active function frame"
            return def
        Just Frame{frLocals} ->
            case lookupLocalValue name frLocals of
                Just value -> return value
                Nothing -> do
                    raiseInternalError $ "unknown local: " <> toText name
                    return def

setLocal :: String -> w -> State (MachineState (IoMem (Isa w w) w) w) ()
setLocal name value = do
    st@State{frames} <- get
    case frames of
        [] -> raiseInternalError "no active function frame"
        (frame@Frame{frLocals} : rest) ->
            if name `elem` map fst frLocals
                then put st{frames = frame{frLocals = map update frLocals} : rest}
                else raiseInternalError $ "unknown local: " <> toText name
    where
        update (n, old)
            | n == name = (n, value)
            | otherwise = (n, old)

enterFunction :: (MachineWord w) => Isa w w -> State (MachineState (IoMem (Isa w w) w) w) ()
enterFunction instruction@Func{funcParams, funcLocals, funcResults} = do
    st@State{frames, pendingCall} <- get
    case pendingCall of
        Just PendingCall{pcReturnPc, pcArgs}
            | length pcArgs == length funcParams -> do
                let paramLocals = zip funcParams pcArgs
                    extraLocals = map (,def) funcLocals
                    frame = Frame{frReturnPc = pcReturnPc, frLocals = paramLocals <> extraLocals, frResults = funcResults}
                put st{frames = frame : frames, pendingCall = Nothing}
                nextPc instruction
            | otherwise ->
                raiseInternalError $
                    "function expects "
                        <> show (length funcParams)
                        <> " arguments, got "
                        <> show (length pcArgs)
        Nothing
            | null frames && null funcParams -> do
                let frame = Frame{frReturnPc = Nothing, frLocals = map (,def) funcLocals, frResults = funcResults}
                put st{frames = [frame]}
                nextPc instruction
            | null frames -> raiseInternalError "entry function cannot have parameters"
            | otherwise -> raiseInternalError "entered function without call"
enterFunction _ = raiseInternalError "internal error: expected function metadata"

returnFromFunction :: (MachineWord w) => State (MachineState (IoMem (Isa w w) w) w) ()
returnFromFunction = do
    State{frames, controlStack} <- get
    case frames of
        [] -> raiseInternalError "return without active function frame"
        (Frame{frReturnPc, frResults} : callerFrames) -> do
            results <- popValues frResults
            let depth = length frames
                controlStack' = filter ((< depth) . cfFrameDepth) controlStack
            modify $ \st' -> st'{frames = callerFrames, controlStack = controlStack'}
            mapM_ pushValue results
            case frReturnPc of
                Just returnPc -> setPc returnPc
                Nothing -> modify $ \st' -> st'{stopped = True}

callFunction :: (MachineWord w) => w -> State (MachineState (IoMem (Isa w w) w) w) ()
callFunction target = do
    State{pc, mem} <- get
    case readInstruction mem (fromEnum target) of
        Right Func{funcParams} -> do
            args <- popValues (length funcParams)
            st' <- get
            put st'{pendingCall = Just PendingCall{pcReturnPc = Just (pc + byteSize (Call target)), pcArgs = args}}
            setPc (fromEnum target)
        Right _ -> raiseInternalError "call target does not point to .func"
        Left err -> raiseInternalError $ "call target error: " <> err

findEndPc :: (MachineWord w) => IoMem (Isa w w) w -> Int -> Either Text Int
findEndPc memory start = go start (0 :: Int)
    where
        go addr depth = do
            instruction <- readInstruction memory addr
            let next = addr + byteSize instruction
            case instruction of
                Block{} -> go next (depth + 1)
                Loop{} -> go next (depth + 1)
                If{} -> go next (depth + 1)
                End
                    | depth == 0 -> Right addr
                    | otherwise -> go next (depth - 1)
                _ -> go next depth

findIfTargets :: (MachineWord w) => IoMem (Isa w w) w -> Int -> Either Text (Maybe Int, Int)
findIfTargets memory start = go start (0 :: Int) Nothing
    where
        go addr depth elsePc = do
            instruction <- readInstruction memory addr
            let next = addr + byteSize instruction
            case instruction of
                Block{} -> go next (depth + 1) elsePc
                Loop{} -> go next (depth + 1) elsePc
                If{} -> go next (depth + 1) elsePc
                Else
                    | depth == 0 -> go next depth (Just addr)
                    | otherwise -> go next depth elsePc
                End
                    | depth == 0 -> Right (elsePc, addr)
                    | otherwise -> go next (depth - 1) elsePc
                _ -> go next depth elsePc

pushControlFrame :: String -> ControlKind -> Int -> Int -> State (MachineState (IoMem (Isa w w) w) w) ()
pushControlFrame label kind startPc endPc = do
    depth <- currentFrameDepth
    modify $ \st@State{controlStack} ->
        st{controlStack = ControlFrame label kind startPc endPc depth : controlStack}

branchTo :: String -> State (MachineState (IoMem (Isa w w) w) w) ()
branchTo label = do
    st@State{controlStack} <- get
    depth <- currentFrameDepth
    let (_above, rest) = break (\cf -> cfFrameDepth cf == depth && cfLabel cf == label) controlStack
    case rest of
        [] -> raiseInternalError $ "unknown control label: " <> toText label
        (target@ControlFrame{cfKind, cfStartPc, cfEndPc} : outer) ->
            case cfKind of
                ControlLoop -> put st{controlStack = target : outer} >> setPc cfStartPc
                ControlBlock -> put st{controlStack = outer} >> setPc (cfEndPc + byteSize End)
                ControlIf -> put st{controlStack = outer} >> setPc (cfEndPc + byteSize End)

popControlEnd :: Isa w w -> State (MachineState (IoMem (Isa w w) w) w) ()
popControlEnd instruction = do
    st@State{controlStack} <- get
    depth <- currentFrameDepth
    case controlStack of
        (cf : rest) | cfFrameDepth cf == depth -> put st{controlStack = rest} >> nextPc instruction
        _ -> raiseInternalError "unexpected end"

executeElse :: State (MachineState (IoMem (Isa w w) w) w) ()
executeElse = do
    st@State{controlStack} <- get
    depth <- currentFrameDepth
    case controlStack of
        (ControlFrame{cfKind = ControlIf, cfFrameDepth, cfEndPc} : rest)
            | cfFrameDepth == depth -> put st{controlStack = rest} >> setPc (cfEndPc + byteSize End)
        _ -> raiseInternalError "unexpected else"

instance (MachineWord w) => StateInterspector (MachineState (IoMem (Isa w w) w) w) (IoMem (Isa w w) w) (Isa w w) w where
    programCounter State{pc} = pc
    memoryDump State{mem} = mem
    ioStreams State{mem = IoMem{mIoStreams}} = mIoStreams
    reprState labels st v
        | Just v' <- defaultView labels st v = v'
    reprState labels st@State{operandStack, frames, controlStack} v =
        case T.splitOn ":" v of
            ["stack", f] -> stack f operandStack
            ["locals", f] -> localsView f (listToMaybe frames)
            ["local", name, f] -> localView f name (listToMaybe frames)
            ["frames"] -> show (length frames)
            ["ctrl"] -> T.intercalate ":" $ map (toText . cfLabel) controlStack
            [r] -> reprState labels st (r <> ":dec")
            [r, _] -> unknownView r
            _ -> errorView v
        where
            stack "dec" values = toText $ intercalate ":" $ map show values
            stack "hex" values = T.intercalate ":" $ map (toText . word32ToHex) values
            stack f _ = unknownFormat f
            localsView _ Nothing = ""
            localsView f (Just Frame{frLocals}) = T.intercalate ":" $ map (\(n, value) -> toText n <> "=" <> viewRegister f value) frLocals
            localView _ _ Nothing = ""
            localView f name (Just Frame{frLocals}) =
                maybe (unknownView name) (viewRegister f) (lookupLocalValue (toString name) frLocals)

lookupLocalValue :: String -> [(String, w)] -> Maybe w
lookupLocalValue name = fmap snd . find ((== name) . fst)

instance (MachineWord w) => Machine (MachineState (IoMem (Isa w w) w) w) (Isa w w) w where
    instructionFetch =
        get
            <&> ( \case
                    State{stopped = True} -> Left halted
                    State{internalError = Just err} -> Left err
                    State{pc, mem} -> do
                        instruction <- readInstruction mem pc
                        return (pc, instruction)
                )

    instructionExecute _pc instruction =
        case instruction of
            Func{} -> enterFunction instruction
            EndFunc -> returnFromFunction
            I32Const value -> pushValue value >> nextPc instruction
            Drop -> popValue >> nextPc instruction
            Select -> do
                condition <- popValue
                falseValue <- popValue
                trueValue <- popValue
                pushValue $ if condition /= 0 then trueValue else falseValue
                nextPc instruction
            LocalGet name -> lookupLocal name >>= pushValue >> nextPc instruction
            LocalSet name -> popValue >>= setLocal name >> nextPc instruction
            LocalTee name -> do
                value <- popValue
                setLocal name value
                pushValue value
                nextPc instruction
            I32Add -> binary id id (+) >> nextPc instruction
            I32Sub -> binary id id (-) >> nextPc instruction
            I32Mul -> binary id id (*) >> nextPc instruction
            I32DivS -> signedDiv div >> nextPc instruction
            I32DivU -> unsignedDiv div >> nextPc instruction
            I32RemS -> signedDiv rem >> nextPc instruction
            I32RemU -> unsignedDiv rem >> nextPc instruction
            I32And -> binary id id (.&.) >> nextPc instruction
            I32Or -> binary id id (.|.) >> nextPc instruction
            I32Xor -> binary id id xor >> nextPc instruction
            I32Shl -> binary id id (\x y -> x `shiftL` (fromEnum y .&. 0x1F)) >> nextPc instruction
            I32ShrS -> binary id id (\x y -> x `shiftR` (fromEnum y .&. 0x1F)) >> nextPc instruction
            I32ShrU -> binary fromSign id (\x y -> toSign $ x `shiftR` (fromEnum y .&. 0x1F)) >> nextPc instruction
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
            I32Load -> popValue >>= getWord . fromEnum >>= pushValue >> nextPc instruction
            I32Store -> do
                value <- popValue
                addr <- popValue
                setWord (fromEnum addr) value
                nextPc instruction
            I32Load8S -> do
                addr <- popValue
                byte <- getByte (fromEnum addr)
                pushValue (fromIntegral (fromIntegral byte :: Int8))
                nextPc instruction
            I32Load8U -> do
                addr <- popValue
                byte <- getByte (fromEnum addr)
                pushValue (fromIntegral byte)
                nextPc instruction
            I32Store8 -> do
                value <- popValue
                addr <- popValue
                setByte (fromEnum addr) (fromIntegral value)
                nextPc instruction
            Block label -> do
                State{pc, mem} <- get
                case findEndPc mem (pc + byteSize instruction) of
                    Right endPc -> pushControlFrame label ControlBlock (pc + byteSize instruction) endPc >> nextPc instruction
                    Left err -> raiseInternalError $ "control flow error: " <> err
            Loop label -> do
                State{pc, mem} <- get
                case findEndPc mem (pc + byteSize instruction) of
                    Right endPc -> pushControlFrame label ControlLoop (pc + byteSize instruction) endPc >> nextPc instruction
                    Left err -> raiseInternalError $ "control flow error: " <> err
            If label -> do
                condition <- popValue
                State{pc, mem} <- get
                case findIfTargets mem (pc + byteSize instruction) of
                    Right (elsePc, endPc)
                        | condition /= 0 -> pushControlFrame label ControlIf (pc + byteSize instruction) endPc >> nextPc instruction
                        | Just elseAddr <- elsePc -> pushControlFrame label ControlIf (elseAddr + byteSize Else) endPc >> setPc (elseAddr + byteSize Else)
                        | otherwise -> setPc (endPc + byteSize End)
                    Left err -> raiseInternalError $ "control flow error: " <> err
            Else -> executeElse
            End -> popControlEnd instruction
            Br label -> branchTo label
            BrIf label -> do
                condition <- popValue
                if condition /= 0 then branchTo label else nextPc instruction
            Call target -> callFunction target
            Return -> returnFromFunction
            Halt -> modify $ \st -> st{stopped = True}
            Unreachable -> raiseInternalError "unreachable"
            Nop -> nextPc instruction
        where
            unary f = popValue >>= pushValue . f
            binary f1 f2 op = do
                y <- f2 <$> popValue
                x <- f1 <$> popValue
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
