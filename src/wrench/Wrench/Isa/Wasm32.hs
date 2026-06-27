{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | A small WebAssembly-inspired 32-bit virtual ISA for Wrench.
module Wrench.Isa.Wasm32 (
    Isa (..),
    Source (..),
    FunctionMeta (..),
    FunctionTable,
    MachineState (..),
    Wasm32State,
    initWasm32State,
    translateWasm32,
) where

import Control.Monad (foldM)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Default (def)
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Relude
import Relude.Unsafe qualified as Unsafe
import Text.Megaparsec (anySingle, choice, parse, try)
import Text.Megaparsec.Char (char, hspace, hspace1, string)
import Text.Megaparsec.Error (errorBundlePretty)
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Report
import Wrench.Translator
import Wrench.Translator.Parser
import Wrench.Translator.Parser.Misc
import Wrench.Translator.Parser.Types
import Wrench.Translator.Types

data Isa w l
    = I32Const l
    | Drop
    | Select
    | LocalGet Int
    | LocalSet Int
    | LocalTee Int
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
    | Block Int
    | Loop Int
    | If Int
    | Else
    | End
    | Br Int
    | BrIf Int
    | Call l
    | Return
    | Halt
    | Unreachable
    | Nop
    deriving (Show)

data Source w l
    = SourceFunc {sourceParams :: [String], sourceLocals :: [String], sourceResults :: Int}
    | SourceEndFunc
    | SourceI32Const l
    | SourceDrop
    | SourceSelect
    | SourceLocalGet String
    | SourceLocalSet String
    | SourceLocalTee String
    | SourceI32Add
    | SourceI32Sub
    | SourceI32Mul
    | SourceI32DivS
    | SourceI32DivU
    | SourceI32RemS
    | SourceI32RemU
    | SourceI32And
    | SourceI32Or
    | SourceI32Xor
    | SourceI32Shl
    | SourceI32ShrS
    | SourceI32ShrU
    | SourceI32Eqz
    | SourceI32Eq
    | SourceI32Ne
    | SourceI32LtS
    | SourceI32LeS
    | SourceI32GtS
    | SourceI32GeS
    | SourceI32LtU
    | SourceI32LeU
    | SourceI32GtU
    | SourceI32GeU
    | SourceI32Load
    | SourceI32Store
    | SourceI32Load8S
    | SourceI32Load8U
    | SourceI32Store8
    | SourceBlock String
    | SourceLoop String
    | SourceIf String
    | SourceElse
    | SourceEnd
    | SourceBr String
    | SourceBrIf String
    | SourceCall l
    | SourceReturn
    | SourceHalt
    | SourceUnreachable
    | SourceNop
    deriving (Show)

data FunctionMeta = FunctionMeta
    { fmParamCount :: !Int
    , fmLocalNames :: ![String]
    , fmResultCount :: !Int
    }
    deriving (Eq, Show)

type FunctionTable = IntMap FunctionMeta

instance CommentStart (Isa w l) where
    commentStart = ";"

instance CommentStart (Source w l) where
    commentStart = ";"

instance (MachineWord w) => MnemonicParser (Source w (Ref w)) where
    mnemonic =
        hspace *> cmd <* eol' (commentStart @(Source _ _))
        where
            cmd =
                choice
                    [ try func
                    , try endFunc
                    , SourceI32Const <$> cmd1 "i32.const" referenceWithDirective
                    , cmd0 "drop" SourceDrop
                    , cmd0 "select" SourceSelect
                    , SourceLocalGet <$> cmd1 "local.get" localId
                    , SourceLocalSet <$> cmd1 "local.set" localId
                    , SourceLocalTee <$> cmd1 "local.tee" localId
                    , cmd0 "i32.add" SourceI32Add
                    , cmd0 "i32.sub" SourceI32Sub
                    , cmd0 "i32.mul" SourceI32Mul
                    , cmd0 "i32.div_s" SourceI32DivS
                    , cmd0 "i32.div_u" SourceI32DivU
                    , cmd0 "i32.rem_s" SourceI32RemS
                    , cmd0 "i32.rem_u" SourceI32RemU
                    , cmd0 "i32.and" SourceI32And
                    , cmd0 "i32.or" SourceI32Or
                    , cmd0 "i32.xor" SourceI32Xor
                    , cmd0 "i32.shl" SourceI32Shl
                    , cmd0 "i32.shr_s" SourceI32ShrS
                    , cmd0 "i32.shr_u" SourceI32ShrU
                    , cmd0 "i32.eqz" SourceI32Eqz
                    , cmd0 "i32.eq" SourceI32Eq
                    , cmd0 "i32.ne" SourceI32Ne
                    , cmd0 "i32.lt_s" SourceI32LtS
                    , cmd0 "i32.le_s" SourceI32LeS
                    , cmd0 "i32.gt_s" SourceI32GtS
                    , cmd0 "i32.ge_s" SourceI32GeS
                    , cmd0 "i32.lt_u" SourceI32LtU
                    , cmd0 "i32.le_u" SourceI32LeU
                    , cmd0 "i32.gt_u" SourceI32GtU
                    , cmd0 "i32.ge_u" SourceI32GeU
                    , cmd0 "i32.load8_s" SourceI32Load8S
                    , cmd0 "i32.load8_u" SourceI32Load8U
                    , cmd0 "i32.load" SourceI32Load
                    , cmd0 "i32.store8" SourceI32Store8
                    , cmd0 "i32.store" SourceI32Store
                    , SourceBlock <$> cmd1 "block" controlLabel
                    , SourceLoop <$> cmd1 "loop" controlLabel
                    , SourceIf <$> cmd1 "if" controlLabel
                    , cmd0 "else" SourceElse
                    , cmd0 "end" SourceEnd
                    , SourceBrIf <$> cmd1 "br_if" controlLabel
                    , SourceBr <$> cmd1 "br" controlLabel
                    , SourceCall <$> cmd1 "call" reference
                    , cmd0 "return" SourceReturn
                    , cmd0 "halt" SourceHalt
                    , cmd0 "unreachable" SourceUnreachable
                    , cmd0 "nop" SourceNop
                    ]

func :: Parser (Source w (Ref w))
func = try $ do
    optionalDot
    void $ string "func"
    hspace
    choice [try numericFunc, try keywordFunc, return SourceFunc{sourceParams = [], sourceLocals = [], sourceResults = 0}]

endFunc :: Parser (Source w (Ref w))
endFunc = try $ do
    optionalDot
    void $ string "endfunc"
    return SourceEndFunc

numericFunc :: Parser (Source w (Ref w))
numericFunc = do
    params <- number
    comma
    locals <- number
    comma
    results <- number
    return
        SourceFunc
            { sourceParams = map show [0 .. params - 1]
            , sourceLocals = map show [params .. params + locals - 1]
            , sourceResults = results
            }

keywordFunc :: Parser (Source w (Ref w))
keywordFunc = buildFunc <$> some funcWord

buildFunc :: [String] -> Source w l
buildFunc tokens =
    let params = collectAfter "params" ["locals", "result", "results"] tokens
        locals = collectAfter "locals" ["params", "result", "results"] tokens
        results =
            case dropWhile (/= "result") tokens of
                ("result" : "i32" : _) -> 1
                ("result" : "none" : _) -> 0
                ("result" : n : _) -> fromMaybe (error $ "invalid result count: " <> toText n) (readMaybe n)
                _ -> case dropWhile (/= "results") tokens of
                    ("results" : n : _) -> fromMaybe (error $ "invalid result count: " <> toText n) (readMaybe n)
                    _ -> 0
     in SourceFunc{sourceParams = params, sourceLocals = locals, sourceResults = results}

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
    byteSize Block{} = 2
    byteSize Loop{} = 2
    byteSize If{} = 2
    byteSize Br{} = 2
    byteSize BrIf{} = 2
    byteSize _ = 1

instance ByteSize (Source w l) where
    byteSize SourceFunc{} = 0
    byteSize SourceEndFunc = 1
    byteSize SourceI32Const{} = 5
    byteSize SourceCall{} = 5
    byteSize SourceLocalGet{} = 2
    byteSize SourceLocalSet{} = 2
    byteSize SourceLocalTee{} = 2
    byteSize SourceBlock{} = 2
    byteSize SourceLoop{} = 2
    byteSize SourceIf{} = 2
    byteSize SourceBr{} = 2
    byteSize SourceBrIf{} = 2
    byteSize _ = 1

translateWasm32 ::
    forall w.
    (MachineWord w) =>
    Int
    -> FilePath
    -> String
    -> Either Text (TranslatorResult (Mem (Isa w w) w) w, FunctionTable)
translateWasm32 memorySize fn src =
    case parse asmParser fn src of
        Left err -> Left $ toText $ errorBundlePretty err
        Right sections -> do
            labels <- firstToText $ evaluateLabels sections
            let resolveLabel l = HashMap.lookup l labels
                marked = markupSectionOffsets 0 sections
            functionTable <- collectFunctions marked
            code <- lowerSections resolveLabel functionTable marked
            let stats = computeDumpStats code
            dump <- prepareDump memorySize code
            Right (TranslatorResult dump labels stats, functionTable)

firstToText :: Either String a -> Either Text a
firstToText = either (Left . toText) Right

collectFunctions ::
    (MachineWord w) =>
    [(w, Section (Source w (Ref w)) w String)]
    -> Either Text FunctionTable
collectFunctions sections = snd <$> foldM collectSection (Nothing, IntMap.empty) sections
    where
        collectSection (active, table) (_, Data{})
            | isJust active = Left "data section inside .func"
            | otherwise = Right (active, table)
        collectSection (active, table) (offset, Code{codeTokens}) = foldM collectToken (active, table, offset) codeTokens <&> \(active', table', _) -> (active', table')

        collectToken (active, table, offset) (Label _) = Right (active, table, offset)
        collectToken (active, table, offset) (Mnemonic instr) =
            let next = offset + toEnum (byteSize instr)
             in case instr of
                    SourceFunc{} -> do
                        when (isJust active) $ Left ".func before .endfunc"
                        meta <- functionMeta instr
                        let addr = fromEnum offset
                        when (IntMap.member addr table) $ Left $ "duplicate function metadata at address " <> show addr
                        Right (Just meta, IntMap.insert addr meta table, next)
                    SourceEndFunc -> do
                        when (isNothing active) $ Left ".endfunc without .func"
                        Right (Nothing, table, next)
                    _ -> Right (active, table, next)

functionMeta :: Source w l -> Either Text FunctionMeta
functionMeta SourceFunc{sourceParams, sourceLocals, sourceResults} = do
    let names = sourceParams <> sourceLocals
    case firstDuplicate names of
        Just name -> Left $ "duplicate local name: " <> toText name
        Nothing ->
            Right
                FunctionMeta
                    { fmParamCount = length sourceParams
                    , fmLocalNames = names
                    , fmResultCount = sourceResults
                    }
functionMeta _ = Left "internal error: expected .func"

firstDuplicate :: (Eq a) => [a] -> Maybe a
firstDuplicate [] = Nothing
firstDuplicate (x : xs)
    | x `elem` xs = Just x
    | otherwise = firstDuplicate xs

newtype LowerState = LowerState
    { lsFunction :: Maybe FunctionCtx
    }
    deriving (Show)

data FunctionCtx = FunctionCtx
    { fcLocals :: ![(String, Int)]
    , fcControls :: ![SourceControl]
    , fcNextControlId :: !Int
    }
    deriving (Show)

data SourceControl = SourceControl
    { scName :: !String
    , scId :: !Int
    , scKind :: !ControlKind
    , scSeenElse :: !Bool
    }
    deriving (Show)

lowerSections ::
    (MachineWord w) =>
    (String -> Maybe w)
    -> FunctionTable
    -> [(w, Section (Source w (Ref w)) w String)]
    -> Either Text [Section (Isa w w) w w]
lowerSections resolveLabel functions sections = do
    (st, lowered) <- foldM lowerSection (LowerState Nothing, []) sections
    when (isJust $ lsFunction st) $ Left "unclosed .func"
    return $ reverse lowered
    where
        lowerSection (st@LowerState{lsFunction}, acc) (_, Data{org, dataTokens}) = do
            when (isJust lsFunction) $ Left "data section inside .func"
            dataTokens' <- traverse lowerDataToken dataTokens
            return (st, Data org dataTokens' : acc)
        lowerSection (st, acc) (offset, Code{org, codeTokens}) = do
            (st', _, codeTokens') <- foldM lowerCodeToken (st, offset, []) codeTokens
            return (st', Code org (reverse codeTokens') : acc)

        lowerDataToken DataToken{dtLabel, dtValue} =
            case resolveLabel dtLabel of
                Just label -> Right DataToken{dtLabel = label, dtValue}
                Nothing -> Left $ "unknown label: " <> toText dtLabel

        lowerCodeToken (st, offset, acc) (Label _) = Right (st, offset, acc)
        lowerCodeToken (st, offset, acc) (Mnemonic source) = do
            (st', instruction) <- lowerSource resolveLabel functions (fromEnum offset) st source
            let offset' = offset + toEnum (byteSize source)
                acc' = maybe acc ((: acc) . Mnemonic) instruction
            return (st', offset', acc')

lowerSource ::
    (MachineWord w) =>
    (String -> Maybe w)
    -> FunctionTable
    -> Int
    -> LowerState
    -> Source w (Ref w)
    -> Either Text (LowerState, Maybe (Isa w w))
lowerSource resolveLabel functions addr st source =
    case source of
        SourceFunc{} -> do
            when (isJust $ lsFunction st) $ Left ".func before .endfunc"
            meta <- maybeToRight ("missing function metadata at address " <> show addr) (IntMap.lookup addr functions)
            let locals = zip (fmLocalNames meta) [0 ..]
            return (st{lsFunction = Just FunctionCtx{fcLocals = locals, fcControls = [], fcNextControlId = 0}}, Nothing)
        SourceEndFunc -> do
            ctx <- requireFunction st ".endfunc"
            case fcControls ctx of
                [] -> return (st{lsFunction = Nothing}, Just Return)
                control : _ -> Left $ "unclosed structured control label: " <> toText (scName control)
        SourceI32Const value -> executable st $ I32Const <$> resolveRef resolveLabel value
        SourceDrop -> executable st $ Right Drop
        SourceSelect -> executable st $ Right Select
        SourceLocalGet name -> executableLocal st name LocalGet
        SourceLocalSet name -> executableLocal st name LocalSet
        SourceLocalTee name -> executableLocal st name LocalTee
        SourceI32Add -> executable st $ Right I32Add
        SourceI32Sub -> executable st $ Right I32Sub
        SourceI32Mul -> executable st $ Right I32Mul
        SourceI32DivS -> executable st $ Right I32DivS
        SourceI32DivU -> executable st $ Right I32DivU
        SourceI32RemS -> executable st $ Right I32RemS
        SourceI32RemU -> executable st $ Right I32RemU
        SourceI32And -> executable st $ Right I32And
        SourceI32Or -> executable st $ Right I32Or
        SourceI32Xor -> executable st $ Right I32Xor
        SourceI32Shl -> executable st $ Right I32Shl
        SourceI32ShrS -> executable st $ Right I32ShrS
        SourceI32ShrU -> executable st $ Right I32ShrU
        SourceI32Eqz -> executable st $ Right I32Eqz
        SourceI32Eq -> executable st $ Right I32Eq
        SourceI32Ne -> executable st $ Right I32Ne
        SourceI32LtS -> executable st $ Right I32LtS
        SourceI32LeS -> executable st $ Right I32LeS
        SourceI32GtS -> executable st $ Right I32GtS
        SourceI32GeS -> executable st $ Right I32GeS
        SourceI32LtU -> executable st $ Right I32LtU
        SourceI32LeU -> executable st $ Right I32LeU
        SourceI32GtU -> executable st $ Right I32GtU
        SourceI32GeU -> executable st $ Right I32GeU
        SourceI32Load -> executable st $ Right I32Load
        SourceI32Store -> executable st $ Right I32Store
        SourceI32Load8S -> executable st $ Right I32Load8S
        SourceI32Load8U -> executable st $ Right I32Load8U
        SourceI32Store8 -> executable st $ Right I32Store8
        SourceBlock label -> executableControl st label ControlBlock Block
        SourceLoop label -> executableControl st label ControlLoop Loop
        SourceIf label -> executableControl st label ControlIf If
        SourceElse -> lowerElse st
        SourceEnd -> lowerEnd st
        SourceBr label -> executableBranch st label Br
        SourceBrIf label -> executableBranch st label BrIf
        SourceCall targetRef -> do
            target <- resolveRef resolveLabel targetRef
            unless (IntMap.member (fromEnum target) functions) $ Left $ "call target does not point to .func: " <> show target
            executable st $ Right $ Call target
        SourceReturn -> executable st $ Right Return
        SourceHalt -> executable st $ Right Halt
        SourceUnreachable -> executable st $ Right Unreachable
        SourceNop -> executable st $ Right Nop

requireFunction :: LowerState -> Text -> Either Text FunctionCtx
requireFunction LowerState{lsFunction = Just ctx} _ = Right ctx
requireFunction LowerState{lsFunction = Nothing} source = Left $ source <> " outside .func"

setFunction :: LowerState -> FunctionCtx -> LowerState
setFunction st ctx = st{lsFunction = Just ctx}

executable :: LowerState -> Either Text (Isa w w) -> Either Text (LowerState, Maybe (Isa w w))
executable st instruction = do
    void $ requireFunction st "instruction"
    (st,) . Just <$> instruction

executableLocal :: LowerState -> String -> (Int -> Isa w w) -> Either Text (LowerState, Maybe (Isa w w))
executableLocal st name constructor = do
    ctx <- requireFunction st "local instruction"
    index <- maybeToRight ("unknown local: " <> toText name) (lookupAssoc name $ fcLocals ctx)
    return (st, Just $ constructor index)

executableControl ::
    LowerState -> String -> ControlKind -> (Int -> Isa w w) -> Either Text (LowerState, Maybe (Isa w w))
executableControl st label kind constructor = do
    ctx <- requireFunction st "control instruction"
    let controlId = fcNextControlId ctx
        control = SourceControl{scName = label, scId = controlId, scKind = kind, scSeenElse = False}
        ctx' = ctx{fcControls = control : fcControls ctx, fcNextControlId = controlId + 1}
    return (setFunction st ctx', Just $ constructor controlId)

executableBranch :: LowerState -> String -> (Int -> Isa w w) -> Either Text (LowerState, Maybe (Isa w w))
executableBranch st label constructor = do
    ctx <- requireFunction st "branch instruction"
    control <- maybeToRight ("unknown control label: " <> toText label) (find ((== label) . scName) $ fcControls ctx)
    return (st, Just $ constructor $ scId control)

lowerElse :: LowerState -> Either Text (LowerState, Maybe (Isa w w))
lowerElse st = do
    ctx <- requireFunction st "else"
    case fcControls ctx of
        (control@SourceControl{scKind = ControlIf, scSeenElse = False} : rest) ->
            let ctx' = ctx{fcControls = control{scSeenElse = True} : rest}
             in return (setFunction st ctx', Just Else)
        (SourceControl{scKind = ControlIf} : _) -> Left "duplicate else"
        _ -> Left "else without active if"

lowerEnd :: LowerState -> Either Text (LowerState, Maybe (Isa w w))
lowerEnd st = do
    ctx <- requireFunction st "end"
    case fcControls ctx of
        [] -> Left "unexpected end"
        (_control : rest) -> return (setFunction st ctx{fcControls = rest}, Just End)

resolveRef :: (String -> Maybe w) -> Ref w -> Either Text w
resolveRef resolveLabel = \case
    ValueR prepare value -> Right $! prepare value
    Ref prepare label -> case resolveLabel label of
        Just value -> Right $! prepare value
        Nothing -> Left $ "Can't resolve label: " <> toText label

type Wasm32State w = MachineState (IoMem (Isa w w) w) w

data ControlKind = ControlBlock | ControlLoop | ControlIf
    deriving (Eq, Show)

data ControlFrame = ControlFrame
    { cfLabel :: Int
    , cfKind :: ControlKind
    , cfStartPc :: Int
    , cfEndPc :: Int
    , cfFrameDepth :: Int
    }
    deriving (Show)

data Frame w = Frame
    { frReturnPc :: Maybe Int
    , frLocals :: [w]
    , frLocalNames :: [String]
    , frResults :: Int
    }
    deriving (Show)

data MachineState mem w = State
    { pc :: Int
    , mem :: mem
    , operandStack :: [w]
    , operandStackMax :: !Int
    , frames :: [Frame w]
    , framesMax :: !Int
    , controlStack :: [ControlFrame]
    , controlStackMax :: !Int
    , functions :: !FunctionTable
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
            , operandStackMax = 0
            , frames = []
            , framesMax = 0
            , controlStack = []
            , controlStackMax = 0
            , functions = IntMap.empty
            , stopped = False
            , internalError = Nothing
            }

initWasm32State ::
    (MachineWord w) =>
    Int
    -> IoMem (Isa w w) w
    -> FunctionTable
    -> Either Text (Wasm32State w)
initWasm32State entryPc dump functionTable =
    case IntMap.lookup entryPc functionTable of
        Nothing -> Left "_start label should point to .func."
        Just meta
            | fmParamCount meta /= 0 -> Left "entry function cannot have parameters"
            | otherwise ->
                Right
                    State
                        { pc = entryPc
                        , mem = dump
                        , operandStack = []
                        , operandStackMax = 0
                        , frames = [emptyFrame Nothing meta]
                        , framesMax = 1
                        , controlStack = []
                        , controlStackMax = 0
                        , functions = functionTable
                        , stopped = False
                        , internalError = Nothing
                        }

emptyFrame :: (MachineWord w) => Maybe Int -> FunctionMeta -> Frame w
emptyFrame returnPc meta =
    Frame
        { frReturnPc = returnPc
        , frLocals = replicate (length $ fmLocalNames meta) def
        , frLocalNames = fmLocalNames meta
        , frResults = fmResultCount meta
        }

calledFrame :: (MachineWord w) => Maybe Int -> FunctionMeta -> [w] -> Either Text (Frame w)
calledFrame returnPc meta args
    | length args /= fmParamCount meta =
        Left
            $ "function expects "
            <> show (fmParamCount meta)
            <> " arguments, got "
            <> show (length args)
    | otherwise =
        Right
            Frame
                { frReturnPc = returnPc
                , frLocals = args <> replicate (length (fmLocalNames meta) - fmParamCount meta) def
                , frLocalNames = fmLocalNames meta
                , frResults = fmResultCount meta
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
pushValue value =
    modify $ \st@State{operandStack, operandStackMax} ->
        let operandStack' = value : operandStack
         in st{operandStack = operandStack', operandStackMax = max operandStackMax (length operandStack')}

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

lookupLocal :: (MachineWord w) => Int -> State (MachineState (IoMem (Isa w w) w) w) w
lookupLocal index = do
    currentFrame >>= \case
        Nothing -> do
            raiseInternalError "no active function frame"
            return def
        Just Frame{frLocals} ->
            case lookupLocalValue index frLocals of
                Just value -> return value
                Nothing -> do
                    raiseInternalError $ "unknown local index: " <> show index
                    return def

setLocal :: Int -> w -> State (MachineState (IoMem (Isa w w) w) w) ()
setLocal index value = do
    st@State{frames} <- get
    case frames of
        [] -> raiseInternalError "no active function frame"
        (frame@Frame{frLocals} : rest) ->
            case replaceAt index value frLocals of
                Just frLocals' -> put st{frames = frame{frLocals = frLocals'} : rest}
                Nothing -> raiseInternalError $ "unknown local index: " <> show index

lookupLocalValue :: Int -> [w] -> Maybe w
lookupLocalValue index values
    | index < 0 = Nothing
    | otherwise = listToMaybe $ drop index values

replaceAt :: Int -> a -> [a] -> Maybe [a]
replaceAt index value values
    | index < 0 = Nothing
    | otherwise = go index values
    where
        go _ [] = Nothing
        go 0 (_ : rest) = Just (value : rest)
        go n (x : rest) = (x :) <$> go (n - 1) rest

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
    State{pc, functions} <- get
    case IntMap.lookup (fromEnum target) functions of
        Nothing -> raiseInternalError "call target does not point to .func"
        Just meta -> do
            args <- popValues (fmParamCount meta)
            case calledFrame (Just $ pc + byteSize (Call target)) meta args of
                Left err -> raiseInternalError err
                Right frame -> do
                    st@State{frames} <- get
                    let frames' = frame : frames
                    put st{frames = frames', framesMax = max (framesMax st) (length frames')}
                    setPc (fromEnum target)

findEndPc :: (MachineWord w) => IoMem (Isa w w) w -> Int -> Either Text Int
findEndPc memory start = go start (0 :: Int)
    where
        go addr depth = do
            (_, instruction) <- readInstruction memory addr
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
            (_, instruction) <- readInstruction memory addr
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

pushControlFrame :: Int -> ControlKind -> Int -> Int -> State (MachineState (IoMem (Isa w w) w) w) ()
pushControlFrame label kind startPc endPc = do
    depth <- currentFrameDepth
    modify $ \st@State{controlStack, controlStackMax} ->
        let controlStack' = ControlFrame label kind startPc endPc depth : controlStack
         in st{controlStack = controlStack', controlStackMax = max controlStackMax (length controlStack')}

branchTo :: Int -> State (MachineState (IoMem (Isa w w) w) w) ()
branchTo label = do
    st@State{controlStack} <- get
    depth <- currentFrameDepth
    let (_above, rest) = break (\cf -> cfFrameDepth cf == depth && cfLabel cf == label) controlStack
    case rest of
        [] -> raiseInternalError $ "unknown control label: " <> show label
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
        (_cf : rest) | cfFrameDepth _cf == depth -> put st{controlStack = rest} >> nextPc instruction
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
            ["ctrl"] -> T.intercalate ":" $ map (show . cfLabel) controlStack
            [r] -> reprState labels st (r <> ":dec")
            [r, _] -> unknownView r
            _ -> errorView v
        where
            stack "dec" values = toText $ intercalate ":" $ map show values
            stack "hex" values = T.intercalate ":" $ map (toText . word32ToHex) values
            stack f _ = unknownFormat f
            localsView _ Nothing = ""
            localsView f (Just Frame{frLocals, frLocalNames}) =
                T.intercalate ":" $ zipWith (\n value -> toText n <> "=" <> viewRegister f value) frLocalNames frLocals
            localView _ _ Nothing = ""
            localView f name (Just frame@Frame{frLocalNames}) =
                case snd <$> find ((== toString name) . fst) (zip frLocalNames [0 :: Int ..]) of
                    Just index -> maybe (unknownView name) (viewRegister f) (frameLocal index frame)
                    Nothing -> case readMaybe (toString name) of
                        Just index -> maybe (unknownView name) (viewRegister f) (frameLocal index frame)
                        Nothing -> unknownView name

    summaryView _labels State{operandStackMax, framesMax, controlStackMax} v = case T.splitOn ":" v of
        ["wasm32", "operand-stack-max"] -> Just $ show operandStackMax
        ["wasm32", "frames-max"] -> Just $ show framesMax
        ["wasm32", "control-stack-max"] -> Just $ show controlStackMax
        ["isa-specific"] ->
            Just
                $ "wasm32:operand-stack-max: "
                <> show operandStackMax
                <> "\n"
                <> "wasm32:frames-max:        "
                <> show framesMax
                <> "\n"
                <> "wasm32:control-stack-max: "
                <> show controlStackMax
        _ -> Nothing

frameLocal :: Int -> Frame w -> Maybe w
frameLocal index Frame{frLocals} = lookupLocalValue index frLocals

lookupAssoc :: (Eq a) => a -> [(a, b)] -> Maybe b
lookupAssoc key = fmap snd . find ((== key) . fst)

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
                        | Just elseAddr <- elsePc ->
                            pushControlFrame label ControlIf (elseAddr + byteSize Else) endPc >> setPc (elseAddr + byteSize Else)
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
