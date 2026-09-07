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
    | -- | Function metadata, embedded in code right before the body: param
      -- count, declared (non-parameter) local count, result count. Emitted
      -- by lowering a source @.func@ directive; never reached by ordinary
      -- fallthrough, only read directly by 'Call'.
      FuncHeader Int Int Int
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
            FuncHeader p l r -> FuncHeader p l r

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
    byteSize FuncHeader{} = 4
    byteSize _ = 1

instance ByteSize (Source w l) where
    byteSize SourceFunc{} = 4
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
            labels <- evaluateLabels sections
            let resolveLabel l = HashMap.lookup l labels
                marked = markupSectionOffsets 0 sections
            functionTable <- collectFunctions marked
            code <- lowerSections resolveLabel functionTable marked
            let stats = computeDumpStats code
            dump <- prepareDump memorySize code
            Right (TranslatorResult dump labels stats, functionTable)

collectFunctions ::
    (MachineWord w) =>
    [(w, Section (Source w (Ref w)) w Text)]
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

-- | Which structured construct a still-open 'SourceControl' is, tracked
-- purely at lowering time (e.g. so 'lowerElse' can reject an `else`
-- outside an `if`). Unrelated to 'RecordKind', the runtime tag stored in a
-- control record.
data ControlKind = ControlBlock | ControlLoop | ControlIf
    deriving (Eq, Show)

data SourceControl = SourceControl
    { scName :: !String
    , scId :: !Int
    , scKind :: !ControlKind
    , scSeenElse :: !Bool
    }
    deriving (Show)

lowerSections ::
    (MachineWord w) =>
    (Text -> Maybe w)
    -> FunctionTable
    -> [(w, Section (Source w (Ref w)) w Text)]
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
    (Text -> Maybe w)
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
                declaredLocalCount = length (fmLocalNames meta) - fmParamCount meta
            return
                ( st{lsFunction = Just FunctionCtx{fcLocals = locals, fcControls = [], fcNextControlId = 0}}
                , Just $ FuncHeader (fmParamCount meta) declaredLocalCount (fmResultCount meta)
                )
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

resolveRef :: (Text -> Maybe w) -> Ref w -> Either Text w
resolveRef resolveLabel = \case
    ValueR prepare value -> Right $! prepare value
    Ref prepare label -> case resolveLabel label of
        Just value -> Right $! prepare value
        Nothing -> Left $ "Can't resolve label: " <> toText label

type Wasm32State w = MachineState (IoMem (Isa w w) w) w

-- | What a control record on the chain represents. Encoded as a plain word
-- in memory -- see 'recordKindOffset'.
data RecordKind = RecordCall | RecordBlock | RecordLoop | RecordIf
    deriving (Bounded, Enum, Eq, Show)

-- | The kind-specific payload passed to 'enter'. docs/wasm32.md describes
-- the persisted record as a C-style tagged union (untyped fields, valid
-- shape implied by `kind`, a convention the reader has to trust); here the
-- shape is the type itself, one constructor per 'RecordKind', so a
-- mismatch between a record's kind and its payload can't be constructed in
-- the first place -- 'recordKindOf' derives the kind from the value rather
-- than needing it passed alongside.
data RecordExtra
    = -- | Block: branch target id only.
      BlockExtra {reLabel :: Int}
    | -- | If: branch target id only.
      IfExtra {reLabel :: Int}
    | -- | Loop: branch target id, and the loop's re-entry point.
      LoopExtra {reLabel :: Int, reStartPc :: Int}
    | -- | Call: caller's frame_base to restore on return, and this call's
      -- own 'FuncHeader' address (report views only).
      CallExtra {reSavedFrameBase :: Int, reEntryPc :: Int}
    deriving (Show)

recordKindOf :: RecordExtra -> RecordKind
recordKindOf BlockExtra{} = RecordBlock
recordKindOf IfExtra{} = RecordIf
recordKindOf LoopExtra{} = RecordLoop
recordKindOf CallExtra{} = RecordCall

-- | The two generic word slots a 'RecordExtra' actually occupies in
-- memory (see 'recordField4Offset'/'recordField5Offset') -- the only place
-- that needs to know both the tagged Haskell shape and the untyped
-- on-disk layout at once.
extraRawFields :: RecordExtra -> (Int, Int)
extraRawFields (BlockExtra l) = (l, 0)
extraRawFields (IfExtra l) = (l, 0)
extraRawFields (LoopExtra l s) = (l, s)
extraRawFields (CallExtra fb ep) = (fb, ep)

-- | Width, in words, of one control record. See docs/wasm32.md's "Control
-- Records" section for the field layout this mirrors.
recordWidth :: Int
recordWidth = 6

recordLinkOffset, recordKindOffset, recordEndPcOffset, recordResultCountOffset :: Int
recordLinkOffset = 0
recordKindOffset = 1
recordEndPcOffset = 2
recordResultCountOffset = 3

-- | 'RecordExtra's first field: label (Block\/If\/Loop) or savedFrameBase
-- (Call).
recordField4Offset :: Int
recordField4Offset = 4

-- | 'RecordExtra's second field: startPc (Loop only) or entryPc (Call).
recordField5Offset :: Int
recordField5Offset = 5

-- | Sentinel for "no previous record" ('ctrlTop'\/a record's `link`), and
-- for a `Call` record's `endPc` meaning "this is `_start` -- halt, don't
-- jump to a return address."
nullAddr :: Int
nullAddr = -1

data MachineState mem w = State
    { pc :: Int
    -- ^ Program counter.
    , sp :: Int
    -- ^ Top of the one runtime stack (locals, operand values, and control
    -- records all live here -- see docs/wasm32.md's Execution Model).
    , frameBase :: Int
    -- ^ Base address of the active function's locals.
    , ctrlTop :: Int
    -- ^ Address of the innermost open block\/loop\/if\/call record, or
    -- 'nullAddr'.
    , mem :: mem
    , spMax :: !Int
    -- ^ High-water mark of 'sp' (locals + operand values + control
    -- records combined -- backs @wasm32:operand-stack-max@).
    , callDepth :: !Int
    , callDepthMax :: !Int
    -- ^ Live/high-water count of open `Call` records (backs @frames@\/
    -- @wasm32:frames-max@).
    , ctrlDepth :: !Int
    , ctrlDepthMax :: !Int
    -- ^ Live/high-water count of open `Block`\/`Loop`\/`If` records (backs
    -- @wasm32:control-stack-max@).
    , functions :: !FunctionTable
    -- ^ Kept only for report\/debug views (@locals@, @local:<name>@,
    -- @stack@) -- not consulted by instruction execution, which reads
    -- function metadata from the embedded 'FuncHeader' instead.
    , stopped :: Bool
    , internalError :: Maybe Text
    }
    deriving (Show)

instance InitState (IoMem (Isa w w) w) (MachineState (IoMem (Isa w w) w) w) where
    initState pc dump _randomStream =
        State
            { pc
            , sp = memTop dump
            , frameBase = memTop dump
            , ctrlTop = nullAddr
            , mem = dump
            , spMax = memTop dump
            , callDepth = 0
            , callDepthMax = 0
            , ctrlDepth = 0
            , ctrlDepthMax = 0
            , functions = IntMap.empty
            , stopped = False
            , internalError = Nothing
            }

-- | Where the runtime stack starts: the upper half of the configured
-- memory, code+data occupying the lower half.
--
-- TODO: make the stack's size/placement independently configurable
-- instead of this fixed split of the ISA's memory-size setting.
memTop :: IoMem (Isa w w) w -> Int
memTop IoMem{mIoCells = Mem{memorySize}} = memorySize `div` 2

-- | Width, in bytes, of the 'FuncHeader' instruction every function
-- starts with. Fixed regardless of the header's actual field values.
funcHeaderSize :: Int
funcHeaderSize = byteSize (FuncHeader 0 0 0 :: Isa w w)

initWasm32State ::
    forall w.
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
            | otherwise -> do
                let step = byteSizeT @w
                    base = memTop dump
                    localCount = length (fmLocalNames meta)
                    recordAddr = base + localCount * step
                    stackTop = recordAddr + recordWidth * step
                mem' <- writeRecordAt dump recordAddr nullAddr (CallExtra nullAddr entryPc) nullAddr 0
                Right
                    State
                        { pc = entryPc + funcHeaderSize
                        , sp = stackTop
                        , frameBase = base
                        , ctrlTop = recordAddr
                        , mem = mem'
                        , spMax = stackTop
                        , callDepth = 1
                        , callDepthMax = 1
                        , ctrlDepth = 0
                        , ctrlDepthMax = 0
                        , functions = functionTable
                        , stopped = False
                        , internalError = Nothing
                        }

-- | Write a control record's six fields at @addr@ (word-index offsets,
-- scaled to bytes internally -- this memory is byte-addressed and a `w`
-- occupies 'byteSizeT' bytes, not one address). Pure -- used both by
-- 'initWasm32State' (before any 'State' machinery exists) and, wrapped by
-- 'writeRecord', by 'enter' during execution.
writeRecordAt ::
    forall w.
    (MachineWord w) =>
    IoMem (Isa w w) w
    -> Int
    -> Int
    -- ^ link
    -> RecordExtra
    -> Int
    -- ^ endPc
    -> Int
    -- ^ resultCount
    -> Either Text (IoMem (Isa w w) w)
writeRecordAt m addr link extra endPc resultCount =
    let step = byteSizeT @w
        (field4, field5) = extraRawFields extra
     in foldlM
            (\m' (o, v) -> writeWord m' (addr + o * step) (toEnum v))
            m
            [ (recordLinkOffset, link)
            , (recordKindOffset, fromEnum (recordKindOf extra))
            , (recordEndPcOffset, endPc)
            , (recordResultCountOffset, resultCount)
            , (recordField4Offset, field4)
            , (recordField5Offset, field5)
            ]

setPc :: Int -> State (MachineState (IoMem (Isa w w) w) w) ()
setPc addr = modify $ \st -> st{pc = addr}

nextPc :: Isa w w -> State (MachineState (IoMem (Isa w w) w) w) ()
nextPc instruction = do
    State{pc} <- get
    setPc (pc + byteSize instruction)

raiseInternalError :: Text -> State (MachineState (IoMem (Isa w w) w) w) ()
raiseInternalError msg = modify $ \st -> st{internalError = Just msg}

-- | Set `sp`, tracking its high-water mark ('spMax').
setSp :: Int -> State (MachineState (IoMem (Isa w w) w) w) ()
setSp sp' = modify $ \st -> st{sp = sp', spMax = max (spMax st) sp'}

-- | This memory is byte-addressed and a `w` occupies 'byteSizeT' bytes,
-- not one address, so `sp` must step by that width, not by 1.
pushValue :: forall w. (MachineWord w) => w -> State (MachineState (IoMem (Isa w w) w) w) ()
pushValue value = do
    State{sp} <- get
    setWord sp value
    setSp (sp + byteSizeT @w)

-- | No underflow guard: popping past the current frame's locals reads
-- whatever is physically below them (another frame's data, or a memory
-- error at the very bottom) -- the same "no floor check" gap real Wasm
-- closes with an ahead-of-time validator, not a runtime check.
popValue :: forall w. (MachineWord w) => State (MachineState (IoMem (Isa w w) w) w) w
popValue = do
    State{sp} <- get
    let sp' = sp - byteSizeT @w
    setSp sp'
    getWord sp'

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

lookupLocal :: forall w. (MachineWord w) => Int -> State (MachineState (IoMem (Isa w w) w) w) w
lookupLocal index = do
    State{frameBase} <- get
    getWord (frameBase + index * byteSizeT @w)

setLocal :: forall w. (MachineWord w) => Int -> w -> State (MachineState (IoMem (Isa w w) w) w) ()
setLocal index value = do
    State{frameBase} <- get
    setWord (frameBase + index * byteSizeT @w) value

readRecordField :: forall w. (MachineWord w) => Int -> Int -> State (MachineState (IoMem (Isa w w) w) w) Int
readRecordField addr offset = fromEnum <$> getWord (addr + offset * byteSizeT @w)

readRecordKind :: (MachineWord w) => Int -> State (MachineState (IoMem (Isa w w) w) w) RecordKind
readRecordKind addr = toEnum <$> readRecordField addr recordKindOffset

writeRecord ::
    (MachineWord w) =>
    Int -> Int -> RecordExtra -> Int -> Int -> State (MachineState (IoMem (Isa w w) w) w) ()
writeRecord addr link extra endPc resultCount = do
    st@State{mem} <- get
    case writeRecordAt mem addr link extra endPc resultCount of
        Right mem' -> put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err

-- | Push a new control record and make it the current one (`ctrlTop`).
-- Used by `block`/`loop`/`if` (once a branch is actually taken) and
-- `call`. See docs/wasm32.md's "Control Records" section.
enter ::
    forall w. (MachineWord w) => Int -> Int -> RecordExtra -> State (MachineState (IoMem (Isa w w) w) w) ()
enter endPc resultCount extra = do
    State{sp, ctrlTop} <- get
    writeRecord sp ctrlTop extra endPc resultCount
    setSp (sp + recordWidth * byteSizeT @w)
    modify $ \st -> st{ctrlTop = sp}
    bumpDepth (recordKindOf extra)
    where
        bumpDepth RecordCall = modify $ \st -> st{callDepth = callDepth st + 1, callDepthMax = max (callDepthMax st) (callDepth st + 1)}
        bumpDepth _ = modify $ \st -> st{ctrlDepth = ctrlDepth st + 1, ctrlDepthMax = max (ctrlDepthMax st) (ctrlDepth st + 1)}

-- | The one operation behind `end`, a taken `br`/`br_if`, and `return`:
-- collapse the record at @r@, optionally keeping it open (a taken branch
-- to a `loop`). See docs/wasm32.md's "Control Records" section.
collapse :: (MachineWord w) => Int -> Bool -> State (MachineState (IoMem (Isa w w) w) w) ()
collapse r keepOpen = do
    kind <- readRecordKind r
    case kind of
        RecordCall -> do
            -- Read every field before touching the stack below: closing
            -- overwrites the record's own bytes (splicing or relocating
            -- results into/through them), so anything still needed from
            -- it must be read first.
            resultCount <- readRecordField r recordResultCountOffset
            savedFrameBase <- readRecordField r recordField4Offset
            link <- readRecordField r recordLinkOffset
            endPc <- readRecordField r recordEndPcOffset
            -- A function declares a real result count, so closing it
            -- collects exactly that many values and reclaims everything
            -- below them: the record itself, and (since `r` sits above
            -- the callee's locals) the locals too.
            results <- popValues resultCount
            State{frameBase} <- get
            setSp frameBase
            mapM_ pushValue results
            modify $ \st -> st{frameBase = savedFrameBase, ctrlTop = link, callDepth = callDepth st - 1}
            if endPc == nullAddr
                then modify $ \st -> st{stopped = True}
                else setPc endPc
        _
            -- Branching back into a `loop` never removes anything: a
            -- well-formed body already leaves the stack exactly as it
            -- found it (Wasm32 gives block/loop/if no declared result
            -- type to collect otherwise), so there's nothing to splice.
            | keepOpen -> readRecordField r recordField5Offset >>= setPc
            -- Closing a `block`/`loop`/`if`: unlike `Call`, there are no
            -- locals of its own to reclaim -- only the record's own words
            -- need to go, so splice them out and keep everything the body
            -- pushed above them (see 'spliceOut'). Read link/endPc first
            -- -- the splice overwrites this record's own bytes.
            | otherwise -> do
                link <- readRecordField r recordLinkOffset
                -- Unlike a Call record's endPc (already a full return
                -- address), Block/Loop/If store the matching `end`
                -- instruction's own address (from findEndPc/
                -- resolveIfTargets) -- same convention `executeElse`
                -- uses, so skip past it the same way here.
                endPc <- readRecordField r recordEndPcOffset
                State{sp} <- get
                spliceOut r recordWidth sp
                modify $ \st -> st{ctrlTop = link, ctrlDepth = ctrlDepth st - 1}
                setPc (endPc + byteSize End)

-- | Remove the @widthWords@-word record at @r@, shifting @[r+width, top)@
-- (moved one whole `w` at a time) down to start at @r@, and shrinking `sp`
-- to match.
spliceOut :: forall w. (MachineWord w) => Int -> Int -> Int -> State (MachineState (IoMem (Isa w w) w) w) ()
spliceOut r widthWords top = do
    let step = byteSizeT @w
        width = widthWords * step
    forM_ [0, step .. top - r - width - 1] $ \i -> getWord (r + width + i) >>= setWord (r + i)
    setSp (top - width)

-- | Walk the control chain from `ctrlTop` for the record tagged @label@,
-- erroring rather than walking past a `Call` record (a valid program's
-- `br`/`br_if` always resolves within its own function).
findLabel :: (MachineWord w) => Int -> State (MachineState (IoMem (Isa w w) w) w) (Either Text Int)
findLabel label = get >>= go . ctrlTop
    where
        go r
            | r == nullAddr = return $ Left $ "unknown control label: " <> show label
            | otherwise = do
                kind <- readRecordKind r
                if kind == RecordCall
                    then return $ Left $ "unknown control label: " <> show label
                    else do
                        l <- readRecordField r recordField4Offset
                        if l == label
                            then return $ Right r
                            else readRecordField r recordLinkOffset >>= go

-- | Walk the control chain from `ctrlTop` for the nearest enclosing `Call`
-- record -- what `return` closes.
findCall :: (MachineWord w) => State (MachineState (IoMem (Isa w w) w) w) (Either Text Int)
findCall = get >>= go . ctrlTop
    where
        go r
            | r == nullAddr = return $ Left "return without active function frame"
            | otherwise = do
                kind <- readRecordKind r
                if kind == RecordCall
                    then return $ Right r
                    else readRecordField r recordLinkOffset >>= go

-- | Collapse every open record from `ctrlTop` down to and including @r@,
-- keeping @r@ itself open only if @keepOpen@ (a taken branch back into a
-- `loop`). Anything strictly above @r@ is always fully closed along the
-- way -- branching or returning past a scope exits it unconditionally,
-- regardless of what its own body would otherwise have preserved.
unwindTo :: (MachineWord w) => Int -> Bool -> State (MachineState (IoMem (Isa w w) w) w) ()
unwindTo r keepOpen = do
    State{ctrlTop} <- get
    if ctrlTop == r
        then collapse r keepOpen
        else do
            collapse ctrlTop False
            unwindTo r keepOpen

-- | `br`/`br_if <label>`: find the targeted record and unwind to it,
-- keeping it open only when it's a `loop` (branching back to its start).
branch :: (MachineWord w) => Int -> State (MachineState (IoMem (Isa w w) w) w) ()
branch label = do
    result <- findLabel label
    case result of
        Right r -> do
            kind <- readRecordKind r
            unwindTo r (kind == RecordLoop)
        Left err -> raiseInternalError err

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

-- | Resolve the else/end targets for the `if` starting right after the
-- given pc. Currently rescans the bytecode on every execution.
--
-- TODO: back this with a small LRU cache keyed by the `if`'s pc (default
-- size 4), storing the resolved (elsePc, endPc) pair, since the bytecode
-- is immutable and targets never change once resolved. This is the
-- branch-target-buffer-style optimization discussed for hot loops.
resolveIfTargets :: (MachineWord w) => IoMem (Isa w w) w -> Int -> Either Text (Maybe Int, Int)
resolveIfTargets = findIfTargets

-- | `else` is reached only by falling through a taken then-branch, so the
-- record it needs (the currently-open `If`) is always `ctrlTop` -- it
-- reads that record's `endPc` and jumps past it, without closing it (the
-- real `end` still has to run to close it).
executeElse :: (MachineWord w) => State (MachineState (IoMem (Isa w w) w) w) ()
executeElse = do
    State{ctrlTop} <- get
    endPc <- readRecordField ctrlTop recordEndPcOffset
    setPc (endPc + byteSize End)

-- | Pure lookup, used only by the report/debug views below (never by
-- instruction execution): the address of the nearest enclosing `Call`
-- record's `FuncHeader`, found by walking from @r@ via `link`.
currentEntryPc :: (MachineWord w) => IoMem (Isa w w) w -> Int -> Maybe Int
currentEntryPc m = go
    where
        go r
            | r == nullAddr = Nothing
            | otherwise = case pureRecordKind m r of
                Just RecordCall -> pureRecordField m r recordField5Offset
                Just _ -> pureRecordField m r recordLinkOffset >>= go
                Nothing -> Nothing

-- | 'Either' has no 'hush' in scope here; a local one-liner is simpler
-- than pulling in another import for it.
eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just

pureRecordField :: forall w. (MachineWord w) => IoMem (Isa w w) w -> Int -> Int -> Maybe Int
pureRecordField m addr offset = fromEnum . snd <$> eitherToMaybe (readWord m (addr + offset * byteSizeT @w))

pureRecordKind :: (MachineWord w) => IoMem (Isa w w) w -> Int -> Maybe RecordKind
pureRecordKind m addr = toEnum <$> pureRecordField m addr recordKindOffset

-- | The current function's declared signature, looked up (for reporting
-- only) via the innermost `Call` record's saved entry pc.
currentFunctionMeta :: (MachineWord w) => MachineState (IoMem (Isa w w) w) w -> Maybe FunctionMeta
currentFunctionMeta State{mem, ctrlTop, functions} = currentEntryPc mem ctrlTop >>= (`IntMap.lookup` functions)

instance (MachineWord w) => StateInterspector (MachineState (IoMem (Isa w w) w) w) (IoMem (Isa w w) w) (Isa w w) w where
    programCounter State{pc} = pc
    memoryDump State{mem} = mem
    ioStreams State{mem = IoMem{mIoStreams}} = mIoStreams
    reprState labels st v
        | Just v' <- defaultView labels st v = v'
    reprState labels st v =
        case T.splitOn ":" v of
            ["stack", f] -> stackView f st
            ["locals", f] -> localsView f st
            ["local", name, f] -> localView f name st
            ["frames"] -> show (callDepth st)
            ["ctrl"] -> ctrlView st
            [r] -> reprState labels st (r <> ":dec")
            [r, _] -> unknownView r
            _ -> errorView v
        where
            formatValues "dec" values = toText $ intercalate ":" $ map show values
            formatValues "hex" values = T.intercalate ":" $ map (toText . word32ToHex) values
            formatValues f _ = unknownFormat f

            -- Raw words above the current frame's own locals and its call
            -- record (which always sits right after them, see 'enter'),
            -- top first: a genuine stack dump, so this includes any
            -- block/loop/if control records currently open above that,
            -- not just pure operand values (those interleave once any
            -- are open).
            step = byteSizeT @w

            stackView f st'@State{mem, sp} = case currentFunctionMeta st' of
                Nothing -> ""
                Just meta ->
                    let lo = frameBase st' + length (fmLocalNames meta) * step + recordWidth * step
                        values = mapMaybe (\a -> eitherToMaybe (readWord mem a) <&> snd) [sp - step, sp - 2 * step .. lo]
                     in formatValues f values

            localsView f st'@State{mem} = case currentFunctionMeta st' of
                Nothing -> ""
                Just meta ->
                    let names = fmLocalNames meta
                        values = map (\i -> maybe def snd (eitherToMaybe (readWord mem (frameBase st' + i * step)))) [0 .. length names - 1]
                     in T.intercalate ":" $ zipWith (\n value -> toText n <> "=" <> viewRegister f value) names values

            localView f name st'@State{mem} = case currentFunctionMeta st' of
                Nothing -> unknownView name
                Just meta ->
                    let names = fmLocalNames meta
                        byName = snd <$> find ((== toString name) . fst) (zip names [0 :: Int ..])
                        index = case byName of
                            Just i -> Just i
                            Nothing -> readMaybe (toString name)
                     in case index of
                            Just i
                                | i >= 0 && i < length names ->
                                    maybe (unknownView name) (viewRegister f . snd) (eitherToMaybe (readWord mem (frameBase st' + i * step)))
                            _ -> unknownView name

            -- Structured control ids only (skips the enclosing `Call`),
            -- innermost first.
            ctrlView State{mem, ctrlTop} = T.intercalate ":" $ map show (go ctrlTop)
                where
                    go r
                        | r == nullAddr = []
                        | otherwise = case pureRecordKind mem r of
                            Just RecordCall -> []
                            Just _ -> case (pureRecordField mem r recordField4Offset, pureRecordField mem r recordLinkOffset) of
                                (Just label, Just link) -> label : go link
                                _ -> []
                            Nothing -> []

    summaryView _labels State{spMax, callDepthMax, ctrlDepthMax} v = case T.splitOn ":" v of
        ["wasm32", "operand-stack-max"] -> Just $ show spMax
        ["wasm32", "frames-max"] -> Just $ show callDepthMax
        ["wasm32", "control-stack-max"] -> Just $ show ctrlDepthMax
        ["isa-specific"] ->
            Just $
                "wasm32:operand-stack-max: "
                    <> show spMax
                    <> "\n"
                    <> "wasm32:frames-max:        "
                    <> show callDepthMax
                    <> "\n"
                    <> "wasm32:control-stack-max: "
                    <> show ctrlDepthMax
        _ -> Nothing

    isHalted State{stopped} = stopped

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
                    Right endPc -> enter endPc 0 (BlockExtra label) >> nextPc instruction
                    Left err -> raiseInternalError $ "control flow error: " <> err
            Loop label -> do
                State{pc, mem} <- get
                case findEndPc mem (pc + byteSize instruction) of
                    Right endPc -> enter endPc 0 (LoopExtra label (pc + byteSize instruction)) >> nextPc instruction
                    Left err -> raiseInternalError $ "control flow error: " <> err
            If label -> do
                condition <- popValue
                State{pc, mem} <- get
                case resolveIfTargets mem (pc + byteSize instruction) of
                    Right (elsePc, endPc)
                        | condition /= 0 -> enter endPc 0 (IfExtra label) >> nextPc instruction
                        | Just elseAddr <- elsePc -> do
                            enter endPc 0 (IfExtra label)
                            setPc (elseAddr + byteSize Else)
                        | otherwise -> setPc (endPc + byteSize End)
                    Left err -> raiseInternalError $ "control flow error: " <> err
            Else -> executeElse
            End -> do
                State{ctrlTop} <- get
                collapse ctrlTop False
            Br label -> branch label
            BrIf label -> do
                condition <- popValue
                if condition /= 0 then branch label else nextPc instruction
            Call target -> do
                State{pc, mem} <- get
                case readInstruction mem (fromEnum target) of
                    Right (mem', FuncHeader paramCount declaredLocalCount resultCount) -> do
                        modify $ \st -> st{mem = mem'}
                        State{sp, frameBase = callerFrameBase} <- get
                        let calleeFrameBase = sp - paramCount * byteSizeT @w
                        replicateM_ declaredLocalCount (pushValue def)
                        enter (pc + byteSize instruction) resultCount (CallExtra callerFrameBase (fromEnum target))
                        modify $ \st -> st{frameBase = calleeFrameBase}
                        setPc (fromEnum target + funcHeaderSize)
                    Right _ -> raiseInternalError "call target does not point to .func"
                    Left err -> raiseInternalError $ "control flow error: " <> err
            Return -> do
                result <- findCall
                case result of
                    Right r -> unwindTo r False
                    Left err -> raiseInternalError err
            Halt -> modify $ \st -> st{stopped = True}
            Unreachable -> raiseInternalError "unreachable"
            Nop -> nextPc instruction
            FuncHeader{} -> raiseInternalError "fell into function metadata; call should have jumped past it"
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
