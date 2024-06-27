{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Report (
    ReportFilter (..),
    ReportSlice (..),
    StateInspector (..),
    StateInspectorToken (..),
    applyReportFilter,
    prepareReport,
    ReportConf (..),
    selectSlice,
    prepareReportRecord,
)
where

import Data.Aeson (FromJSON (..), Value (..), genericParseJSON)
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Data.Text (strip)
import Isa.Risc
import Machine.Memory
import Machine.Types
import Relude
import Relude.Extra
import Relude.Unsafe qualified as Unsafe

data ReportConf = ReportConf
    { rcName :: Maybe String
    , rcSlice :: ReportSlice
    , rcFilter :: [ReportFilter]
    , rcInspector :: Maybe [StateInspector]
    , rcAssert :: Maybe String
    }
    deriving (Show, Generic)

instance FromJSON ReportConf where
    parseJSON = genericParseJSON $ aesonDrop 2 snakeCase

prepareReport verbose records rc@ReportConf{rcName, rcFilter, rcSlice, rcInspector, rcAssert} =
    let header = maybe "" ("# " <>) rcName
        details = if verbose then show rc else ""
        filtered =
            if null rcFilter
                then records
                else filter (applyReportFilter rcFilter) records
        sliced = selectSlice rcSlice filtered
        journalText = intercalate " ;;\n" $ map (prepareReportRecord $ fromMaybe [] rcInspector) sliced
        assertReport =
            let actual = strip $ toText journalText
                expect = maybe "" (strip . toText) rcAssert
                diff = getGroupedDiff (map toString $ lines actual) (map toString $ lines expect)
             in if isNothing rcAssert || actual == expect
                    then ""
                    else "\nASSERTION FAIL, expect:\n" <> toString expect <> "\nDiff:\n" <> ppDiff diff
     in ( null assertReport
        , intercalate "\n" $ filter (not . null) [header, details, journalText, assertReport]
        )

-----------------------------------------------------------

data ReportFilter = IsInstruction | IsState
    deriving (Show)

instance FromJSON ReportFilter where
    parseJSON (String "instruction") = return IsInstruction
    parseJSON (String "state") = return IsState
    parseJSON _ = fail "Invalid filter format, expect: \"instruction\" or \"state\""

applyReportFilter (IsInstruction : _) TInstruction{} = True
applyReportFilter (IsState : _) TState{} = True
applyReportFilter (_ : fs) x = applyReportFilter fs x
applyReportFilter _ _ = False

-----------------------------------------------------------

data ReportSlice = HeadSlice Int | AllSlice | TailSlice Int | LastSlice
    deriving (Show)

instance FromJSON ReportSlice where
    parseJSON (Array xs) | [String "head", Number n] <- toList xs = return $ HeadSlice $ round n
    parseJSON (String "all") = return AllSlice
    parseJSON (Array xs) | [String "tail", Number n] <- toList xs = return $ TailSlice $ round n
    parseJSON (String "last") = return LastSlice
    parseJSON _ = fail "Invalid slice format, expect: [\"head\", n], \"all\", [\"tail\", n], \"last\""

selectSlice (HeadSlice n) = take n
selectSlice AllSlice = id
selectSlice (TailSlice n) = reverse . take n . reverse
selectSlice LastSlice = take 1 . reverse

-----------------------------------------------------------

newtype StateInspector = StateInspector [StateInspectorToken]
    deriving (Show, Generic)

instance FromJSON StateInspector

data StateInspectorToken
    = Label String
    | MemoryCells (Int, Int)
    | Register String
    | NumberIoStream Int
    | SymbolIoStream Int
    deriving (Show, Generic)

instance FromJSON StateInspectorToken where
    parseJSON (Array xs) = case toList xs of
        [String "label", String l] -> return $ Label $ toString l
        [String "memory_cells", Number a, Number b] -> return $ MemoryCells (round a, round b)
        [String "register", String r] -> return $ Register $ toString r
        [String "number_io_stream", Number n] -> return $ NumberIoStream $ round n
        [String "symbol_io_stream", Number n] -> return $ SymbolIoStream $ round n
        _ -> fail "Invalid inspector format."
    parseJSON _ = fail "Invalid inspector format."

prepareReportRecord inspectors record =
    case record of
        TState st -> intercalate "\n" $ map (toString . inspect st) inspectors
        TLog msg -> toString msg
        (TInstruction pc label i) ->
            let label' = maybe "" ("  \t@" <>) label
             in show pc <> ":\t" <> show i <> label'

inspect st (StateInspector inspectors) = unwords $ map (toText . inspectToken st) inspectors

inspectToken _ (Label l) = l
inspectToken st (Register name) = case registers st !? Unsafe.read name of
    Just v -> show v
    Nothing -> "register " <> name <> " not found"
inspectToken st (MemoryCells (a, b)) = prettyDump mempty $ fromList $ sliceMem [a .. b] $ memoryDump st
inspectToken st (NumberIoStream addr) = case ioStreams st !? addr of
    Just (is, os) -> show is <> " >>> " <> show (reverse os)
    Nothing -> error $ "incorrect IO address: " <> show addr
inspectToken st (SymbolIoStream addr) = case bimap sym sym <$> ioStreams st !? addr of
    Just (is, os) -> show is <> " >>> " <> show (reverse os)
    Nothing -> error $ "incorrect IO address: " <> show addr
    where
        sym = map (chr . fromEnum)
