{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Report (
    ReportFilter (..),
    ReportSlice (..),
    StateInspector (..),
    StateInspectorToken (..),
    applyReportFilter,
    prepareReport,
    ReportConf (..),
) where

import Data.Aeson (FromJSON (..), Value (..), genericParseJSON)
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Data.Text (replace, strip)
import Isa.RiscIv
import Machine.Memory
import Machine.Types
import Relude
import Relude.Extra
import Relude.Unsafe qualified as Unsafe

data ReportConf = ReportConf
    { rcName :: Maybe String
    -- ^ Optional name of the report.
    -- Example: Just "My Report"
    , rcSlice :: ReportSlice
    -- ^ Specifies which part of the report to select.
    -- Example: HeadSlice 10
    , rcFilter :: Maybe [ReportFilter]
    -- ^ List of filters to apply to the report.
    -- Example: [IsInstruction, IsState]
    , rcInspector :: Maybe [StateInspector]
    -- ^ Optional list of state inspectors.
    -- Example: Just [StateInspector [Label "PC", Register "r1"]]
    , rcAssert :: Maybe String
    -- ^ Optional assertion string to compare the report against.
    -- Example: Just "Expected output"
    }
    deriving (Show, Generic)

instance FromJSON ReportConf where
    parseJSON = genericParseJSON $ aesonDrop 2 snakeCase

prepareReport verbose records rc@ReportConf{rcName, rcFilter, rcSlice, rcInspector, rcAssert} =
    let header = maybe "" ("# " <>) rcName
        details = if verbose then show rc else ""
        filtered = case rcFilter of
            Nothing -> filter (applyReportFilter [IsState]) records
            Just [] -> filter (applyReportFilter [IsState]) records
            Just fs -> filter (applyReportFilter fs) records
        sliced = selectSlice rcSlice filtered
        journalText = intercalate " ;;\n" $ map (prepareReportRecord $ fromMaybe [] rcInspector) sliced
        assertReport =
            let actual = strip $ toText journalText
                expect = maybe "" (strip . toText) rcAssert
                diff = getGroupedDiff (map toString $ lines actual) (map toString $ lines expect)
             in if isNothing rcAssert || actual == expect
                    then ""
                    else "\nASSERTION FAIL, expect:\n" <> toString expect <> "\n\nDiff:\n" <> ppDiff diff
     in ( null assertReport
        , intercalate "\n" $ filter (not . null) [header, details, journalText, assertReport]
        )

-----------------------------------------------------------

-- | Filters that can be applied to the report.
data ReportFilter
    = -- | Filter for instruction records.
      IsInstruction
    | -- | Filter for state records.
      IsState
    deriving (Show)

instance FromJSON ReportFilter where
    parseJSON (String "instruction") = return IsInstruction
    parseJSON (String "state") = return IsState
    parseJSON _ = fail "Invalid filter format, expect: \"instruction\" or \"state\""

applyReportFilter (IsInstruction : _) TInstruction{} = True
applyReportFilter (IsState : _) TState{} = True
applyReportFilter _ TWarn{} = True
applyReportFilter (_ : fs) x = applyReportFilter fs x
applyReportFilter _ _ = False

-----------------------------------------------------------

-- | Specifies which part of the report to select.
data ReportSlice
    = -- | Select the first 'n' records.
      HeadSlice Int
    | -- | Select all records.
      AllSlice
    | -- | Select the last 'n' records.
      TailSlice Int
    | -- | Select only the last record.
      LastSlice
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

-- | Represents a state inspector which contains a list of state inspector tokens.
newtype StateInspector = StateInspector [StateInspectorToken]
    deriving (Show, Generic)

instance FromJSON StateInspector

-- | Tokens that can be used by a state inspector to extract and format specific parts of the state.
data StateInspectorToken
    = -- | A label with the given text.
      Label String
    | -- | A range of memory cells from the first to the second address.
      MemoryCells (Int, Int)
    | View Text
    | -- | The value of the register with the given name.
      Register String
    | -- | The value of the register with the given name, formatted as hexadecimal.
      RegisterHex String
    | -- | The number of tokens in the input stream at the given address.
      NumberIoStream Int
    | -- | The symbols in the input stream at the given address.
      SymbolIoStream Int
    deriving (Show, Generic)

instance FromJSON StateInspectorToken where
    parseJSON (String l) = return $ Label $ toString l
    parseJSON (Array xs) = case toList xs of
        [String "label", String l] -> return $ Label $ toString l
        [String "memory_cells", Number a, Number b] -> return $ MemoryCells (round a, round b)
        [String "register", String r] -> return $ Register $ toString r
        [String "register_hex", String r] -> return $ RegisterHex $ toString r
        [String "view", String v] -> return $ View v
        [String "number_io_stream", Number n] -> return $ NumberIoStream $ round n
        [String "symbol_io_stream", Number n] -> return $ SymbolIoStream $ round n
        _ -> fail "Invalid inspector format."
    parseJSON _ = fail "Invalid inspector format."

prepareReportRecord inspectors record =
    case record of
        TState st -> intercalate "\n" $ map (toString . inspect st) inspectors
        TWarn msg -> toString msg
        (TInstruction pc label i) ->
            let label' = maybe "" ("  \t@" <>) label
             in show pc <> ":\t" <> show i <> label'

inspect st (StateInspector inspectors) = unwords $ map (toText . inspectToken st) inspectors

inspectToken _ (Label l) = l
inspectToken st (Register name) = case registers st !? Unsafe.read name of
    Just v -> show v
    Nothing -> "register " <> name <> " not found"
inspectToken st (RegisterHex name) = case registers st !? Unsafe.read name of
    Just v -> word32ToHex v
    Nothing -> "register " <> name <> " not found"
inspectToken st (View name) = toString $ viewState st name
inspectToken st (MemoryCells (a, b)) = prettyDump mempty $ fromList $ sliceMem [a .. b] $ memoryDump st
inspectToken st (NumberIoStream addr) = case ioStreams st !? addr of
    Just (is, os) -> show is <> " >>> " <> show (reverse os)
    Nothing -> error $ "incorrect IO address: " <> show addr
inspectToken st (SymbolIoStream addr) = case bimap sym sym <$> ioStreams st !? addr of
    Just (is, os) -> show is <> " >>> " <> fixEscapes (show (reverse os))
    Nothing -> error $ "incorrect IO address: " <> show addr
    where
        sym = map (chr . fromEnum)
        fixEscapes = toString . replace "\\NUL" "\\0" . (toText :: String -> Text)
