{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Report (
    ReportFilter (..),
    ReportSlice (..),
    StateInspector (..),
    StateInspectorToken (..),
    applyReportFilter,
    prepareReport,
    ReportConf (..),
    substituteBrackets,
    viewRegister,
    defaultView,
    errorView,
    unknownView,
    unknownFormat,
) where

import Data.Aeson (FromJSON (..), Value (..), genericParseJSON)
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Text qualified as T
import Machine.Memory
import Machine.Types
import Relude
import Relude.Extra
import Relude.Unsafe qualified as Unsafe
import Text.Regex.TDFA
import Translator (TranslatorResult (..))

substituteBrackets :: (Text -> Text) -> Text -> Text
substituteBrackets f input =
    let regex = "\\{([^}]*)\\}" :: Text -- Regex pattern to match text inside {}
        matches = getAllTextMatches (input =~ regex)
        changes =
            map
                ( \(x :: Text) ->
                    let v = T.tail $ T.init x
                     in (x, f v)
                )
                matches
     in foldr (\(old, new) st -> T.replace old new st) input changes

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
    , rcView :: Maybe Text
    }
    deriving (Generic, Show)

instance FromJSON ReportConf where
    parseJSON = genericParseJSON $ aesonDrop 2 snakeCase

prepareReport
    trResult@TranslatorResult{}
    verbose
    records
    rc@ReportConf{rcName, rcFilter, rcSlice, rcInspector, rcAssert, rcView} =
        let header = maybe "" ("# " <>) rcName
            details = if verbose then show rc else ""
            filtered = case rcFilter of
                Nothing -> filter (applyReportFilter [IsState]) records
                Just [] -> filter (applyReportFilter [IsState]) records
                Just fs -> filter (applyReportFilter fs) records
            sliced = selectSlice rcSlice filtered
            journalText = case rcInspector of
                Nothing -> ""
                Just is -> unlines $ map (toText . prepareReportRecord is) sliced
            stateViews = case rcView of
                Nothing -> ""
                Just rvView' ->
                    concat
                        $ filter (not . null)
                        $ map (prepareStateView rvView' trResult)
                        $ mapMaybe (\case (TState st) -> Just st; _ -> Nothing) (selectSlice rcSlice records)
            assertReport =
                let actual = T.strip journalText <> T.strip (toText stateViews)
                    expect = maybe "" (T.strip . toText) rcAssert
                 in if isNothing rcAssert || actual == expect
                        then ""
                        else "ASSERTION FAIL, expect:\n" <> toString expect
         in ( null assertReport
            , unlines
                $ map (T.strip . toText)
                $ filter (not . null) [header, details, toString journalText, stateViews, assertReport]
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
    deriving (Generic, Show)

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
    deriving (Generic, Show)

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

prepareStateView line TranslatorResult{labels} st =
    toString $ substituteBrackets (reprState labels st) line

defaultView labels st "pc:label" =
    Just $ case filter (\(_l, a) -> a == toEnum (programCounter st)) $ toPairs labels of
        (l, _a) : _ -> "@" <> toText l
        _ -> ""
defaultView _labels st "instruction" =
    Just $ show $ evalState (readInstruction (programCounter st)) $ memoryDump st
defaultView labels st v =
    case T.splitOn ":" v of
        ["pc"] -> Just $ reprState labels st "pc:dec"
        ["pc", f] -> Just $ viewRegister f (programCounter st)
        ["memory", a, b] -> Just $ viewMemory a b st
        ["io", a] -> Just $ reprState labels st ("io:" <> a <> ":dec")
        ["io", a, fmt] -> Just $ viewIO fmt a st
        _ -> Nothing

viewMemory a b st =
    toText $ prettyDump mempty $ fromList $ sliceMem [readAddr a .. readAddr b] $ memoryDump st

viewIO "dec" addr st = case ioStreams st !? readAddr addr of
    Just (is, os) -> show is <> " >>> " <> show (reverse os)
    Nothing -> error $ "incorrect IO address: " <> show addr
viewIO "hex" addr st = case ioStreams st !? readAddr addr of
    Just (is, os) ->
        T.replace "\"" ""
            $ T.intercalate
                ""
                [ show (map word32ToHex is)
                , " >>> "
                , show (reverse (map word32ToHex os))
                ]
    Nothing -> error $ "incorrect IO address: " <> show addr
viewIO "sym" addr st = case bimap sym sym <$> ioStreams st !? readAddr addr of
    Just (is, os) -> show is <> " >>> " <> fixEscapes (show (reverse os))
    Nothing -> error $ "incorrect IO address: " <> show addr
    where
        sym = map (chr . fromEnum)
        fixEscapes = T.replace "\\NUL" "\\0" . (toText :: String -> Text)
viewIO fmt _addr _st = unknownFormat fmt

readAddr t = fromMaybe (error $ "can't parse memory address: " <> t) $ readMaybe $ toString t

viewRegister "dec" = show
viewRegister "hex" = toText . word32ToHex
viewRegister f = \_ -> unknownFormat f

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
        fixEscapes = toString . T.replace "\\NUL" "\\0" . (toText :: String -> Text)

errorView v = error $ "view error: " <> v

unknownView v = "[unknown-view <" <> v <> ">]"

unknownFormat f = "[unknown-format <" <> f <> ">]"
