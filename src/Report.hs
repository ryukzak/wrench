{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Report (
    ReportSlice (..),
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
    rc@ReportConf{rcName, rcSlice, rcAssert, rcView} =
        let header = maybe "" ("# " <>) rcName
            details = if verbose then show rc else ""
            sliced = selectSlice rcSlice records
            stateViews = case rcView of
                Nothing -> ""
                Just rvView' ->
                    concat
                        $ filter (not . null)
                        $ map (prepareStateView rvView' trResult)
                        $ getTraceStates sliced
            assertReport =
                let actual = nospaces $ toText stateViews
                    expect = maybe "" (nospaces . toText) rcAssert
                 in if isNothing rcAssert || actual == expect
                        then ""
                        else "ASSERTION FAIL, expect:\n" <> toString expect
         in ( null assertReport
            , unlines
                $ map (T.strip . toText)
                $ filter (not . null) [header, details, stateViews, assertReport]
            )
        where
            nospaces = unlines . map T.strip . lines . T.strip

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

prepareStateView line TranslatorResult{labels} st =
    toString $ substituteBrackets (reprState labels st) line

defaultView labels st "pc:label" =
    Just $ case filter (\(_l, a) -> a == toEnum (programCounter st)) $ toPairs labels of
        (l, _a) : _ -> "@" <> toText l
        _ -> ""
defaultView _labels st "instruction" =
    Just $ either error show (readInstruction (memoryDump st) (programCounter st))
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
    Just (is, os) -> fixEscapes (show is) <> " >>> " <> fixEscapes (show (reverse os))
    Nothing -> error $ "incorrect IO address: " <> show addr
    where
        sym =
            map
                ( ( \case
                        0 -> '\0'
                        10 -> '\n'
                        x | 32 <= x && x <= 126 -> chr x
                        _ -> '?'
                  )
                    . fromEnum
                )
        fixEscapes = T.replace "\\NUL" "\\0" . (toText :: String -> Text)
viewIO fmt _addr _st = unknownFormat fmt

readAddr t = fromMaybe (error $ "can't parse memory address: " <> t) $ readMaybe $ toString t

viewRegister "dec" = show
viewRegister "hex" = toText . word32ToHex
viewRegister f = \_ -> unknownFormat f

errorView v = error $ "view error: " <> v

unknownView v = "[unknown-view <" <> v <> ">]"

unknownFormat f = "[unknown-format <" <> f <> ">]"
