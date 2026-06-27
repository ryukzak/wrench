{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Wrench.Report (
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
import Numeric (showHex)
import Relude
import Relude.Extra
import Text.Regex.TDFA
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Translator (TranslatorResult (..))

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
    { rcName :: Maybe Text
    -- ^ Optional name of the report.
    -- Example: Just "My Report"
    , rcSlice :: ReportSlice
    -- ^ Specifies which part of the report to select.
    -- Example: HeadSlice 10
    , rcAssert :: Maybe Text
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
    finalState
    records
    rc@ReportConf{rcName, rcSlice, rcAssert, rcView} =
        let header = maybe "" ("# " <>) rcName
            details = if verbose then show rc else ""
            sliced = selectSlice rcSlice records
            stateViews = case rcView of
                Nothing -> ""
                Just rvView' ->
                    T.concat
                        $ filter (not . T.null)
                        $ map
                            ( \case
                                TState{tInstructionCount, tState} ->
                                    prepareStateView rvView' trResult finalState tInstructionCount tState
                                (TError err) -> "ERROR: " <> err <> "\n"
                                (TWarn warn) -> "WARN: " <> warn <> "\n"
                            )
                            sliced

            assertReport =
                let actual = nospaces stateViews
                    expect = maybe "" nospaces rcAssert
                 in if isNothing rcAssert || actual == expect
                        then ""
                        else "ASSERTION FAIL, expect:\n" <> expect
         in ( T.null assertReport
            , T.unlines
                $ map T.strip
                $ filter (not . T.null) [header, details, stateViews, assertReport]
            )
        where
            nospaces = T.unlines . map T.strip . T.lines . T.strip

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

prepareStateView line TranslatorResult{labels, dumpStats} finalState instrCount st =
    let DumpStats
            { dsSectionsTotalBytes
            , dsTextSectionsBytes
            , dsDataSectionsBytes
            , dsTextIntervals
            , dsDataIntervals
            } = dumpStats
        AccessLog{alInstr, alData, alIo} = accessLog (memoryDump finalState)
        resolver v = case T.splitOn ":" v of
            ["sim", "instruction-count"] -> show instrCount
            ["layout", "sections-size"] -> show dsSectionsTotalBytes
            ["layout", "text-sections-size"] -> show dsTextSectionsBytes
            ["layout", "data-sections-size"] -> show dsDataSectionsBytes
            ["layout", "text-ranges"] -> renderIntervalsHex dsTextIntervals
            ["layout", "text-ranges", fmt] -> rangesFmt fmt dsTextIntervals
            ["layout", "data-ranges"] -> renderIntervalsHex dsDataIntervals
            ["layout", "data-ranges", fmt] -> rangesFmt fmt dsDataIntervals
            ["mem", "instr-ranges"] -> renderIntervalsHex alInstr
            ["mem", "instr-ranges", fmt] -> rangesFmt fmt alInstr
            ["mem", "data-ranges"] -> renderIntervalsHex alData
            ["mem", "data-ranges", fmt] -> rangesFmt fmt alData
            ["mem", "io-ranges"] -> renderIntervalsHex alIo
            ["mem", "io-ranges", fmt] -> rangesFmt fmt alIo
            ["memory", "table"] -> renderMemoryTable dumpStats (memoryDump finalState)
            -- ISA-specific summary block: each architecture fills it with its
            -- own stats (vliw:*, f32a:*, ...); ISAs without any render "".
            -- Lets a single report template stay uniform across ISAs.
            ["isa-specific"] -> fromMaybe "" (summaryView labels finalState "isa-specific")
            _ -> case summaryView labels finalState v of
                Just txt -> txt
                Nothing -> reprState labels st v
        rangesFmt "dec" = renderIntervals
        rangesFmt "hex" = renderIntervalsHex
        rangesFmt fmt = const (unknownFormat fmt)
     in substituteBrackets resolver line

-- | Render a full address-space table: one row per declared section, IO
--   cluster, or @x@ (anything else) span. Auto-sized columns, hex addresses
--   widened to fit @memory_size@.
renderMemoryTable ::
    forall m isa w.
    (MachineWord w, Memory m isa w) =>
    DumpStats
    -> m
    -> Text
renderMemoryTable dumpStats mem =
    let memSize = memCapacity mem
        ports = ioPorts mem
        wordSize = byteSizeT @w
        AccessLog{alInstr, alData, alIo} = accessLog mem
        DumpStats{dsTextIntervals, dsDataIntervals} = dumpStats
        ioIntervals = foldr (`recordRange` wordSize) emptyIntervals ports
        accessedAll = alInstr `intervalsUnion` alData `intervalsUnion` alIo

        -- Declared section rows: one per text/data/io cluster (in-section
        -- coverage is shown via multi-cluster Accessed in the row).
        textRows = [("text", lo, hi) | (lo, hi) <- intervalsToList dsTextIntervals]
        dataRows = [("data", lo, hi) | (lo, hi) <- intervalsToList dsDataIntervals]
        ioRows = [("io", lo, hi) | (lo, hi) <- intervalsToList ioIntervals]

        -- Free rows: complement of (text ∪ data ∪ io) within [0, memSize-1],
        -- split further at access boundaries (so the stack gets its own row).
        sectionsAll = dsTextIntervals `intervalsUnion` dsDataIntervals `intervalsUnion` ioIntervals
        boundedMem = intervalsRange 0 (memSize - 1)
        freeRegions = boundedMem `intervalsDifference` sectionsAll
        freeRows = concatMap (uncurry (splitByAccess accessedAll)) (intervalsToList freeRegions)

        spans = sortOn (\(_, lo, _) -> lo) (textRows <> dataRows <> ioRows <> freeRows)

        hexW = max 3 (length (showHex (max 0 (memSize - 1)) ""))
        addrStr a =
            let h = showHex a ""
             in "0x" <> toText (replicate (hexW - length h) '0') <> toText h

        formatRange lo hi = addrStr lo <> ".." <> addrStr hi

        renderAccessed intervals = case intervalsToList intervals of
            [] -> "-"
            pairs -> T.intercalate ", " [formatRange lo hi | (lo, hi) <- pairs]

        buildRow (kind, lo, hi) =
            let bytes = hi - lo + 1
                accessedHere = accessedAll `intervalsIntersect` intervalsRange lo hi
                accessedBytes = intervalsSize accessedHere
                cov :: Int
                cov = if bytes > 0 then (accessedBytes * 100) `div` bytes else 0
             in ( kind
                , formatRange lo hi
                , show bytes
                , renderAccessed accessedHere
                , show cov <> "%"
                )

        headerRow :: (Text, Text, Text, Text, Text)
        headerRow = ("Kind", "Range", "Bytes", "Accessed", "Coverage")
        allRows = headerRow : map buildRow spans

        colWidth f = foldr (max . T.length . f) 0 allRows
        wKind = colWidth (\(k, _, _, _, _) -> k)
        wRange = colWidth (\(_, r, _, _, _) -> r)
        wBytes = colWidth (\(_, _, b, _, _) -> b)
        wAcc = colWidth (\(_, _, _, a, _) -> a)
        wCov = colWidth (\(_, _, _, _, c) -> c)

        padR n s = s <> T.replicate (n - T.length s) " "
        padL n s = T.replicate (n - T.length s) " " <> s

        renderRow (k, r, b, a, c) =
            T.intercalate "  " [padR wKind k, padR wRange r, padL wBytes b, padR wAcc a, padL wCov c]
     in T.intercalate "\n" (map renderRow allRows)

-- | Split a free range @[lo, hi]@ at access boundaries — each accessed cluster
--   and each unaccessed gap becomes its own @x@ row.
splitByAccess :: Intervals -> Int -> Int -> [(Text, Int, Int)]
splitByAccess accessedAll lo hi =
    let inRange = intervalsToList (accessedAll `intervalsIntersect` intervalsRange lo hi)
        go cursor [] = [("x", cursor, hi) | cursor <= hi]
        go cursor ((alo, ahi) : rest)
            | cursor < alo = ("x", cursor, alo - 1) : ("x", alo, ahi) : go (ahi + 1) rest
            | otherwise = ("x", alo, ahi) : go (ahi + 1) rest
     in go lo inRange

defaultView ::
    (ByteSize isa, MachineWord w, Memory m isa w, Show isa, StateInterspector st m isa w) =>
    HashMap Text w
    -> st
    -> Text
    -> Maybe Text
defaultView labels st "pc:label" =
    Just $ case filter (\(_l, a) -> a == toEnum (programCounter st)) $ toPairs labels of
        (l, _a) : _ -> "@" <> toText l
        _ -> ""
defaultView _labels st "instruction" =
    Just $ either error (show . snd) (readInstruction (memoryDump st) (programCounter st))
defaultView labels st v =
    case T.splitOn ":" v of
        ["pc"] -> Just $ reprState labels st "pc:dec"
        ["pc", f] -> Just $ viewRegister f (programCounter st)
        ["memory", a, b] -> Just $ viewMemory a b $ dumpCells $ memoryDump st
        ["io", a] -> Just $ reprState labels st ("io:" <> a <> ":dec")
        ["io", a, fmt] -> Just $ viewIO fmt a st
        _ -> Nothing

viewMemory :: (ByteSize isa, MachineWord w, Show isa) => Text -> Text -> IntMap (Cell isa w) -> Text
viewMemory a b mem =
    toText $ prettyDump mempty $ fromList $ sliceMem [readAddr a .. readAddr b] mem

viewIO "dec" addr st = case ioStreams st !? readAddr addr of
    Just (is, os) -> show is <> " >>> " <> show (reverse os)
    Nothing -> error ("incorrect IO address: " <> show addr)
viewIO "hex" addr st = case ioStreams st !? readAddr addr of
    Just (is, os) ->
        T.replace "\"" ""
            $ T.intercalate
                ""
                [ show (map word32ToHex is)
                , " >>> "
                , show (reverse (map word32ToHex os))
                ]
    Nothing -> error ("incorrect IO address: " <> show addr)
viewIO "sym" addr st = case bimap sym sym <$> ioStreams st !? readAddr addr of
    Just (is, os) -> fixEscapes (show is) <> " >>> " <> fixEscapes (show (reverse os))
    Nothing -> error ("incorrect IO address: " <> show addr)
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
        fixEscapes = T.replace "\\NUL" "\\0"
viewIO fmt _addr _st = unknownFormat fmt

readAddr t = fromMaybe (error $ "can't parse memory address: " <> t) $ readMaybe (toString t)

viewRegister "dec" = show
viewRegister "hex" = toText . word32ToHex
viewRegister f = \_ -> unknownFormat f

errorView v = error $ "view error: " <> v

unknownView v = "[unknown-view <" <> v <> ">]"

unknownFormat f = "[unknown-format <" <> f <> ">]"
