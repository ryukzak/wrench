{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Wrench.Report (
    ReportSlice (..),
    ReportConf (..),
    Report,
    compileReports,
    prepareReport,
    renderIsaView,
    sharedViews,
    isaViews,
    ReportViews,
    valueView,
    flagView,
    showValue,
    showValues,
) where

import Data.Aeson (FromJSON (..), Value (..), genericParseJSON, rejectUnknownFields)
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Text qualified as T
import Numeric (showHex)
import Relude
import Relude.Extra
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Report.View
import Wrench.Translator (TranslatorResult (..))

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
    parseJSON = genericParseJSON $ (aesonDrop 2 snakeCase){rejectUnknownFields = True}

-- | What a shared view sees: the ISA's 'StateEnv' plus the record and the run.
data ReportEnv st = ReportEnv
    { reState :: StateEnv st
    , reDumpStats :: DumpStats
    , reInstrCount :: Int
    , rePrevInstr :: Maybe (IsaOf st)
    , reAccesses :: AccessCount
    -- ^ Made by the instruction that produced this record.
    }

type ReportViews st =
    ( ByteSize (IsaOf st)
    , Inspectable st
    , IsWord (WordOf st)
    , Memory (MemOf st) (IsaOf st) (WordOf st)
    , Show (IsaOf st)
    )

-- | The ISA's own views, its stats among them.
isaViews :: forall st. (Inspectable st) => [View (StateEnv st)]
isaViews = views @st <> map statView (isaStats @st)

-- | Every view a report on this ISA can use.
reportViews :: forall st. (ReportViews st) => [View (ReportEnv st)]
reportViews = map (contramapView reState) (isaViews @st) <> sharedViews

-- | The views every ISA has.
sharedViews :: forall st. (ReportViews st) => [View (ReportEnv st)]
sharedViews =
    [ valueView "pc" "Program counter." (programCounter . current)
    , flagView "pc:label" "`@label` if the program counter is at a label, otherwise empty." pcLabel
    , flagView "instruction:prev" "The instruction that produced this state, `-` for the initial state." $
        maybe "-" show . rePrevInstr
    , flagView "instruction:next" "The instruction to execute next, `-` once halted." (nextInstruction . current)
    , flagView "sim:instruction-count" "Instructions executed so far." (show . reInstrCount)
    , count
        "memAccesses"
        "Memory accesses of the instruction that produced this state, its fetch included."
        (acTotal . reAccesses)
    , count "memReads" "Reads among `memAccesses`; a fetch is a read." (acReads . reAccesses)
    , count "memWrites" "Writes among `memAccesses`." (acWrites . reAccesses)
    , count "memAccessesTotal" "Memory accesses of the run so far." (acTotal . runTotal)
    , count "memReadsTotal" "Reads of the run so far." (acReads . runTotal)
    , count "memWritesTotal" "Writes of the run so far." (acWrites . runTotal)
    , layout "layout:sections-size" "Bytes in all sections." dsSectionsTotalBytes
    , layout "layout:text-sections-size" "Bytes in `.text` sections." dsTextSectionsBytes
    , layout "layout:data-sections-size" "Bytes in `.data` sections." dsDataSectionsBytes
    , ranges "layout:text-ranges" "Address ranges of `.text` sections." (dsTextIntervals . reDumpStats)
    , ranges "layout:data-ranges" "Address ranges of `.data` sections." (dsDataIntervals . reDumpStats)
    , ranges "mem:instr-ranges" "Address ranges the run fetched instructions from." (alInstr . finalLog)
    , ranges "mem:data-ranges" "Address ranges the run read or wrote data at." (alData . finalLog)
    , ranges "mem:io-ranges" "Memory-mapped IO addresses the run accessed." (alIo . finalLog)
    , flagView "memory:table" "Address-space table: sections, IO, free spans and their access coverage." $ \e ->
        renderMemoryTable (reDumpStats e) (memoryDump (final e))
    , checkedView
        (lit "memory" *> ((,) <$> arg "from" <*> arg "to"))
        "Memory dump from `<from>` to `<to>`, inclusive."
        checkRange
        (\e (from, to) -> viewMemory (current e) from to)
    , checkedView
        (lit "io" *> ((,) <$> arg "addr" <*> format [Dec, Hex, Sym] Dec))
        "IO port at `<addr>`: `input >>> output` so far; `sym` shows characters."
        (\env (addr, _) -> checkPort env addr)
        (\e (addr, fmt) -> viewIO fmt addr (current e))
    , flagView "isa-specific" "The ISA's run-wide stats, one `name: value` per line; empty if it has none." $
        isaSpecific . reState
    ]
    where
        current = seState . reState
        final = seFinal . reState
        finalLog = accessLog . memoryDump . final
        runTotal = memAccessCount . memoryDump . current
        count name doc f = mkView (lit name *> formatRequired [Dec, Hex]) doc $ \e fmt -> showValue fmt (f e)
        layout name doc f = flagView name doc (show . f . reDumpStats)
        ranges name doc f = mkView (lit name *> format [Dec, Hex] Hex) doc $ \e fmt ->
            (if fmt == Hex then renderIntervalsHex else renderIntervals) (f e)
        pcLabel ReportEnv{reState = StateEnv{seLabels, seState}} =
            case [l | (l, a) <- toPairs seLabels, a == toEnum (programCounter seState)] of
                l : _ -> "@" <> l
                [] -> ""
        nextInstruction st
            | isHalted st = "-"
            | otherwise = either (const "-") (show . snd) (readInstruction (memoryDump st) (programCounter st))
        isaSpecific env =
            let width = foldr (max . T.length . statName) 0 (isaStats @st) + 1
             in T.intercalate
                    "\n"
                    [T.justifyLeft width ' ' (statName <> ":") <> " " <> statRender env | Stat{statName, statRender} <- isaStats @st]
        checkRange CheckEnv{ceMemorySize} (from, to)
            | from < 0 || to >= ceMemorySize =
                Left $ "range " <> show from <> ".." <> show to <> " is outside memory 0.." <> show (ceMemorySize - 1)
            | from > to = Left $ "range " <> show from <> ".." <> show to <> " is empty: <from> is after <to>"
            | otherwise = Right ()
        checkPort CheckEnv{ceIoPorts} addr
            | addr `elem` ceIoPorts = Right ()
            | otherwise =
                Left $
                    "no IO port at "
                        <> hex addr
                        <> case ceIoPorts of
                            [] -> ", memory_mapped_io is empty"
                            ports -> ", configured: " <> T.intercalate ", " (map hex (sort ports))
        hex a = "0x" <> toText (showHex a "")

-- | A register-like value: @dec@ by default, or @hex@.
valueView :: (Integral a, Show a) => Text -> Text -> (env -> a) -> View env
valueView name doc f = mkView (lit name *> format [Dec, Hex] Dec) doc $ \e fmt -> showValue fmt (f e)

-- | A view without arguments or format.
flagView :: Text -> Text -> (env -> Text) -> View env
flagView name doc f = mkView (lit name) doc (\e () -> f e)

showValue :: (Integral a, Show a) => Format -> a -> Text
showValue Hex = toText . word32ToHex
showValue _ = show

-- | Values joined top first, @a:b:c@, as stacks are shown.
showValues :: (Integral a, Show a) => Format -> [a] -> Text
showValues fmt = T.intercalate ":" . map (showValue fmt)

-- | A report with its view template compiled against the ISA's views.
data Report st = Report
    { rConf :: ReportConf
    , rTemplate :: Maybe (Template (ReportEnv st))
    }

-- | Compile every report before the simulation runs, so a mistake fails
-- the same way in every ISA, naming the report and the key, and rendering
-- never meets a key it cannot resolve.
compileReports :: forall st. (ReportViews st) => CheckEnv -> [ReportConf] -> Either Text [Report st]
compileReports checkEnv confs =
    case partitionEithers (zipWith compile [1 :: Int ..] confs) of
        ([], reports) -> Right reports
        (errs, _) -> Left $ T.intercalate "\n" (ordNub (concat errs))
    where
        compile i rc@ReportConf{rcName, rcView} = case rcView of
            Nothing -> Right (Report rc Nothing)
            Just text ->
                bimap
                    (map (("report " <> maybe ("#" <> show i) show rcName <> ": ") <>))
                    (Report rc . Just)
                    (compileTemplate checkEnv (reportViews @st) text)

-- | Render one of the ISA's own views of a state.
renderIsaView :: forall st. (Inspectable st) => HashMap Text (WordOf st) -> st -> Text -> Either Text Text
renderIsaView labels st key = do
    template <- first (T.intercalate "\n") $ compileTemplate unchecked (isaViews @st) ("{" <> key <> "}")
    return $ renderTemplate template StateEnv{seLabels = labels, seState = st, seFinal = st}
    where
        unchecked = CheckEnv{ceMemorySize = maxBound, ceIoPorts = []}

prepareReport ::
    forall st mem.
    (ReportViews st) =>
    TranslatorResult mem (WordOf st)
    -> Bool
    -> st
    -> [Trace st (IsaOf st)]
    -> Report st
    -> (Bool, Text)
prepareReport
    TranslatorResult{labels, dumpStats}
    verbose
    finalState
    records
    Report{rConf = rc@ReportConf{rcName, rcSlice, rcAssert}, rTemplate} =
        let header = maybe "" ("# " <>) rcName
            details = if verbose then show rc else ""
            sliced = selectSlice rcSlice (withAccessDeltas records)
            stateViews = case rTemplate of
                Nothing -> ""
                Just template ->
                    T.concat
                        $ filter (not . T.null)
                        $ map
                            ( \(accesses, record) -> case record of
                                TState{tInstructionCount, tPrevInstruction, tState} ->
                                    renderTemplate
                                        template
                                        ReportEnv
                                            { reState = StateEnv{seLabels = labels, seState = tState, seFinal = finalState}
                                            , reDumpStats = dumpStats
                                            , reInstrCount = tInstructionCount
                                            , rePrevInstr = tPrevInstruction
                                            , reAccesses = accesses
                                            }
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

-- | Pair each state record with the accesses the instruction that produced
-- it made. One record per step, so consecutive totals differ by one
-- instruction's worth. Before slicing, so a slice still gets real deltas.
withAccessDeltas ::
    forall st.
    (Inspectable st, Memory (MemOf st) (IsaOf st) (WordOf st)) =>
    [Trace st (IsaOf st)]
    -> [(AccessCount, Trace st (IsaOf st))]
withAccessDeltas = go emptyAccessCount
    where
        go _ [] = []
        go before (record : rest) = case record of
            TState{tState} ->
                let after = memAccessCount (memoryDump tState)
                 in (acSince before after, record) : go after rest
            _ -> (emptyAccessCount, record) : go before rest

selectSlice (HeadSlice n) = take n
selectSlice AllSlice = id
selectSlice (TailSlice n) = reverse . take n . reverse
selectSlice LastSlice = take 1 . reverse

-----------------------------------------------------------

-- | Render a full address-space table: one row per declared section, IO
--   cluster, or @x@ (anything else) span. Auto-sized columns, hex addresses
--   widened to fit @memory_size@.
renderMemoryTable ::
    forall m isa w.
    (IsWord w, Memory m isa w) =>
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

viewMemory :: forall st. (ReportViews st) => st -> Int -> Int -> Text
viewMemory st from to =
    toText
        $ prettyDump True (hexAddr (hexAddrWidth (memCapacity mem))) mempty
        $ fromList
        $ sliceMem [from .. to] (dumpCells mem)
    where
        mem = memoryDump st

-- | The port was checked against the configuration when the template was
-- compiled, and ports come only from there.
viewIO :: (Inspectable st, IsWord (WordOf st)) => Format -> Int -> st -> Text
viewIO fmt addr st = maybe "-" render (ioStreams st !? addr)
    where
        render (is, os) = case fmt of
            Hex -> T.replace "\"" "" $ show (map word32ToHex is) <> " >>> " <> show (map word32ToHex (reverse os))
            Sym -> T.replace "\\NUL" "\\0" $ show (map sym is) <> " >>> " <> show (map sym (reverse os))
            _ -> show is <> " >>> " <> show (reverse os)
        sym w = case fromEnum w of
            0 -> '\0'
            10 -> '\n'
            x | 32 <= x && x <= 126 -> chr x
            _ -> '?'
