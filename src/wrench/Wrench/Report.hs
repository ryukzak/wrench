{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Wrench.Report (
    ReportSlice (..),
    prepareReport,
    checkReportViews,
    renderIsaView,
    sharedViews,
    ReportConf (..),
    substituteBrackets,
    viewValue,
    viewValues,
) where

import Data.Aeson (FromJSON (..), Value (..), genericParseJSON, rejectUnknownFields)
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Text qualified as T
import Numeric (showHex)
import Relude
import Relude.Extra
import Text.Regex.TDFA (getAllTextMatches, (=~))
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Report.View
import Wrench.Translator (TranslatorResult (..))

bracketRegex :: Text
bracketRegex = "\\{([^}]*)\\}"

-- | The keys a view uses, without braces.
viewKeys :: Text -> [Text]
viewKeys input = ordNub $ map (T.tail . T.init) $ getAllTextMatches (input =~ bracketRegex)

substituteBrackets :: (Text -> Text) -> Text -> Text
substituteBrackets f input =
    let matches = getAllTextMatches (input =~ bracketRegex)
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

-- | Every view a report on this ISA can use: the ISA's own, then the shared.
reportViews :: forall st. (ReportViews st) => [View (ReportEnv st)]
reportViews = map (mapEnv reState) (views @st) <> sharedViews

-- | The views every ISA has.
sharedViews :: forall st. (ReportViews st) => [View (ReportEnv st)]
sharedViews = commonViews (views @st)

commonViews :: forall st. (ReportViews st) => [View (StateEnv st)] -> [View (ReportEnv st)]
commonViews isaViews =
    [ mkView (valueKey "pc") "Program counter." $ \e m -> viewValue m (programCounter (current e))
    , mkView (flagKey "pc:label") "`@label` if the program counter is at a label, otherwise empty." $ \e _ -> pcLabel e
    , mkView (flagKey "instruction:prev") "The instruction that produced this state, `-` for the initial state." $ \e _ ->
        maybe "-" show (rePrevInstr e)
    , mkView (flagKey "instruction:next") "The instruction to execute next, `-` once halted." $ \e _ ->
        nextInstruction (current e)
    , mkView (flagKey "sim:instruction-count") "Instructions executed so far." $ \e _ -> show (reInstrCount e)
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
    , mkView (flagKey "memory:table") "Address-space table: sections, IO, free spans and their access coverage." $ \e _ ->
        renderMemoryTable (reDumpStats e) (memoryDump (final e))
    , ( mkView (keyword "memory" [Arg "from", Arg "to"]) "Memory dump from `<from>` to `<to>`, inclusive." $ \e m -> viewMemory (current e) m
      )
        { vCheck = checkRange
        }
    , ( mkView
            (keyword "io" [Arg "addr", Fmt ["dec", "hex", "sym"] (Just "dec")])
            "IO port at `<addr>`: `input >>> output` so far; `sym` shows characters."
            $ \e m -> viewIO m (current e)
      )
        { vCheck = checkPort
        }
    , mkView (flagKey "isa-specific") "The ISA's run-wide stats, one `name: value` per line; empty if it has none." $ \e _ ->
        isaSpecific (reState e)
    ]
    where
        current = seState . reState
        final = seFinal . reState
        finalLog = accessLog . memoryDump . final
        runTotal = memAccessCount . memoryDump . current
        count name doc f = mkView (keyword name [Fmt ["dec", "hex"] Nothing]) doc $ \e m -> viewValue m (f e)
        layout name doc f = mkView (flagKey name) doc $ \e _ -> show (f (reDumpStats e))
        ranges name doc f = mkView (keyword name [Fmt ["dec", "hex"] (Just "hex")]) doc $ \e m ->
            (if isHex m then renderIntervalsHex else renderIntervals) (f e)
        pcLabel ReportEnv{reState = StateEnv{seLabels, seState}} =
            case [l | (l, a) <- toPairs seLabels, a == toEnum (programCounter seState)] of
                l : _ -> "@" <> l
                [] -> ""
        nextInstruction st
            | isHalted st = "-"
            | otherwise = either (const "-") (show . snd) (readInstruction (memoryDump st) (programCounter st))
        summaries = [v | v@View{vSummary = True} <- isaViews]
        isaSpecific env =
            let width = foldr (max . T.length . keywordName . vKeyword) 0 summaries + 1
             in T.intercalate
                    "\n"
                    [ T.justifyLeft width ' ' (keywordName vKeyword <> ":") <> " " <> vRender env (Match [] "")
                    | View{vKeyword, vRender} <- summaries
                    ]
        checkRange CheckEnv{ceMemorySize} Match{mArgs} = case mArgs of
            [from, to]
                | from < 0 || to >= ceMemorySize ->
                    Left $ "range " <> show from <> ".." <> show to <> " is outside memory 0.." <> show (ceMemorySize - 1)
                | from > to -> Left $ "range " <> show from <> ".." <> show to <> " is empty: <from> is after <to>"
            _ -> Right ()
        checkPort CheckEnv{ceIoPorts} Match{mArgs} = case mArgs of
            [addr]
                | addr `notElem` ceIoPorts ->
                    Left $
                        "no IO port at "
                            <> hex addr
                            <> case ceIoPorts of
                                [] -> ", memory_mapped_io is empty"
                                ports -> ", configured: " <> T.intercalate ", " (map hex (sort ports))
            _ -> Right ()
        hex a = "0x" <> toText (showHex a "")

-- | Check every view key of every report before the simulation runs, so a
-- mistake fails the same way in every ISA, naming the report and the key.
checkReportViews :: forall st. (ReportViews st) => st -> [ReportConf] -> Either Text ()
checkReportViews st reports =
    case errors of
        [] -> Right ()
        errs -> Left $ T.intercalate "\n" errs
    where
        env = CheckEnv{ceMemorySize = memCapacity (memoryDump st), ceIoPorts = keys (ioStreams st)}
        errors =
            [ "report " <> maybe ("#" <> show i) show rcName <> ": " <> err
            | (i :: Int, ReportConf{rcName, rcView}) <- zip [1 ..] reports
            , key <- maybe [] viewKeys rcView
            , Left err <- [checkView env (reportViews @st) key]
            ]

-- | Render one of the ISA's own views of a state.
renderIsaView :: forall st. (Inspectable st) => HashMap Text (WordOf st) -> st -> Text -> Either Text Text
renderIsaView labels st key = do
    (View{vRender}, m) <- matchView (views @st) key
    return $ vRender StateEnv{seLabels = labels, seState = st, seFinal = st} m

prepareReport ::
    forall st mem.
    (ReportViews st) =>
    TranslatorResult mem (WordOf st)
    -> Bool
    -> st
    -> [Trace st (IsaOf st)]
    -> ReportConf
    -> (Bool, Text)
prepareReport
    TranslatorResult{labels, dumpStats}
    verbose
    finalState
    records
    rc@ReportConf{rcName, rcSlice, rcAssert, rcView} =
        let header = maybe "" ("# " <>) rcName
            details = if verbose then show rc else ""
            sliced = selectSlice rcSlice (withAccessDeltas records)
            stateViews = case rcView of
                Nothing -> ""
                Just rvView' ->
                    let matched :: HashMap Text (View (ReportEnv st), Match)
                        matched = fromList [(key, either error id (matchView reportViews key)) | key <- viewKeys rvView']
                        render env key = case matched !? key of
                            Just (View{vRender}, m) -> vRender env m
                            Nothing -> error $ "view error: " <> key
                     in T.concat
                            $ filter (not . T.null)
                            $ map
                                ( \(accesses, record) -> case record of
                                    TState{tInstructionCount, tPrevInstruction, tState} ->
                                        substituteBrackets
                                            ( render
                                                ReportEnv
                                                    { reState = StateEnv{seLabels = labels, seState = tState, seFinal = finalState}
                                                    , reDumpStats = dumpStats
                                                    , reInstrCount = tInstructionCount
                                                    , rePrevInstr = tPrevInstruction
                                                    , reAccesses = accesses
                                                    }
                                            )
                                            rvView'
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

viewMemory :: forall st. (ReportViews st) => st -> Match -> Text
viewMemory st Match{mArgs} = case mArgs of
    [from, to] ->
        toText
            $ prettyDump True (hexAddr (hexAddrWidth (memCapacity mem))) mempty
            $ fromList
            $ sliceMem [from .. to] (dumpCells mem)
    _ -> error "memory view: expected <from>:<to>"
    where
        mem = memoryDump st

viewIO :: (Inspectable st, IsWord (WordOf st)) => Match -> st -> Text
viewIO Match{mArgs, mFormat} st = case mArgs of
    [addr] | Just (is, os) <- ioStreams st !? addr -> case mFormat of
        "hex" -> T.replace "\"" "" $ show (map word32ToHex is) <> " >>> " <> show (map word32ToHex (reverse os))
        "sym" -> T.replace "\\NUL" "\\0" $ show (map sym is) <> " >>> " <> show (map sym (reverse os))
        _ -> show is <> " >>> " <> show (reverse os)
    _ -> error $ "io view: no port at " <> show mArgs
    where
        sym w = case fromEnum w of
            0 -> '\0'
            10 -> '\n'
            x | 32 <= x && x <= 126 -> chr x
            _ -> '?'

-- | A register-like value in the format the view asked for.
viewValue :: (Integral a, Show a) => Match -> a -> Text
viewValue m = if isHex m then toText . word32ToHex else show

-- | Values joined top first, @a:b:c@, as stacks are shown.
viewValues :: (Integral a, Show a) => Match -> [a] -> Text
viewValues m = T.intercalate ":" . map (viewValue m)
