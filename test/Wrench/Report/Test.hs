module Wrench.Report.Test (tests) where

import Data.Default (def)
import Data.Text qualified as T
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Wrench.Config (Config (..))
import Wrench.Isa.Acc32 (Acc32St)
import Wrench.Isa.F32a (F32aSt)
import Wrench.Isa.M68k (M68kSt)
import Wrench.Isa.RiscIv (RiscIvSt)
import Wrench.Isa.VliwIv (VliwIvSt)
import Wrench.Isa.Wasm32 ()
import Wrench.Isa.Wasm32.State (Wasm32St)
import Wrench.Report (
    ReportConf (..),
    ReportSlice (..),
    ReportViews,
    TracePlan (..),
    compileReports,
    isaViews,
    sharedViews,
    tracePlan,
 )
import Wrench.Report.View
import Wrench.Wrench (Options (maxStateLogLimit), Result (rTrace), wrench)

tests :: TestTree
tests =
    testGroup
        "Views"
        [ testCase "A template fills each hole once" $ do
            rendered "" @?= Right ""
            rendered "pc={pc} label={pc:label}" @?= Right "pc=pc:Dec label=pc:label"
            rendered "a { b" @?= Right "a { b"
            rendered "a } {pc:hex} {" @?= Right "a } pc:Hex {"
            rendered "{echo} {pc}" @?= Right "{pc} pc:Dec"
        , testCase "Default format is filled in" $ do
            rendered "{frames:hex:2}" @?= Right "frames:Hex:Just 2"
            rendered "{frames}" @?= Right "frames:Dec:Nothing"
        , testCase "Longest literal prefix wins" $ do
            rendered "{mem:table}" @?= Right "mem:table"
            rendered "{mem:0:0x0f}" @?= Right "mem:0:15"
            "usage: {mem:table}" `isErrorFor` "mem:table:hex"
        , testCase "An old name still works, with a warning naming the new one" $ do
            rendered "{memory:0:0x0f}" @?= Right "mem:0:15"
            warnings "{memory:0:0x0f} {memory:table} {mem:table}"
                @?= ["{memory:0:0x0f} is renamed, use {mem:0:0x0f}", "{memory:table} is renamed, use {mem:table}"]
            "usage: {memory:table}" `isErrorFor` "memory:table:hex"
        , testCase "A template names the views it uses, renamed ones by the new name" $
            templateViews . snd <$> compileTemplate checkEnv testViews "{pc:hex} x {memory:table} {mem:0:4}"
                @?= Right ["pc", "mem:table", "mem"]
        , testCase "The trace keeps what the largest slices need, at most the limit" $ do
            plan [LastSlice] @?= Right (TracePlan 0 1)
            plan [HeadSlice 5, TailSlice 3, LastSlice] @?= Right (TracePlan 5 3)
            plan [HeadSlice 500, AllSlice] @?= Right (TracePlan 100 100)
            plan [] @?= Right (TracePlan 0 0)
        , testCase "mem:per-instruction is renamed to profile" $
            either id fst (compileTemplate checkEnv (sharedViews @(RiscIvSt Int32)) "{mem:per-instruction}")
                @?= ["{mem:per-instruction} is renamed, use {profile}"]
        , testCase "Errors name the problem and the usage" $ do
            "unknown format 'bin', one of: dec, hex, usage: {pc[:dec|hex]}" `isErrorFor` "pc:bin"
            "missing format, one of: dec, hex" `isErrorFor` "count"
            "<frames> should be a number, got 'x'" `isErrorFor` "frames:dec:x"
            "<from> should be a number" `isErrorFor` "mem:a:b"
        , testCase "Arguments are checked against the configuration" $ do
            "{mem:0:16}: range 0..16 is outside memory 0..15" `isErrorFor` "mem:0:16"
            "range 8..4 is empty" `isErrorFor` "mem:8:4"
        , testCase "Unknown keys suggest close keywords" $ do
            "unknown view {t2}, did you mean {T1[:dec|hex]}?" `isErrorFor` "t2"
            "unknown view {memry:table}, did you mean {mem:table}?" `isErrorFor` "memry:table"
            rendered "{foo}" @?= Left ["unknown view {foo}"]
        , testGroup
            "Every example key selects its own view, and only it"
            [ unambiguous "acc32" (allViews @(Acc32St Int32))
            , unambiguous "f32a" (allViews @(F32aSt Int32))
            , unambiguous "m68k" (allViews @(M68kSt Int32))
            , unambiguous "risc-iv-32" (allViews @(RiscIvSt Int32))
            , unambiguous "vliw-iv" (allViews @(VliwIvSt Int32))
            , unambiguous "wasm32" (allViews @(Wasm32St Int32))
            ]
        , viewsReference
        , testGroup
            "State log limit"
            [ testCase "A failed run keeps its error at any limit" $ do
                failing 5 [HeadSlice 8] @?= [["0", "1", "2", "3", "4", dropped 45, limitError]]
                failing 0 [HeadSlice 8] @?= [[dropped 50, limitError]]
                failing 0 [AllSlice] @?= [[dropped 50, limitError]]
                failing 0 [LastSlice] @?= [[limitError]]
                failing 5 [LastSlice] @?= [[limitError]]
                failing 5 [TailSlice 2] @?= [["49", limitError]]
            , testCase "A slice over the limit stays at its end of the run" $ do
                halting 3 [HeadSlice 8, TailSlice 8]
                    @?= [["0", "1", "2", dropped 8], [dropped 8, "8", "9", "10"]]
                failing 3 [HeadSlice 8, TailSlice 8]
                    @?= [["0", "1", "2", dropped 47, limitError], [dropped 47, "47", "48", "49", limitError]]
                halting 3 [AllSlice] @?= [[dropped 8, "8", "9", "10"]]
                halting 0 [LastSlice] @?= [[dropped 11]]
            , testCase "A slice within the limit shows what it asked for, with no warning" $ do
                halting 5 [HeadSlice 3, TailSlice 3, LastSlice] @?= [["0", "1", "2"], ["8", "9", "10"], ["10"]]
                failing 5 [HeadSlice 3, TailSlice 3] @?= [["0", "1", "2"], ["48", "49", limitError]]
            , testCase "A limit above the run drops nothing" $ do
                let run = map show [0 .. 10 :: Int]
                halting 11 [AllSlice, HeadSlice 20, TailSlice 20] @?= [run, run, run]
                failing 50 [HeadSlice 60] @?= [map show [0 .. 49 :: Int] <> [limitError]]
            , testCase "A report does not depend on the others" $
                forM_ [0, 1, 2, 3, 4, 5, 6, 8, 10, 11, 12] $ \limit -> do
                    let slices = [AllSlice, HeadSlice 2, HeadSlice 8, TailSlice 2, TailSlice 8, LastSlice]
                    halting limit slices @?= concatMap (\s -> halting limit [s]) slices
                    failing limit slices @?= concatMap (\s -> failing limit [s]) slices
            , testCase "Access counts and the profile cover the dropped states" $ do
                shown "{mem:accesses}\n" haltingSrc 1000 2 [TailSlice 2] @?= [["1", "1"]]
                shown "{profile}\n" haltingSrc 1000 1 [LastSlice] @?= shown "{profile}\n" haltingSrc 1000 100 [LastSlice]
            ]
        ]

-- | What each slice of a run shows, a state as its instruction count: 50 steps
-- up to the instruction limit, or 11 states up to a halt.
failing, halting :: Int -> [ReportSlice] -> [[Text]]
failing = shown "{sim:instruction-count}\n" "    .text\n_start:\n    j        _start\n" 50
halting = shown "{sim:instruction-count}\n" haltingSrc 1000

haltingSrc :: String
haltingSrc = toString $ unlines $ ["    .text", "_start:"] <> replicate 9 "    addi     t1, t1, 1" <> ["    halt"]

shown :: Text -> String -> Int -> Int -> [ReportSlice] -> [[Text]]
shown view src cLimit stateLogLimit slices =
    either (error . ("simulation failed: " <>)) (drop 1 . map T.lines . T.splitOn "---\n" . rTrace) $
        wrench @(RiscIvSt Int32) [] def{maxStateLogLimit = stateLogLimit} conf src
    where
        conf =
            def
                { cLimit
                , cReports = Just [ReportConf{rcName = Nothing, rcSlice = s, rcAssert = Nothing, rcView = Just view} | s <- slices]
                }

dropped :: Int -> Text
dropped n = "WARN: Dropped " <> show n <> " states"

limitError :: Text
limitError = "ERROR: Simulation limit reached"

-- | Test views that render the key they parsed.
testViews :: [View ()]
testViews =
    withRenames
        [("memory:table", "mem:table"), ("memory", "mem")]
        [ mkView (lit "pc" *> format [Dec, Hex] Dec) "" (\_ f -> "pc:" <> show f)
        , mkView (lit "pc:label") "" (\_ () -> "pc:label")
        , mkView (lit "mem:table") "" (\_ () -> "mem:table")
        , checkedView (lit "mem" *> ((,) <$> arg "from" <*> arg "to")) "" range $ \_ (from, to) ->
            "mem:" <> show from <> ":" <> show to
        , mkView (lit "count" *> formatRequired [Dec, Hex]) "" (\_ f -> show f)
        , mkView (lit "frames" *> ((,) <$> format [Dec, Hex] Dec <*> optArg "frames")) "" $ \_ (f, n) ->
            "frames:" <> show f <> ":" <> show n
        , mkView (lit "T1" *> format [Dec, Hex] Dec) "" (\_ _ -> "T1")
        , mkView (lit "echo") "" (\_ () -> "{pc}")
        ]
    where
        range CheckEnv{ceMemorySize} (from, to)
            | to >= ceMemorySize =
                Left $ "range " <> show from <> ".." <> show to <> " is outside memory 0.." <> show (ceMemorySize - 1)
            | from > to = Left $ "range " <> show from <> ".." <> show to <> " is empty"
            | otherwise = Right ()

checkEnv :: CheckEnv
checkEnv = CheckEnv{ceMemorySize = 16, ceIoPorts = [0]}

plan :: [ReportSlice] -> Either Text TracePlan
plan slices =
    tracePlan 100 . snd
        <$> compileReports @(RiscIvSt Int32)
            checkEnv
            [ReportConf{rcName = Nothing, rcSlice = s, rcAssert = Nothing, rcView = Nothing} | s <- slices]

rendered :: Text -> Either [Text] Text
rendered input = (`renderTemplate` ()) . snd <$> compileTemplate checkEnv testViews input

warnings :: Text -> [Text]
warnings input = either id fst (compileTemplate checkEnv testViews input)

isErrorFor :: Text -> Text -> Assertion
isErrorFor expected key = case rendered ("{" <> key <> "}") of
    Left errs | any (expected `T.isInfixOf`) errs -> pass
    other -> assertFailure $ "expected an error with " <> show expected <> ", got " <> show other

-- | What the ambiguity check needs of a view: its usage, its example keys,
-- and whether it accepts a key. Drops the environment, so the ISA's views
-- and the shared ones can sit in one list.
data Probe = Probe
    { pUsage :: Text
    , pExamples :: [Text]
    , pAccepts :: Text -> Bool
    }

allViews :: forall st. (ReportViews st) => [Probe]
allViews = map probe (isaViews @st) <> map probe (sharedViews @st)
    where
        probe v =
            Probe
                { pUsage = viewUsage v
                , pExamples = examples v
                , pAccepts = \key -> isRight (compileTemplate checkEnv [v] ("{" <> key <> "}"))
                }

-- | Each example key of each view parses under that view and no other:
-- no view shadows another, and the usage the docs print is what the parser
-- takes.
unambiguous :: String -> [Probe] -> TestTree
unambiguous name probes = testCase name $
    forM_ probes $ \Probe{pUsage, pExamples} -> forM_ pExamples $ \key -> do
        let takers = [usage' | Probe{pUsage = usage', pAccepts} <- probes, pAccepts key]
        when (takers /= [pUsage])
            $ assertFailure
            $ toString
            $ "{" <> key <> "}, an example of " <> pUsage <> ", is accepted by " <> show takers

-- | @docs/report-views.md@, generated from the views themselves so it cannot
-- fall behind them.
viewsReference :: TestTree
viewsReference =
    goldenVsString "Every report view, documented" "docs/report-views.md"
        $ return
        $ encodeUtf8
        $ T.intercalate "\n"
        $ [ "# Report views\n"
          , "<!-- Generated from the source by `make test-accept`. Do not edit. -->\n"
          , "Every expression a report `view` can use inside `{...}`. `[...]` is optional, `a|b` is a choice, `<name>` is a number (decimal or `0x` hex). See [Reports configuration](README.md#reports-configuration).\n"
          ]
            <> section "All ISAs" (sharedViews @(RiscIvSt Int32))
            <> section "acc32" (isaViews @(Acc32St Int32))
            <> section "f32a" (isaViews @(F32aSt Int32))
            <> section "m68k" (isaViews @(M68kSt Int32))
            <> section "risc-iv-32" (isaViews @(RiscIvSt Int32))
            <> section "vliw-iv" (isaViews @(VliwIvSt Int32))
            <> section "wasm32" (isaViews @(Wasm32St Int32))
    where
        section name vs = ["## " <> name <> "\n", viewsTable vs]
