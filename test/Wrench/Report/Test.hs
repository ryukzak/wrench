module Wrench.Report.Test (tests) where

import Data.Text qualified as T
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Wrench.Isa.Acc32 (Acc32St)
import Wrench.Isa.F32a (F32aSt)
import Wrench.Isa.M68k (M68kSt)
import Wrench.Isa.RiscIv (RiscIvSt)
import Wrench.Isa.VliwIv (VliwIvSt)
import Wrench.Isa.Wasm32 ()
import Wrench.Isa.Wasm32.State (Wasm32St)
import Wrench.Report (ReportViews, isaViews, sharedViews)
import Wrench.Report.View

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
            rendered "{memory:table}" @?= Right "memory:table"
            rendered "{memory:0:0x0f}" @?= Right "memory:0:15"
            "usage: {memory:table}" `isErrorFor` "memory:table:hex"
        , testCase "Errors name the problem and the usage" $ do
            "unknown format 'bin', one of: dec, hex, usage: {pc[:dec|hex]}" `isErrorFor` "pc:bin"
            "missing format, one of: dec, hex" `isErrorFor` "count"
            "<frames> should be a number, got 'x'" `isErrorFor` "frames:dec:x"
            "<from> should be a number" `isErrorFor` "memory:a:b"
        , testCase "Arguments are checked against the configuration" $ do
            "{memory:0:16}: range 0..16 is outside memory 0..15" `isErrorFor` "memory:0:16"
            "range 8..4 is empty" `isErrorFor` "memory:8:4"
        , testCase "Unknown keys suggest close keywords" $ do
            "unknown view {t2}, did you mean {T1[:dec|hex]}?" `isErrorFor` "t2"
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
        ]

-- | Test views that render the key they parsed.
testViews :: [View ()]
testViews =
    [ mkView (lit "pc" *> format [Dec, Hex] Dec) "" (\_ f -> "pc:" <> show f)
    , mkView (lit "pc:label") "" (\_ () -> "pc:label")
    , mkView (lit "memory:table") "" (\_ () -> "memory:table")
    , checkedView (lit "memory" *> ((,) <$> arg "from" <*> arg "to")) "" range $ \_ (from, to) ->
        "memory:" <> show from <> ":" <> show to
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

rendered :: Text -> Either [Text] Text
rendered input = (`renderTemplate` ()) <$> compileTemplate checkEnv testViews input

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
