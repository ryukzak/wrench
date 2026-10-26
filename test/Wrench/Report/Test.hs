module Wrench.Report.Test (tests) where

import Data.Text qualified as T
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Wrench.Isa.Acc32 (Acc32St)
import Wrench.Isa.F32a (F32aSt)
import Wrench.Isa.M68k (M68kSt)
import Wrench.Isa.RiscIv (RiscIvSt)
import Wrench.Isa.VliwIv (VliwIvSt)
import Wrench.Isa.Wasm32 ()
import Wrench.Isa.Wasm32.State (Wasm32St)
import Wrench.Machine.Types (Inspectable (..))
import Wrench.Report (sharedViews, substituteBrackets)
import Wrench.Report.View

tests :: TestTree
tests =
    testGroup
        "ISA"
        [ testCase "Extract brackets" $ do
            substituteBrackets (const "_") "" @?= ""
            substituteBrackets (const "_") "foo {bar} h {g} {z}" @?= "foo _ h _ _"
            substituteBrackets (const "{") "foo {bar} h {g} {z}" @?= "foo { h { {"
        , testCase "Default format is filled in" $ do
            matched "pc" @?= Right ("pc", Match [] "dec")
            matched "pc:hex" @?= Right ("pc", Match [] "hex")
            matched "frames:hex:2" @?= Right ("frames", Match [2] "hex")
            matched "frames" @?= Right ("frames", Match [] "dec")
        , testCase "Longest literal prefix wins" $ do
            matched "pc:label" @?= Right ("pc:label", Match [] "")
            matched "memory:table" @?= Right ("memory:table", Match [] "")
            matched "memory:0:0x10" @?= Right ("memory", Match [0, 16] "")
            "usage: {memory:table}" `isErrorFor` "memory:table:hex"
        , testCase "Errors name the problem and the usage" $ do
            "unknown format 'bin', one of: dec, hex, usage: {pc[:dec|hex]}" `isErrorFor` "pc:bin"
            "missing format, one of: dec, hex" `isErrorFor` "count"
            "<frames> should be a number, got 'x'" `isErrorFor` "frames:dec:x"
            "<from> should be a number" `isErrorFor` "memory:a:b"
        , testCase "Arguments are checked against the configuration" $ do
            checkView env testViews "memory:0:15" @?= Right ()
            "{memory:0:16}: range 0..16 is outside memory 0..15" `isErrorFor` "memory:0:16"
            "range 8..4 is empty" `isErrorFor` "memory:8:4"
        , testCase "Unknown keys suggest close keywords" $ do
            "unknown view {t2}, did you mean {T1[:dec|hex]}?" `isErrorFor` "t2"
            matched "foo" @?= Left "unknown view {foo}"
        , viewsReference
        ]

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
          , "## All ISAs\n"
          , viewsTable (sharedViews @(RiscIvSt Int32))
          ]
            <> concat
                [ ["## " <> name <> "\n", table]
                | (name, table) <-
                    [ ("acc32", viewsTable (views @(Acc32St Int32)))
                    , ("f32a", viewsTable (views @(F32aSt Int32)))
                    , ("m68k", viewsTable (views @(M68kSt Int32)))
                    , ("risc-iv-32", viewsTable (views @(RiscIvSt Int32)))
                    , ("vliw-iv", viewsTable (views @(VliwIvSt Int32)))
                    , ("wasm32", viewsTable (views @(Wasm32St Int32)))
                    ]
                ]

testViews :: [View ()]
testViews =
    [ plain (valueKey "pc")
    , plain (flagKey "pc:label")
    , plain (flagKey "memory:table")
    , (plain (keyword "memory" [Arg "from", Arg "to"])){vCheck = range}
    , plain (keyword "count" [Fmt ["dec", "hex"] Nothing])
    , plain (keyword "frames" [valueFormat, OptArg "frames"])
    , plain (valueKey "T1")
    ]
    where
        plain k = mkView k "" (\_ _ -> "")
        range CheckEnv{ceMemorySize} Match{mArgs = [from, to]}
            | to >= ceMemorySize =
                Left $ "range " <> show from <> ".." <> show to <> " is outside memory 0.." <> show (ceMemorySize - 1)
            | from > to = Left $ "range " <> show from <> ".." <> show to <> " is empty"
        range _ _ = Right ()

env :: CheckEnv
env = CheckEnv{ceMemorySize = 16, ceIoPorts = []}

matched :: Text -> Either Text (Text, Match)
matched key = first' <$> matchView testViews key
    where
        first' (View{vKeyword}, m) = (keywordName vKeyword, m)

isErrorFor :: Text -> Text -> Assertion
isErrorFor expected key = case checkView env testViews key of
    Left err | expected `T.isInfixOf` err -> pass
    other -> assertFailure $ "expected an error with " <> show expected <> ", got " <> show other
