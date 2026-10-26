module Wrench.Report.Test (tests) where

import Data.Text qualified as T
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Wrench.Report (substituteBrackets)
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
