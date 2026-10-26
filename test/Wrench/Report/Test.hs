module Wrench.Report.Test (tests) where

import Data.Text qualified as T
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Wrench.Report (substituteBrackets)
import Wrench.Report.Keyword

tests :: TestTree
tests =
    testGroup
        "ISA"
        [ testCase "Extract brackets" $ do
            substituteBrackets (const "_") "" @?= ""
            substituteBrackets (const "_") "foo {bar} h {g} {z}" @?= "foo _ h _ _"
            substituteBrackets (const "{") "foo {bar} h {g} {z}" @?= "foo { h { {"
        , testCase "Default format is filled in" $ do
            normalizeKey keywords "pc" @?= Right "pc:dec"
            normalizeKey keywords "pc:hex" @?= Right "pc:hex"
            normalizeKey keywords "frames:hex:2" @?= Right "frames:hex:2"
        , testCase "Longest literal prefix wins" $ do
            normalizeKey keywords "pc:label" @?= Right "pc:label"
            normalizeKey keywords "memory:table" @?= Right "memory:table"
            normalizeKey keywords "memory:0:0x10" @?= Right "memory:0:0x10"
            "usage: {memory:table}" `isErrorFor` "memory:table:hex"
        , testCase "Errors name the problem and the usage" $ do
            "unknown format 'bin', one of: dec, hex, usage: {pc[:dec|hex]}" `isErrorFor` "pc:bin"
            "missing format, one of: dec, hex" `isErrorFor` "count"
            "<frames> should be a number, got 'x'" `isErrorFor` "frames:dec:x"
            "<from> should be a number" `isErrorFor` "memory:a:b"
        , testCase "Unknown keys suggest close keywords" $ do
            "unknown view {t1}, did you mean {T1[:dec|hex]}?" `isErrorFor` "t1"
            normalizeKey keywords "foo" @?= Left "unknown view {foo}"
        ]

keywords :: [Keyword]
keywords =
    [ valueKey "pc"
    , flagKey "pc:label"
    , flagKey "memory:table"
    , keyword "memory" [Arg "from", Arg "to"]
    , keyword "count" [Fmt ["dec", "hex"] Nothing]
    , keyword "frames" [valueFormat, OptArg "frames"]
    , valueKey "T1"
    ]

isErrorFor :: Text -> Text -> Assertion
isErrorFor expected key = case normalizeKey keywords key of
    Left err | expected `T.isInfixOf` err -> pass
    other -> assertFailure $ "expected an error with " <> show expected <> ", got " <> show other
