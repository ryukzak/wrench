module Wrench.Report.Test (tests) where

import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Wrench.Report (substituteBrackets)

tests :: TestTree
tests =
    testGroup
        "ISA"
        [ testCase "Extract brackets" $ do
            substituteBrackets (const "_") "" @?= ""
            substituteBrackets (const "_") "foo {bar} h {g} {z}" @?= "foo _ h _ _"
            substituteBrackets (const "{") "foo {bar} h {g} {z}" @?= "foo { h { {"
        ]
