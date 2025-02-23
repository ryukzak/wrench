module Machine.Types.Test (tests) where

import Machine.Types
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "Machine.Types"
        [ testCase "addWithOverflow: 1 + 1 = 2, no overflow" $ do
            (1 :: Int32) `addWithOverflow` 1 @?= (2, False)
        , testCase "addWithOverflow: maxBound + 1 = minBound, overflow" $ do
            (maxBound :: Int32) `addWithOverflow` 1 @?= (minBound, True)
        , testCase "addWithOverflow: maxBound + maxBound = -2, overflow" $ do
            (maxBound :: Int32) `addWithOverflow` maxBound @?= (-2, True)
        , testCase "subWithOverflow: 1 - 1 = 0, no overflow" $ do
            (1 :: Int32) `subWithOverflow` 1 @?= (0, False)
        , testCase "subWithOverflow: minBound - 1 = maxBound, overflow" $ do
            (minBound :: Int32) `subWithOverflow` 1 @?= (maxBound, True)
        , testCase "subWithOverflow: minBound - maxBound = 1, overflow" $ do
            (minBound :: Int32) `subWithOverflow` maxBound @?= (1, True)
        , testCase "mulWithOverflow: 2 * 3 = 6, no overflow" $ do
            (2 :: Int32) `mulWithOverflow` 3 @?= (6, False)
        , testCase "mulWithOverflow: maxBound * 2 = -2, overflow" $ do
            (maxBound :: Int32) `mulWithOverflow` 2 @?= (-2, True)
        , testCase "mulWithOverflow: maxBound * maxBound = 1, overflow" $ do
            (maxBound :: Int32) `mulWithOverflow` maxBound @?= (1, True)
        ]
