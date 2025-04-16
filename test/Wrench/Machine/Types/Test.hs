{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Wrench.Machine.Types.Test (tests) where

import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Wrench.Machine.Types

tests :: TestTree
tests =
    testGroup
        "Machine.Types"
        [ testCase "addExt: 1 + 1 = 2, no overflow, no carry" $ do
            addExt (1 :: Int32) 1 @?= Ext{value = 2, overflow = False, carry = False}
        , testCase "addExt: maxBound + 1 = minBound, overflow, carry" $ do
            addExt (maxBound :: Int32) 1 @?= Ext{value = minBound, overflow = True, carry = False}
        , testCase "addExt: maxBound + maxBound = -2, overflow, carry" $ do
            addExt (maxBound :: Int32) maxBound @?= Ext{value = -2, overflow = True, carry = False}
        , testCase "addExt: 0xFFFFFFFF + 1, overflow, carry" $ do
            addExt (0xFFFFFFFF :: Int32) 1 @?= Ext{value = 0, overflow = False, carry = True}
        , testCase "subExt: 1 - 1 = 0, no overflow, no carry" $ do
            subExt (1 :: Int32) 1 @?= Ext{value = 0, overflow = False, carry = False}
        , testCase "subExt: minBound - 1 = maxBound, overflow, carry" $ do
            subExt (minBound :: Int32) 1 @?= Ext{value = maxBound, overflow = True, carry = False}
        , testCase "subExt: minBound - maxBound = 1, overflow, carry" $ do
            subExt (minBound :: Int32) maxBound @?= Ext{value = 1, overflow = True, carry = False}
        , testCase "mulExt: 2 * 3 = 6, no overflow, no carry" $ do
            mulExt (2 :: Int32) 3 @?= Ext{value = 6, overflow = False, carry = False}
        , testCase "mulExt: maxBound * 2 = -2, overflow, carry" $ do
            mulExt (maxBound :: Int32) 2 @?= Ext{value = -2, overflow = True, carry = False}
        , testCase "mulExt: maxBound * maxBound = 1, overflow, carry" $ do
            mulExt (maxBound :: Int32) maxBound @?= Ext{value = 1, overflow = True, carry = False}
        ]
