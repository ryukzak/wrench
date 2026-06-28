{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Wrench.Machine.Types.Test (tests) where

import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Wrench.Machine.Types
import Wrench.Machine.Word

tests :: TestTree
tests =
    testGroup
        "Machine.Types"
        [ extTests
        , intervalsTests
        ]

extTests :: TestTree
extTests =
    testGroup
        "Ext arithmetic"
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
        , testCase "subExt: 0 - 1 = -1, no overflow, carry (borrow)" $ do
            subExt (0 :: Int32) 1 @?= Ext{value = -1, overflow = False, carry = True}
        , testCase "subExt: 1 - 2 = -1, no overflow, carry (borrow)" $ do
            subExt (1 :: Int32) 2 @?= Ext{value = -1, overflow = False, carry = True}
        , testCase "subExt: 5 - 3 = 2, no overflow, no carry" $ do
            subExt (5 :: Int32) 3 @?= Ext{value = 2, overflow = False, carry = False}
        , testCase "mulExt: 2 * 3 = 6, no overflow, no carry" $ do
            mulExt (2 :: Int32) 3 @?= Ext{value = 6, overflow = False, carry = False}
        , testCase "mulExt: maxBound * 2 = -2, overflow, carry" $ do
            mulExt (maxBound :: Int32) 2 @?= Ext{value = -2, overflow = True, carry = False}
        , testCase "mulExt: maxBound * maxBound = 1, overflow, carry" $ do
            mulExt (maxBound :: Int32) maxBound @?= Ext{value = 1, overflow = True, carry = False}
        ]

intervalsTests :: TestTree
intervalsTests =
    testGroup
        "Intervals"
        [ testGroup
            "recordRange"
            [ testCase "single range"
                $ renderIntervals (recordRange 10 4 emptyIntervals)
                @?= "10..13"
            , testCase "adjacent ranges merge into one"
                $ renderIntervals (recordRange 0 4 (recordRange 4 4 emptyIntervals))
                @?= "0..7"
            , testCase "overlapping ranges merge into one"
                $ renderIntervals (recordRange 0 6 (recordRange 4 4 emptyIntervals))
                @?= "0..7"
            , testCase "non-adjacent ranges stay separate"
                $ renderIntervals (recordRange 0 4 (recordRange 10 4 emptyIntervals))
                @?= "0..3, 10..13"
            , testCase "byte-level adjacency: [0,3] and [4,7] merge"
                $ renderIntervals (recordRange 4 4 (recordRange 0 4 emptyIntervals))
                @?= "0..7"
            , testCase "byte-level gap: [0,3] and [5,8] stay separate"
                $ renderIntervals (recordRange 5 4 (recordRange 0 4 emptyIntervals))
                @?= "0..3, 5..8"
            ]
        , testGroup
            "rendering"
            [ testCase "empty renders as -" $ do
                renderIntervals emptyIntervals @?= "-"
                renderIntervalsHex emptyIntervals @?= "-"
            , testCase "hex format uses 0x prefix"
                $ renderIntervalsHex (recordRange 0 16 emptyIntervals)
                @?= "0x0..0xf"
            , testCase "hex format on larger range"
                $ renderIntervalsHex (recordRange 0x80 8 emptyIntervals)
                @?= "0x80..0x87"
            , testCase "multiple clusters separated by comma"
                $ renderIntervalsHex
                    ( recordRange 0x100 4
                        $ recordRange 0x10 4 emptyIntervals
                    )
                @?= "0x10..0x13, 0x100..0x103"
            ]
        , testGroup
            "set operations"
            [ testCase "union of disjoint ranges"
                $ renderIntervals (intervalsUnion (recordRange 0 5 emptyIntervals) (recordRange 10 5 emptyIntervals))
                @?= "0..4, 10..14"
            , testCase "intersection of overlapping ranges"
                $ renderIntervals (intervalsIntersect (recordRange 0 10 emptyIntervals) (recordRange 5 10 emptyIntervals))
                @?= "5..9"
            , testCase "intersection of disjoint ranges is empty"
                $ renderIntervals (intervalsIntersect (recordRange 0 5 emptyIntervals) (recordRange 10 5 emptyIntervals))
                @?= "-"
            , testCase "difference carves out a hole"
                $ renderIntervals (intervalsDifference (recordRange 0 10 emptyIntervals) (recordRange 3 4 emptyIntervals))
                @?= "0..2, 7..9"
            ]
        , testGroup
            "queries"
            [ testCase "inIntervals: address inside"
                $ inIntervals 5 (recordRange 0 10 emptyIntervals)
                @?= True
            , testCase "inIntervals: address outside"
                $ inIntervals 15 (recordRange 0 10 emptyIntervals)
                @?= False
            , testCase "intervalsSize sums byte counts"
                $ intervalsSize (recordRange 5 4 (recordRange 0 4 emptyIntervals))
                @?= 8
            , testCase "intervalsToList returns inclusive pairs"
                $ intervalsToList (recordRange 5 4 (recordRange 0 4 emptyIntervals))
                @?= [(0, 3), (5, 8)]
            ]
        ]
