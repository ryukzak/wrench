{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Wrench.Machine.Memory.Test (tests) where

import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Wrench.Machine.Memory

tests :: TestTree
tests =
    testGroup
        "Machine.Memory"
        [ testCase "Read words" $ do
            let mem :: Mem String Int32
                mem = Mem{memoryData = fromList $ map (\x -> (fromEnum x, Value $ 10 + x)) [0 .. 9]}
            Right (mem, 0x0D0C0B0A) @=? readWord mem 0
            Left "memory[-1]: out of memory\n" @=? readWord mem (-1)
            Left "memory[10]: out of memory\n" @=? readWord mem 7
        , testCase "Write words" $ do
            let mem0 :: Mem String Int32
                mem0 = Mem{memoryData = fromList $ map (,Value 0) [0 .. 9]}
            Right
                Mem
                    { memoryData =
                        fromList
                            ( map (\x -> (fromEnum x, Value $ 10 + x)) [0 .. 3]
                                ++ map (,Value 0) [4 .. 9]
                            )
                    }
                @=? writeWord mem0 0 0x0D0C0B0A
            -- FIXME: should fail
            Right
                Mem
                    { memoryData =
                        fromList
                            ( map (,Value 0) [0 .. 9]
                                ++ map (\x -> (fromEnum x, Value x)) [10 .. 13]
                            )
                    }
                @=? writeWord mem0 10 0x0D0C0B0A
        ]
