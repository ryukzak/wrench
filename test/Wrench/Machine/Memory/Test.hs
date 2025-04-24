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
                mem =
                    Mem
                        { memorySize = 10
                        , memoryData = fromList $ map (\x -> (fromEnum x, Value $ 10 + x)) [0 .. 9]
                        }
            Right (mem, 0x0D0C0B0A) @=? readWord mem 0
            Left "memory[-1]: out of memory\n" @=? readWord mem (-1)
            Left "memory[10]: out of memory\n" @=? readWord mem 7
        , testCase "Write words & bytes" $ do
            let mem0 :: Mem String Int32
                mem0 =
                    Mem
                        { memorySize = 10
                        , memoryData = fromList $ map (,Value 0) [0 .. 9]
                        }
            Right
                Mem
                    { memorySize = 10
                    , memoryData =
                        fromList
                            ( map (\x -> (fromEnum x, Value $ 10 + x)) [0 .. 3]
                                ++ map (,Value 0) [4 .. 9]
                            )
                    }
                @=? writeWord mem0 0 0x0D0C0B0A
            Left "memory[7]: out of memory for word access" @=? writeWord mem0 7 0x0D0C0B0A
            Left "memory[-1]: out of memory for word access" @=? writeWord mem0 (-1) 0x0D0C0B0A
            Right
                Mem
                    { memorySize = 10
                    , memoryData =
                        fromList
                            ( map (,Value 0) [0 .. 2]
                                ++ [(3, Value 0x0A)]
                                ++ map (,Value 0) [4 .. 9]
                            )
                    }
                @=? writeByte mem0 3 0x0A
            Left "memory[10]: out of memory" @=? writeByte mem0 10 0x0A
            Left "memory[-1]: out of memory" @=? writeByte mem0 (-1) 0x0A
        ]
