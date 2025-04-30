{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Wrench.Machine.Memory.Test (tests) where

import Data.Default (def)
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Wrench.Machine.Memory
import Wrench.Machine.Types

newtype Isa = Isa Int deriving (Eq, Show)

instance ByteLength Isa where
    byteLength (Isa n) = n

tests :: TestTree
tests =
    testGroup
        "Machine.Memory"
        [ testCase "Read words" $ do
            let mem :: Mem Isa Int32
                mem =
                    Mem
                        { memorySize = 10
                        , memoryData = fromList $ map (\x -> (fromEnum x, Value $ 10 + x)) [0 .. 9]
                        }
            Right (mem, 0x0D0C0B0A) @=? readWord mem 0
            Left "memory[-1]: out of memory\n" @=? readWord mem (-1)
            Left "memory[10]: out of memory\n" @=? readWord mem 7
        , testCase "Write words & bytes" $ do
            let mem0 :: Mem Isa Int32
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
        , testCase "readWord around IO port" $ do
            Right 0x03020100 @=? (snd <$> readWord iomem 0)
            Left "memory[1]: can't read word from input port" @=? readWord iomem 1
            Left "memory[2]: can't read word from input port" @=? readWord iomem 2
            Left "memory[3]: can't read word from input port" @=? readWord iomem 3
            Right 1 @=? (snd <$> readWord iomem 4)
            Left "memory[5]: can't read word from input port" @=? readWord iomem 5
            Left "memory[6]: can't read word from input port" @=? readWord iomem 6
            Left "memory[7]: can't read word from input port" @=? readWord iomem 7
            Right 0x0B0A0908 @=? (snd <$> readWord iomem 8)
        , testCase "writeByte around IO port" $ do
            True @=? isRight (writeByte iomem 3 def)
            True @=? isRight (writeByte iomem 4 def)
            Left "memory[5]: can't write byte to input port" @=? writeByte iomem 5 def
            Left "memory[6]: can't write byte to input port" @=? writeByte iomem 6 def
            Left "memory[7]: can't write byte to input port" @=? writeByte iomem 7 def
            True @=? isRight (writeByte iomem 8 def)
        , testCase "writeWord around IO port" $ do
            True @=? isRight (writeWord iomem 0 def)
            Left "memory[1]: can't write word to input port" @=? writeWord iomem 1 def
            Left "memory[2]: can't write word to input port" @=? writeWord iomem 2 def
            Left "memory[3]: can't write word to input port" @=? writeWord iomem 3 def
            True @=? isRight (writeWord iomem 4 def)
            Left "memory[5]: can't write word to input port" @=? writeWord iomem 5 def
            Left "memory[6]: can't write word to input port" @=? writeWord iomem 6 def
            Left "memory[7]: can't write word to input port" @=? writeWord iomem 7 def
            True @=? isRight (writeWord iomem 8 def)
        , testCase "readInstruction around IO port" $ do
            True @=? isRight (readInstruction piomem 0)
            Left "memory[1]: instruction in memory corrupted" @=? readInstruction piomem 1
            True @=? isRight (readInstruction piomem 2)
            Left "memory[3]: instruction in memory corrupted" @=? readInstruction piomem 3
            Left "memory[4]: instruction in memory corrupted" @=? readInstruction piomem 4
            Left "memory[5]: instruction in memory corrupted" @=? readInstruction piomem 5
            True @=? isRight (readInstruction piomem 6)
            True @=? isRight (readInstruction piomem 8)
            Left "memory[9]: instruction in memory corrupted" @=? readInstruction piomem 9
            Left "memory[10]: instruction in memory corrupted" @=? readInstruction piomem 10
        ]
    where
        iomem :: IoMem Isa Int32
        iomem =
            IoMem
                { mIoStreams = fromList [(4, ([1, 2, 3], [0, 9, 8]))]
                , mIoCells =
                    Mem
                        { memorySize = 12
                        , memoryData = fromList $ map (\x -> (fromEnum x, Value x)) [0 .. 11]
                        }
                }
        piomem :: IoMem Isa Int32
        piomem =
            IoMem
                { mIoStreams = fromList [(4, ([1, 2, 3], [0, 9, 8]))]
                , mIoCells =
                    Mem
                        { memorySize = 11
                        , memoryData = fromList $ map (\x -> (x, if even x then Instruction (Isa 2) else InstructionPart)) [0 .. 10]
                        }
                }
