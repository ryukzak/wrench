{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Wrench.Machine.Memory.Test (tests) where

import Data.Default (def)
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Wrench.Machine.Memory
import Wrench.Machine.Types

newtype Isa = Isa Int deriving (Eq, Show)

instance ByteSize Isa where
    byteSize (Isa n) = n

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
            Left "iomemory[1]: can't read word from input port" @=? readWord iomem 1
            Left "iomemory[2]: can't read word from input port" @=? readWord iomem 2
            Left "iomemory[3]: can't read word from input port" @=? readWord iomem 3
            Right 0x04030201 @=? (snd <$> readWord iomem 4)
            Left "iomemory[5]: can't read word from input port" @=? readWord iomem 5
            Left "iomemory[6]: can't read word from input port" @=? readWord iomem 6
            Left "iomemory[7]: can't read word from input port" @=? readWord iomem 7
            Right 0x0B0A0908 @=? (snd <$> readWord iomem 8)
        , testCase "readByte around IO port" $ do
            Right 3 @=? (snd <$> readByte iomem 3)
            Right 1 @=? (snd <$> readByte iomem 4)
            Right 2 @=? (snd <$> readByte iomem 5)
            Right 3 @=? (snd <$> readByte iomem 6)
            Right 4 @=? (snd <$> readByte iomem 7)
            Right 8 @=? (snd <$> readByte iomem 8)
        , testCase "writeByte around IO port" $ do
            True @=? isRight (writeByte iomem 3 def)
            True @=? isRight (writeByte iomem 4 def)
            Left "iomemory[5]: can't write byte to input port" @=? writeByte iomem 5 def
            Left "iomemory[6]: can't write byte to input port" @=? writeByte iomem 6 def
            Left "iomemory[7]: can't write byte to input port" @=? writeByte iomem 7 def
            True @=? isRight (writeByte iomem 8 def)
        , testCase "writeWord around IO port" $ do
            True @=? isRight (writeWord iomem 0 def)
            Left "iomemory[1]: can't write word to input port" @=? writeWord iomem 1 def
            Left "iomemory[2]: can't write word to input port" @=? writeWord iomem 2 def
            Left "iomemory[3]: can't write word to input port" @=? writeWord iomem 3 def
            True @=? isRight (writeWord iomem 4 def)
            Left "iomemory[5]: can't write word to input port" @=? writeWord iomem 5 def
            Left "iomemory[6]: can't write word to input port" @=? writeWord iomem 6 def
            Left "iomemory[7]: can't write word to input port" @=? writeWord iomem 7 def
            True @=? isRight (writeWord iomem 8 def)
        , testCase "readInstruction around IO port" $ do
            True @=? isRight (readInstruction piomem 0)
            Left "memory[1]: instruction in memory corrupted" @=? readInstruction piomem 1
            True @=? isRight (readInstruction piomem 2)
            Left "memory[3]: instruction in memory corrupted" @=? readInstruction piomem 3
            Left "iomemory[4]: instruction in memory corrupted" @=? readInstruction piomem 4
            Left "memory[5]: instruction in memory corrupted" @=? readInstruction piomem 5
            Left "iomemory[6]: instruction in memory corrupted" @=? readInstruction piomem 6
            Left "memory[7]: instruction in memory corrupted" @=? readInstruction piomem 7
            Left "memory[8]: instruction in memory corrupted" @=? readInstruction piomem 8
            True @=? isRight (readInstruction piomem 9)
            Left "iomemory[10]: instruction in memory corrupted" @=? readInstruction piomem 10
            Left "memory[11]: instruction in memory corrupted" @=? readInstruction piomem 11
            Left "memory[12]: instruction in memory corrupted" @=? readInstruction piomem 12
            Left "memory[13]: instruction in memory corrupted" @=? readInstruction piomem 13
            Left "iomemory[14]: instruction in memory corrupted" @=? readInstruction piomem 14
            Left "iomemory[15]: instruction in memory corrupted" @=? readInstruction piomem 15
            Left "iomemory[16]: instruction in memory corrupted" @=? readInstruction piomem 16
            Left "iomemory[17]: instruction in memory corrupted" @=? readInstruction piomem 17
            True @=? isRight (readInstruction piomem 18)
        ]
    where
        iomem :: IoMem Isa Int32
        iomem =
            mkIoMem
                (fromList [(4, ([0x04030201, 2, 3], [0, 9, 8]))])
                ( Mem
                    { memorySize = 12
                    , memoryData = fromList $ map (\x -> (fromEnum x, Value x)) [0 .. 11]
                    }
                )
                Nothing
        piomem :: IoMem Isa Int32
        piomem =
            mkIoMem
                ( fromList
                    [ (4, ([1, 2, 3], [0, 9, 8]))
                    , (14, ([1, 2, 3], [0, 9, 8]))
                    ]
                )
                ( Mem
                    { memorySize = 19
                    , memoryData =
                        fromList
                            [ (0, Instruction (Isa 2))
                            , (1, InstructionPart)
                            , (2, Instruction (Isa 2))
                            , (3, InstructionPart)
                            , (4, Instruction (Isa 2)) -- io
                            , (5, InstructionPart) -- io
                            , (6, Instruction (Isa 3)) -- io
                            , (7, InstructionPart) -- io
                            , (8, InstructionPart)
                            , (9, Instruction (Isa 1))
                            , (10, Instruction (Isa 5))
                            , (11, InstructionPart)
                            , (12, InstructionPart)
                            , (13, InstructionPart)
                            , (14, InstructionPart) -- io
                            , (15, Instruction (Isa 1)) -- io
                            , (16, Instruction (Isa 1)) -- io
                            , (17, Instruction (Isa 1)) -- io
                            , (18, Instruction (Isa 1))
                            ]
                    }
                )
                Nothing
