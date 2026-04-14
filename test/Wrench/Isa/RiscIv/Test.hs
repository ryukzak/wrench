module Wrench.Isa.RiscIv.Test (tests) where

import Data.Default
import Relude
import Relude.Extra
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Text.Megaparsec (parse)
import Wrench.Isa.RiscIv
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Translator.Parser.Types (MnemonicParser (..))
import Wrench.Translator.Types (Ref)

tests :: TestTree
tests =
    testGroup
        "ISA"
        [ testCase "Parse register s10" $ do
            assertBool "s10 should parse" $ isRight (translate "add s10, s10, s10")
        , testCase "Parse register s11" $ do
            assertBool "s11 should parse" $ isRight (translate "add s11, s11, s11")
        , testCase "Parse register s1 (not confused with s10/s11)" $ do
            assertBool "s1 should parse" $ isRight (translate "add s1, s1, s1")
        , testCase "Zero register is hardwired to 0" $ do
            runInstruction Addi{rd = Zero, rs1 = Zero, k = 42} [(Zero, 0)] Zero @?= 0
        , testCase "Addi: A0(5) + 3 = 8" $ do
            runInstruction Addi{rd = A1, rs1 = A0, k = 3} [(A0, 5)] A1 @?= 8
        , testCase "Sll masks shift amount to 5 bits" $ do
            runInstruction Sll{rd = A2, rs1 = A0, rs2 = A1} [(A0, 1), (A1, 33)] A2 @?= 2
        , testCase "Srl: A0(16) >> A1(2) = 4" $ do
            runInstruction Srl{rd = A2, rs1 = A0, rs2 = A1} [(A0, 16), (A1, 2)] A2 @?= 4
        , testCase "Sll: A0(3) << A1(2) = 12" $ do
            runInstruction Sll{rd = A2, rs1 = A0, rs2 = A1} [(A0, 3), (A1, 2)] A2 @?= 12
        , testCase "Srl: A0(-16) >> A1(2) = 1073741820" $ do
            runInstruction Srl{rd = A2, rs1 = A0, rs2 = A1} [(A0, -16), (A1, 2)] A2 @?= 1073741820
        , testCase "Sra: A0(-16) >> A1(2) = -4" $ do
            runInstruction Sra{rd = A2, rs1 = A0, rs2 = A1} [(A0, -16), (A1, 2)] A2 @?= -4
        , testCase "Slti: 5 < 10 = 1" $ do
            runInstruction Slti{rd = A1, rs1 = A0, k = 10} [(A0, 5)] A1 @?= 1
        , testCase "Slti: 5 < 3 = 0" $ do
            runInstruction Slti{rd = A1, rs1 = A0, k = 3} [(A0, 5)] A1 @?= 0
        , testCase "Slti: -1 < 0 = 1" $ do
            runInstruction Slti{rd = A1, rs1 = A0, k = 0} [(A0, -1)] A1 @?= 1
        , testCase "Lb: load byte 0x41" $ do
            runInstructionWithMem
                Lb{rd = A1, offsetRs1 = MemRef{mrOffset = 20, mrReg = A0}}
                [(A0, 0)]
                [(20, 0x41)]
                A1
                @?= 0x41
        , testCase "Lb: sign extension of 0x80 = -128" $ do
            runInstructionWithMem
                Lb{rd = A1, offsetRs1 = MemRef{mrOffset = 20, mrReg = A0}}
                [(A0, 0)]
                [(20, 0x80)]
                A1
                @?= -128
        , testCase "Andi: 0x1234 & 0x0FF = 0x0034" $ do
            runInstruction Andi{rd = A1, rs1 = A0, k = 0x0FF} [(A0, 0x1234)] A1 @?= 0x0034
        , testCase "Ori: 0x1230 | 0x00F = 0x123F" $ do
            runInstruction Ori{rd = A1, rs1 = A0, k = 0x00F} [(A0, 0x1230)] A1 @?= 0x123F
        , testCase "Xori: 0x1234 ^ 0x0FF = 0x12CB" $ do
            runInstruction Xori{rd = A1, rs1 = A0, k = 0x0FF} [(A0, 0x1234)] A1 @?= 0x12CB
        ]

initialState :: Int -> HashMap Register Int32 -> Isa Int32 Int32 -> MachineState (IoMem (Isa Int32 Int32) Int32) Int32
initialState pc regs instr =
    State
        { pc = pc
        , mem =
            mkIoMem
                def
                ( Mem
                    { memoryData =
                        fromList
                            [ (pc, Instruction instr)
                            , (pc + 1, InstructionPart)
                            , (pc + 2, InstructionPart)
                            , (pc + 3, InstructionPart)
                            ]
                    , memorySize = 4
                    }
                )
        , regs = regs
        , stopped = False
        , internalError = Nothing
        }

runInstruction :: Isa Int32 Int32 -> [(Register, Int32)] -> Register -> Int32
runInstruction instr initRegs result = do
    let st = initialState 0 (fromList initRegs) instr
        State{regs} = execState instructionStep st
    fromMaybe (error "Register not found") (regs !? result)

runInstructionWithMem :: Isa Int32 Int32 -> [(Register, Int32)] -> [(Int, Word8)] -> Register -> Int32
runInstructionWithMem instr initRegs memWrites result = do
    let st = initialStateWithMem 0 (fromList initRegs) instr memWrites
        State{regs} = execState instructionStep st
    fromMaybe (error "Register not found") (regs !? result)

initialStateWithMem ::
    Int -> HashMap Register Int32 -> Isa Int32 Int32 -> [(Int, Word8)] -> MachineState (IoMem (Isa Int32 Int32) Int32) Int32
initialStateWithMem pc regs instr memWrites =
    let baseMem =
            mkIoMem
                def
                ( Mem
                    { memoryData =
                        fromList
                            $ [(i, Value 0) | i <- [0 .. 255]]
                            <> [ (pc, Instruction instr)
                               , (pc + 1, InstructionPart)
                               , (pc + 2, InstructionPart)
                               , (pc + 3, InstructionPart)
                               ]
                    , memorySize = 256
                    }
                )
        mem' = either error id $ foldlM (\m (i, b) -> writeByte m i b) baseMem memWrites
     in State
            { pc = pc
            , mem = mem'
            , regs = regs
            , stopped = False
            , internalError = Nothing
            }

translate :: String -> Either String (Isa Int32 (Ref Int32))
translate code =
    case parse mnemonic "-" (code <> "\n") of
        Left err -> Left $ show err
        Right m -> Right m
