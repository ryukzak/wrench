module Wrench.Isa.RiscIv.Test (tests) where

import Data.Default
import Relude
import Relude.Extra
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Wrench.Isa.RiscIv
import Wrench.Machine.Memory
import Wrench.Machine.Types

tests :: TestTree
tests =
    testGroup
        "ISA"
        [ testCase "Addi: A0(5) + 3 = 8" $ do
            runInstruction Addi{rd = A1, rs1 = A0, k = 3} [(A0, 5)] A1 @?= 8
        , testCase "Srl: A0(16) >> A1(2) = 4" $ do
            runInstruction Srl{rd = A2, rs1 = A0, rs2 = A1} [(A0, 16), (A1, 2)] A2 @?= 4
        , testCase "Sll: A0(3) << A1(2) = 12" $ do
            runInstruction Sll{rd = A2, rs1 = A0, rs2 = A1} [(A0, 3), (A1, 2)] A2 @?= 12
        , testCase "Srl: A0(-16) >> A1(2) = 1073741820" $ do
            runInstruction Srl{rd = A2, rs1 = A0, rs2 = A1} [(A0, -16), (A1, 2)] A2 @?= 1073741820
        , testCase "Sra: A0(-16) >> A1(2) = -4" $ do
            runInstruction Sra{rd = A2, rs1 = A0, rs2 = A1} [(A0, -16), (A1, 2)] A2 @?= -4
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
                Nothing
        , regs = regs
        , stopped = False
        , internalError = Nothing
        }

runInstruction :: Isa Int32 Int32 -> [(Register, Int32)] -> Register -> Int32
runInstruction instr initRegs result = do
    let st = initialState 0 (fromList initRegs) instr
        State{regs} = execState instructionStep st
    fromMaybe (error "Register not found") (regs !? result)
