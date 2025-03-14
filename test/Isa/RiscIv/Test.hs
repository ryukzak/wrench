module Isa.RiscIv.Test (tests) where

import Data.Default
import Isa.RiscIv
import Machine.Memory
import Machine.Types
import Relude
import Relude.Extra
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "ISA"
        [ testCase "Addi: A0(5) + 3 = 8" $ do
            runInstruction Addi{rd = A1, rs1 = A0, k = 3} [(A0, 5)] A1 @?= 8
        ]

initialState :: Int -> HashMap Register Int32 -> Isa Int32 Int32 -> MachineState (IoMem (Isa Int32 Int32) Int32) Int32
initialState pc regs instr =
    State
        { pc = pc
        , mem = IoMem{mIoCells = fromList [(pc, Instruction instr)], mIoStreams = def}
        , regs = regs
        , stopped = False
        , internalError = Nothing
        }

runInstruction :: Isa Int32 Int32 -> [(Register, Int32)] -> Register -> Int32
runInstruction instr initRegs result = do
    let st = initialState 0 (fromList initRegs) instr
        State{regs} = execState instructionStep st
    fromMaybe (error "Register not found") (regs !? result)
