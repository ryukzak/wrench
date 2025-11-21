module Wrench.Isa.VliwIv.Test (tests) where

import Data.Default
import Relude
import Relude.Extra
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Wrench.Isa.VliwIv
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Translator.Parser.Types (MnemonicParser (..))
import Wrench.Translator.Types (DerefMnemonic (..), Ref (..))

tests :: TestTree
tests =
    testGroup
        "ISA"
        [ testCase "Bundle with single ALU operation: Addi" $ do
            let State{regs} =
                    simulate
                        "nop | addi a1, a0, 3 | nop | nop"
                        (withRegs [(A0, 5)])
             in (regs !? A1) @?= Just 8
        , testCase "Bundle with two ALU operations: Add and Sub" $ do
            let State{regs} =
                    simulate
                        "nop | add a2, a0, a1 | sub a3, a0, a1 | nop"
                        (withRegs [(A0, 10), (A1, 3)])
             in do
                    (regs !? A2) @?= Just 13
                    (regs !? A3) @?= Just 7
        , testCase "Shift operations: Sll" $ do
            let State{regs} =
                    simulate
                        "nop | sll a2, a0, a1 | nop | nop"
                        (withRegs [(A0, 3), (A1, 2)])
             in (regs !? A2) @?= Just 12 -- 3 << 2 = 12
        , testCase "Srl: unsigned right shift" $ do
            let State{regs} =
                    simulate
                        "nop | srl a2, a0, a1 | nop | nop"
                        (withRegs [(A0, -16), (A1, 2)])
             in (regs !? A2) @?= Just 1073741820
        , testCase "Sra: arithmetic right shift" $ do
            let State{regs} =
                    simulate
                        "nop | sra a2, a0, a1 | nop | nop"
                        (withRegs [(A0, -16), (A1, 2)])
             in (regs !? A2) @?= Just (-4)
        , testCase "Memory operations: Lw" $ do
            let st1 = withRegs [(A0, 0)]
                mem1 = either error id $ do
                    m1 <- writeByte (mem st1) 20 0x12
                    m2 <- writeByte m1 21 0x34
                    m3 <- writeByte m2 22 0x56
                    writeByte m3 23 0x78
                State{regs} = simulate "lw a1, 20(a0) | nop | nop | nop" st1{mem = mem1}
             in (regs !? A1) @?= Just 0x78563412
        , testCase "Memory operations: Sw" $ do
            let State{mem} =
                    simulate
                        "sw a1, 20(a0) | nop | nop | nop"
                        (withRegs [(A0, 0), (A1, -559038737)]) -- 0xDEADBEEF as Int32
            case readWord mem 20 of
                Right (_, w) -> w @?= -559038737
                Left err -> assertFailure $ "Memory read failed: " <> toString err
        , testCase "Memory operations: Sb" $ do
            let State{mem} =
                    simulate
                        "sb a1, 20(a0) | nop | nop | nop"
                        (withRegs [(A0, 0), (A1, 0x12345678)])
            case readByte mem 20 of
                Right (_, b) -> b @?= 0x78
                Left err -> assertFailure $ "Memory read failed: " <> toString err
        , testCase "Control flow: J (jump)" $ do
            let State{pc} =
                    simulate
                        "nop | nop | nop | j 100"
                        st0
             in pc @?= 100
        , testCase "Control flow: Jal (jump and link)" $ do
            let State{pc, regs} =
                    simulate
                        "nop | nop | nop | jal ra, 100"
                        st0{pc = 50}
             in do
                    pc @?= 150
                    (regs !? Ra) @?= Just 61 -- 50 + 11 (bundle size)
        , testCase "Control flow: Jr (jump register)" $ do
            let State{pc} =
                    simulate
                        "nop | nop | nop | jr a0"
                        (withRegs [(A0, 200)])
             in pc @?= 200
        , testCase "Control flow: Beqz (branch if equal to zero) - taken" $ do
            let State{pc} =
                    simulate
                        "nop | nop | nop | beqz a0, 50"
                        (withRegs [(A0, 0)])
             in pc @?= 50
        , testCase "Control flow: Beqz - not taken" $ do
            let State{pc} =
                    simulate
                        "nop | nop | nop | beqz a0, 50"
                        (withRegs [(A0, 5)])
             in pc @?= 11 -- Advanced by bundle size
        , testCase "Control flow: Bnez (branch if not equal to zero) - taken" $ do
            let State{pc} =
                    simulate
                        "nop | nop | nop | bnez a0, 50"
                        (withRegs [(A0, 5)])
             in pc @?= 50
        , testCase "Control flow: Bgt (branch if greater than) - taken" $ do
            let State{pc} =
                    simulate
                        "nop | nop | nop | bgt a0, a1, 50"
                        (withRegs [(A0, 10), (A1, 5)])
             in pc @?= 50
        , testCase "Control flow: Bgt - not taken" $ do
            let State{pc} =
                    simulate
                        "nop | nop | nop | bgt a0, a1, 50"
                        (withRegs [(A0, 5), (A1, 10)])
             in pc @?= 11
        , testCase "Control flow: Ble (branch if less than or equal) - taken" $ do
            let State{pc} =
                    simulate
                        "nop | nop | nop | ble a0, a1, 50"
                        (withRegs [(A0, 5), (A1, 10)])
             in pc @?= 50
        , testCase "Control flow: Halt" $ do
            let State{stopped} =
                    simulate
                        "nop | nop | nop | halt"
                        st0
             in stopped @?= True
        , testCase "Logical operations: And and Or" $ do
            let State{regs} =
                    simulate
                        "nop | and a2, a0, a1 | or a3, a0, a1 | nop"
                        (withRegs [(A0, 0xFF00), (A1, 0x0FF0)])
             in do
                    (regs !? A2) @?= Just 0x0F00
                    (regs !? A3) @?= Just 0xFFF0
        , testCase "Logical operations: Xor" $ do
            let State{regs} =
                    simulate
                        "nop | xor a2, a0, a1 | nop | nop"
                        (withRegs [(A0, 0xFF00), (A1, 0x0FF0)])
             in (regs !? A2) @?= Just 0xF0F0
        , testCase "Arithmetic operations: Mul and Div" $ do
            let State{regs} =
                    simulate
                        "nop | mul a2, a0, a1 | div a3, a0, a1 | nop"
                        (withRegs [(A0, 6), (A1, 3)])
             in do
                    (regs !? A2) @?= Just 18
                    (regs !? A3) @?= Just 2
        , testCase "Arithmetic operations: Rem (remainder)" $ do
            let State{regs} =
                    simulate
                        "nop | rem a2, a0, a1 | nop | nop"
                        (withRegs [(A0, 17), (A1, 5)])
             in (regs !? A2) @?= Just 2
        , testCase "Mulh: high-order multiplication result" $ do
            let State{regs} =
                    simulate
                        "nop | mulh a2, a0, a1 | nop | nop"
                        (withRegs [(A0, 0x10000000), (A1, 0x10)])
             in (regs !? A2) @?= Just 1
        , testCase "Slti: set less than immediate" $ do
            let State{regs} =
                    simulate
                        "nop | slti a1, a0, 10 | slti a2, a0, 3 | nop"
                        (withRegs [(A0, 5)])
             in do
                    (regs !? A1) @?= Just 1 -- 5 < 10
                    (regs !? A2) @?= Just 0 -- 5 >= 3
        , testCase "Lui: load upper immediate" $ do
            let State{regs} =
                    simulate
                        "nop | lui a0, 0xABCDE | nop | nop"
                        st0
             in (regs !? A0) @?= Just (-1412571136) -- 0xABCDE000 as Int32
        , testCase "Mv: move register" $ do
            let State{regs} =
                    simulate
                        "nop | mv a1, a0 | nop | nop"
                        (withRegs [(A0, 42)])
             in (regs !? A1) @?= Just 42
        , testCase "Translator: parsing various instructions" $ do
            translate "nop | addi a1, a0, 3 | nop | nop" @?= True
            translate "lw a1, 20(a0) | nop | nop | j 100" @?= True
            translate "sw a1, 0(a0) | add a2, a0, a1 | sub a3, a0, a1 | halt" @?= True
        ]

-- Test helper: Initial state for most tests
st0 :: VliwIvState Int32
st0 =
    State
        { pc = 0
        , mem = mkIoMem def (Mem 1000 def) Nothing
        , regs = def
        , stopped = False
        , internalError = Nothing
        , randoms = []
        }

withRegs :: [(Register, Int32)] -> VliwIvState Int32
withRegs pairs = st0{regs = fromList pairs}

translate :: String -> Bool
translate code =
    case parse mnemonic "-" (code <> "\n") of
        Left _ -> False
        Right (_ :: Isa Int32 (Ref Int32)) -> True

simulate ::
    String
    -> VliwIvState Int32
    -> VliwIvState Int32
simulate code st =
    let instr =
            either
                (error . toText . errorBundlePretty)
                (derefMnemonic (error "labels not defined") def)
                (parse mnemonic "-" (code <> "\n"))
     in execState (instructionExecute 0 instr) st
