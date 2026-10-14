module Wrench.Isa.VliwIv.Test (tests) where

import Control.Exception (ErrorCall (..), evaluate, try)
import Data.Default
import Data.List (isInfixOf)
import Relude
import Relude.Extra
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
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
            let VliwIvSt{regs} =
                    simulate
                        "addi a1, a0, 3 / nop / nop / nop"
                        (withRegs [(A0, 5)])
             in (regs !? A1) @?= Just 8
        , testCase "Bundle with two ALU operations: Add and Sub" $ do
            let VliwIvSt{regs} =
                    simulate
                        "add a2, a0, a1 / sub a3, a0, a1 / nop / nop"
                        (withRegs [(A0, 10), (A1, 3)])
             in do
                    (regs !? A2) @?= Just 13
                    (regs !? A3) @?= Just 7
        , testCase "Shift operations: Sll" $ do
            let VliwIvSt{regs} =
                    simulate
                        "sll a2, a0, a1 / nop / nop / nop"
                        (withRegs [(A0, 3), (A1, 2)])
             in (regs !? A2) @?= Just 12 -- 3 << 2 = 12
        , -- The docs promise "the number of bits specified in the lower 5 bits of the
          -- second source register", and without the mask a count outside 0..31 took
          -- `shiftL` out of range and aborted the simulator with `arithmetic overflow`.
          testCase "Sll masks the shift amount to 5 bits" $ do
            let VliwIvSt{regs} =
                    simulate
                        "sll a2, a0, a1 / nop / nop / nop"
                        (withRegs [(A0, 1), (A1, 33)])
             in (regs !? A2) @?= Just 2
        , testCase "Sll with a negative shift amount masks to 31" $ do
            let VliwIvSt{regs} =
                    simulate
                        "sll a2, a0, a1 / nop / nop / nop"
                        (withRegs [(A0, 1), (A1, -1)])
             in (regs !? A2) @?= Just (-2147483648)
        , testCase "Srl masks the shift amount too" $ do
            let VliwIvSt{regs} =
                    simulate
                        "srl a2, a0, a1 / nop / nop / nop"
                        (withRegs [(A0, 16), (A1, 33)])
             in (regs !? A2) @?= Just 8
        , testCase "Sra masks the shift amount too" $ do
            let VliwIvSt{regs} =
                    simulate
                        "sra a2, a0, a1 / nop / nop / nop"
                        (withRegs [(A0, -16), (A1, -1)])
             in (regs !? A2) @?= Just (-1)
        , testCase "Srl: unsigned right shift" $ do
            let VliwIvSt{regs} =
                    simulate
                        "srl a2, a0, a1 / nop / nop / nop"
                        (withRegs [(A0, -16), (A1, 2)])
             in (regs !? A2) @?= Just 1073741820
        , testCase "Sra: arithmetic right shift" $ do
            let VliwIvSt{regs} =
                    simulate
                        "sra a2, a0, a1 / nop / nop / nop"
                        (withRegs [(A0, -16), (A1, 2)])
             in (regs !? A2) @?= Just (-4)
        , testCase "Memory operations: Lw" $ do
            let st1 = withRegs [(A0, 0)]
                mem1 = either error id $ do
                    m1 <- writeByte (mem st1) 20 0x12
                    m2 <- writeByte m1 21 0x34
                    m3 <- writeByte m2 22 0x56
                    writeByte m3 23 0x78
                VliwIvSt{regs} = simulate "nop / nop / lw a1, 20(a0) / nop" st1{mem = mem1}
             in (regs !? A1) @?= Just 0x78563412
        , testCase "Memory operations: Sw" $ do
            let VliwIvSt{mem} =
                    simulate
                        "nop / nop / sw a1, 20(a0) / nop"
                        (withRegs [(A0, 0), (A1, -559038737)]) -- 0xDEADBEEF as Int32
            case readWord mem 20 of
                Right (_, w) -> w @?= -559038737
                Left err -> assertFailure $ "Memory read failed: " <> toString err
        , testCase "Memory operations: Sb" $ do
            let VliwIvSt{mem} =
                    simulate
                        "nop / nop / sb a1, 20(a0) / nop"
                        (withRegs [(A0, 0), (A1, 0x12345678)])
            case readByte mem 20 of
                Right (_, b) -> b @?= 0x78
                Left err -> assertFailure $ "Memory read failed: " <> toString err
        , testCase "Memory operations: Lb (positive byte)" $ do
            let st1 = withRegs [(A0, 0)]
                mem1 = either error id $ do
                    writeByte (mem st1) 20 0x41
                VliwIvSt{regs} = simulate "nop / nop / lb a1, 20(a0) / nop" st1{mem = mem1}
             in (regs !? A1) @?= Just 0x41
        , testCase "Memory operations: Lb (sign extension for negative byte)" $ do
            let st1 = withRegs [(A0, 0)]
                mem1 = either error id $ do
                    writeByte (mem st1) 20 0x80
                VliwIvSt{regs} = simulate "nop / nop / lb a1, 20(a0) / nop" st1{mem = mem1}
             in (regs !? A1) @?= Just (-128)
        , testCase "Memory operations: Lb with offset (sign extension from 0xFF)" $ do
            let st1 = withRegs [(A0, 0)]
                mem1 = either error id $ do
                    writeByte (mem st1) 25 0xFF
                VliwIvSt{regs} = simulate "nop / nop / lb a1, 25(a0) / nop" st1{mem = mem1}
             in (regs !? A1) @?= Just (-1)
        , testCase "Control flow: J (jump)" $ do
            let VliwIvSt{pc} =
                    simulate
                        "nop / nop / nop / j 100"
                        st0
             in pc @?= 100
        , testCase "Control flow: Jal (jump and link)" $ do
            let VliwIvSt{pc, regs} =
                    simulate
                        "nop / nop / nop / jal ra, 100"
                        st0{pc = 50}
             in do
                    pc @?= 150
                    (regs !? Ra) @?= Just 64 -- 50 + 14 (bundle size)
        , testCase "Control flow: Jr (jump register)" $ do
            let VliwIvSt{pc} =
                    simulate
                        "nop / nop / nop / jr a0"
                        (withRegs [(A0, 200)])
             in pc @?= 200
        , testCase "Control flow: Beqz (branch if equal to zero) - taken" $ do
            let VliwIvSt{pc} =
                    simulate
                        "nop / nop / nop / beqz a0, 50"
                        (withRegs [(A0, 0)])
             in pc @?= 50
        , testCase "Control flow: Beqz - not taken" $ do
            let VliwIvSt{pc} =
                    simulate
                        "nop / nop / nop / beqz a0, 50"
                        (withRegs [(A0, 5)])
             in pc @?= 14 -- Advanced by bundle size
        , testCase "Control flow: Bnez (branch if not equal to zero) - taken" $ do
            let VliwIvSt{pc} =
                    simulate
                        "nop / nop / nop / bnez a0, 50"
                        (withRegs [(A0, 5)])
             in pc @?= 50
        , testCase "Control flow: Bgt (branch if greater than) - taken" $ do
            let VliwIvSt{pc} =
                    simulate
                        "nop / nop / nop / bgt a0, a1, 50"
                        (withRegs [(A0, 10), (A1, 5)])
             in pc @?= 50
        , testCase "Control flow: Bgt - not taken" $ do
            let VliwIvSt{pc} =
                    simulate
                        "nop / nop / nop / bgt a0, a1, 50"
                        (withRegs [(A0, 5), (A1, 10)])
             in pc @?= 14
        , testCase "Control flow: Ble (branch if less than or equal) - taken" $ do
            let VliwIvSt{pc} =
                    simulate
                        "nop / nop / nop / ble a0, a1, 50"
                        (withRegs [(A0, 5), (A1, 10)])
             in pc @?= 50
        , testCase "Control flow: Halt" $ do
            let VliwIvSt{stopped} =
                    simulate
                        "nop / nop / nop / halt"
                        st0
             in stopped @?= True
        , testCase "Logical operations: And and Or" $ do
            let VliwIvSt{regs} =
                    simulate
                        "and a2, a0, a1 / or a3, a0, a1 / nop / nop"
                        (withRegs [(A0, 0xFF00), (A1, 0x0FF0)])
             in do
                    (regs !? A2) @?= Just 0x0F00
                    (regs !? A3) @?= Just 0xFFF0
        , testCase "Logical operations: Xor" $ do
            let VliwIvSt{regs} =
                    simulate
                        "xor a2, a0, a1 / nop / nop / nop"
                        (withRegs [(A0, 0xFF00), (A1, 0x0FF0)])
             in (regs !? A2) @?= Just 0xF0F0
        , testCase "Arithmetic operations: Mul and Div" $ do
            let VliwIvSt{regs} =
                    simulate
                        "mul a2, a0, a1 / div a3, a0, a1 / nop / nop"
                        (withRegs [(A0, 6), (A1, 3)])
             in do
                    (regs !? A2) @?= Just 18
                    (regs !? A3) @?= Just 2
        , testCase "Arithmetic operations: Rem (remainder)" $ do
            let VliwIvSt{regs} =
                    simulate
                        "rem a2, a0, a1 / nop / nop / nop"
                        (withRegs [(A0, 17), (A1, 5)])
             in (regs !? A2) @?= Just 2
        , testCase "Div rounds toward zero, rem keeps the sign of the dividend: -7 / 2" $ do
            let VliwIvSt{regs} =
                    simulate
                        "div a2, a0, a1 / rem a3, a0, a1 / nop / nop"
                        (withRegs [(A0, -7), (A1, 2)])
             in do
                    (regs !? A2) @?= Just (-3)
                    (regs !? A3) @?= Just (-1)
        , testCase "Mulh: high-order multiplication result" $ do
            let VliwIvSt{regs} =
                    simulate
                        "mulh a2, a0, a1 / nop / nop / nop"
                        (withRegs [(A0, 0x10000000), (A1, 0x10)])
             in (regs !? A2) @?= Just 1
        , testCase "Slti: set less than immediate" $ do
            let VliwIvSt{regs} =
                    simulate
                        "slti a1, a0, 10 / slti a2, a0, 3 / nop / nop"
                        (withRegs [(A0, 5)])
             in do
                    (regs !? A1) @?= Just 1 -- 5 < 10
                    (regs !? A2) @?= Just 0 -- 5 >= 3
        , testCase "Lui: load upper immediate" $ do
            let VliwIvSt{regs} =
                    simulate
                        "lui a0, 0xABCDE / nop / nop / nop"
                        st0
             in (regs !? A0) @?= Just (-1412571136) -- 0xABCDE000 as Int32
        , testCase "Mv: move register" $ do
            let VliwIvSt{regs} =
                    simulate
                        "mv a1, a0 / nop / nop / nop"
                        (withRegs [(A0, 42)])
             in (regs !? A1) @?= Just 42
        , testCase "Translator: parsing various instructions" $ do
            translate "addi a1, a0, 3 / nop / nop / nop" @?= True
            translate "nop / nop / lw a1, 20(a0) / j 100" @?= True
            translate "add a2, a0, a1 / sub a3, a0, a1 / sw a1, 0(a0) / halt" @?= True
        , testCase "Translator: blank slots (single space) are equivalent to nop" $ do
            translate "addi a1, a0, 3 /  /  /  " @?= True
            translate " /  / lw a1, 20(a0) / j 100" @?= True
            translate "add a2, a0, a1 / sub a3, a0, a1 / sw a1, 0(a0) / halt" @?= True
        , testCase "Blank slot behaves exactly like an explicit nop" $ do
            let VliwIvSt{regs = regsBlank} =
                    simulate
                        "addi a1, a0, 3 /  /  /  "
                        (withRegs [(A0, 5)])
                VliwIvSt{regs = regsNop} =
                    simulate
                        "addi a1, a0, 3 / nop / nop / nop"
                        (withRegs [(A0, 5)])
             in regsBlank @?= regsNop
        , testGroup
            "Operand fields"
            -- A slot is 24 or 32 bits with the opcode and registers already spent, so
            -- every immediate and displacement is narrower than the machine word. One
            -- that does not fit used to be reduced to the low bits of its field and
            -- sign-extended, with nothing reported.
            [ testCase "widest ALU immediate that fits" $ do
                accepted "addi a0, zero, 65535 / nop / nop / nop"
            , testCase "ALU immediate above the 17-bit field" $ do
                rejected "addi a0, zero, 65536 / nop / nop / nop" "-65536..65535"
            , testCase "ALU immediate that used to truncate to -1" $ do
                rejected "addi a0, zero, 0x1FFFF / nop / nop / nop" "-65536..65535"
            , testCase "slti carries the same field" $ do
                rejected "slti a0, zero, -65537 / nop / nop / nop" "-65536..65535"
            , testCase "widest lui immediate that fits" $ do
                accepted "lui a0, 0xfffff / nop / nop / nop"
            , testCase "lui immediate above the usable field" $ do
                rejected "lui a0, 0x100000 / nop / nop / nop" "0..1048575"
            , testCase "lui is unsigned, so a negative immediate is rejected" $ do
                rejected "lui a0, -1 / nop / nop / nop" "0..1048575"
            , testCase "widest memory offset that fits" $ do
                accepted "nop / nop / lw a1, 1023(a0) / nop"
            , testCase "memory offset that used to truncate to 0" $ do
                rejected "nop / nop / lw a1, 0x800(a0) / nop" "-1024..1023"
            , testCase "store offset below the field" $ do
                rejected "nop / nop / sw a1, -1025(a0) / nop" "-1024..1023"
            , testCase "widest branch displacement that fits" $ do
                accepted "nop / nop / nop / beq a0, a1, 511"
            , testCase "branch displacement that used to truncate to 1" $ do
                rejected "nop / nop / nop / beq a0, a1, 0x401" "-512..511"
            , testCase "beqz and jal share a wider displacement" $ do
                accepted "nop / nop / nop / jal a0, 16383"
            , testCase "beqz displacement above its field" $ do
                rejected "nop / nop / nop / beqz a0, 16384" "-16384..16383"
            , testCase "j has the widest displacement of the three" $ do
                accepted "nop / nop / nop / j 524287"
            , testCase "j displacement above its field" $ do
                rejected "nop / nop / nop / j 524288" "-524288..524287"
            , testCase "rejection names the position, the mnemonic, the field and the range" $ do
                outcome <- fieldOutcome "nop / nop / nop / beq a0, a1, 1025"
                case outcome of
                    Right m -> assertFailure $ "should be rejected, got: " <> show m
                    Left err -> do
                        assertBool ("source position in: " <> err) $ "1:" `isInfixOf` err
                        assertBool ("mnemonic in: " <> err) $ "beq" `isInfixOf` err
                        assertBool ("field in: " <> err) $ "CTRL disp" `isInfixOf` err
                        assertBool ("displacement in: " <> err) $ "1025" `isInfixOf` err
                        assertBool ("range in: " <> err) $ "-512..511" `isInfixOf` err
            , testCase "the memory offset is reported under the operand, while parsing" $ do
                outcome <- fieldOutcome "nop / nop / lw a1, 0x800(a0) / nop"
                case outcome of
                    Right m -> assertFailure $ "should be rejected, got: " <> show m
                    Left err -> do
                        assertBool ("field width in: " <> err) $ "11-bit" `isInfixOf` err
                        assertBool ("carrier in: " <> err) $ "14 byte bundle" `isInfixOf` err
                        assertBool ("range in: " <> err) $ "-1024..1023" `isInfixOf` err
            ]
        ]

-- | Parse one bundle and resolve its operands, which is where the field guards
-- live. A guard that fires calls 'error', so it lands in 'Left' here; the
-- memory offset is checked while parsing, so it lands in 'Left' too.
fieldOutcome :: String -> IO (Either String (VliwIvIsa Int32 Int32))
fieldOutcome code =
    case parse mnemonic "-" (code <> "\n") of
        Left err -> return $ Left (errorBundlePretty err)
        Right (m :: VliwIvIsa Int32 (Ref Int32)) -> do
            -- Walking @show@ forces every resolved operand, the same way
            -- 'Wrench.Translator.Types.derefSection' does.
            let m' = derefMnemonic (const (Just 0)) 0 m
            outcome <- try @ErrorCall $ evaluate $ length (show m' :: String)
            return $ case outcome of
                Left (ErrorCallWithLocation msg _location) -> Left msg
                Right _ -> Right m'

accepted :: String -> IO ()
accepted code = do
    outcome <- fieldOutcome code
    case outcome of
        Right _ -> return ()
        Left err -> assertFailure $ code <> " should be accepted, got: " <> err

rejected :: String -> String -> IO ()
rejected code range = do
    outcome <- fieldOutcome code
    case outcome of
        Right m -> assertFailure $ code <> " should be rejected, got: " <> show m
        Left err -> assertBool ("expected range " <> range <> " in: " <> err) $ range `isInfixOf` err

-- Test helper: Initial state for most tests
st0 :: VliwIvSt Int32
st0 =
    VliwIvSt
        { pc = 0
        , mem = mkIoMem def (Mem 1000 def)
        , regs = def
        , stopped = False
        , internalError = Nothing
        , randoms = []
        , vliwLoad = emptyVliwLoad
        }

withRegs :: [(Register, Int32)] -> VliwIvSt Int32
withRegs pairs = st0{regs = fromList pairs}

translate :: String -> Bool
translate code =
    case parse mnemonic "-" (code <> "\n") of
        Left _ -> False
        Right (_ :: VliwIvIsa Int32 (Ref Int32)) -> True

simulate ::
    String
    -> VliwIvSt Int32
    -> VliwIvSt Int32
simulate code st =
    let instr =
            either
                (error . toText . errorBundlePretty)
                (derefMnemonic (error "labels not defined") def)
                (parse mnemonic "-" (code <> "\n"))
     in execState (instructionExecute 0 instr) st
