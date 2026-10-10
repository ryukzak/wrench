module Wrench.Isa.RiscIv.Test (tests) where

import Control.Exception (ErrorCall (..), evaluate, try)
import Data.Bits (complement)
import Data.Default
import Data.List (isInfixOf)
import Numeric (showHex)
import Relude
import Relude.Extra
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Text.Megaparsec (parse)
import Wrench.Isa.RiscIv
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Translator.Parser.Types (MnemonicParser (..))
import Wrench.Translator.Types (DerefMnemonic (..), Ref, deref')

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
        , testCase "Slli: 1 << 4 = 16" $ do
            runInstruction Slli{rd = A1, rs1 = A0, k = 4} [(A0, 1)] A1 @?= 16
        , testCase "Srli: 0x100 >> 4 = 0x10" $ do
            runInstruction Srli{rd = A1, rs1 = A0, k = 4} [(A0, 0x100)] A1 @?= 0x10
        , testCase "Srli: -16 >> 4 = 0x0FFFFFFF (no sign extension)" $ do
            runInstruction Srli{rd = A1, rs1 = A0, k = 4} [(A0, -16)] A1 @?= 0x0FFFFFFF
        , testCase "Srai: -16 >> 4 = -1" $ do
            runInstruction Srai{rd = A1, rs1 = A0, k = 4} [(A0, -16)] A1 @?= -1
        , testCase "Div by zero: 42 / 0 = -1" $ do
            runInstruction Div{rd = A1, rs1 = A0, rs2 = A2} [(A0, 42), (A2, 0)] A1 @?= -1
        , testCase "Rem by zero: 42 % 0 = 42" $ do
            runInstruction Rem{rd = A1, rs1 = A0, rs2 = A2} [(A0, 42), (A2, 0)] A1 @?= 42
        , testGroup
            "12-bit immediate fields"
            [ testCase "Addi: sign comes from bit 11, not from the full word" $ do
                runInstruction Addi{rd = A1, rs1 = A0, k = 0xFFF} [(A0, 0)] A1 @?= -1
            , testCase "Addi: 0x800 is the most negative field value" $ do
                runInstruction Addi{rd = A1, rs1 = A0, k = 0x800} [(A0, 0)] A1 @?= -2048
            , testCase "Addi: 0x7FF is the most positive field value" $ do
                runInstruction Addi{rd = A1, rs1 = A0, k = 0x7FF} [(A0, 0)] A1 @?= 2047
            , testCase "Addi: bits above bit 11 are discarded" $ do
                runInstruction Addi{rd = A1, rs1 = A0, k = 0x12345678} [(A0, 0)] A1 @?= 0x678
            , testCase "Addi: a negative immediate survives the field" $ do
                runInstruction Addi{rd = A1, rs1 = A0, k = -1} [(A0, 0)] A1 @?= -1
            , testCase "Andi: 0xFFF is -1, so the value is unchanged" $ do
                runInstruction Andi{rd = A1, rs1 = A0, k = 0xFFF} [(A0, -1)] A1 @?= -1
            , testCase "Ori: 0xFFF is -1, so every bit is set" $ do
                runInstruction Ori{rd = A1, rs1 = A0, k = 0xFFF} [(A0, 0)] A1 @?= -1
            , testCase "Xori: 0xFFF is -1, so the value is inverted" $ do
                runInstruction Xori{rd = A1, rs1 = A0, k = 0xFFF} [(A0, 0x1234)] A1 @?= complement 0x1234
            , testCase "Slti: 0xFFF is -1, so 0 is not less than it" $ do
                runInstruction Slti{rd = A1, rs1 = A0, k = 0xFFF} [(A0, 0)] A1 @?= 0
            ]
        , testGroup
            "%hi / %lo relocation directives"
            [ testCase "%lo(-1) matches the literal -1" $ do
                immediate "addi t0, zero, %lo(-1)" @?= Right (-1)
            , testCase "%lo keeps the low 12 bits sign-extended" $ do
                immediate "addi t0, zero, %lo(0x12345FFF)" @?= Right (-1)
            , testCase "%lo of a positive low half stays positive" $ do
                immediate "addi t0, zero, %lo(0x12345678)" @?= Right 0x678
            , testCase "%hi rounds up when %lo borrows" $ do
                immediate "lui t0, %hi(0x12345FFF)" @?= Right 0x12346
            , testCase "%hi does not round up when %lo does not borrow" $ do
                immediate "lui t0, %hi(0x12345678)" @?= Right 0x12345
            , testCase "%hi(0xFFFFFFFF) is 0, because %lo alone covers -1" $ do
                immediate "lui t0, %hi(0xFFFFFFFF)" @?= Right 0
            , testCase "%hi/%lo pair reconstructs the whole word" $ do
                reconstruct 0x12345FFF @?= Right 0x12345FFF
            , testCase "%hi/%lo pair reconstructs -1" $ do
                reconstruct (-1) @?= Right (-1)
            ]
        , testGroup
            "Memory offset field"
            [ testCase "Offset without parentheses is still allowed" $ do
                assertBool "lw a0, zero should parse" $ isRight (translate "lw a0, zero")
            , testCase "Hex offset inside the field" $ do
                assertBool "lw a0, 0x84(zero) should parse" $ isRight (translate "lw a0, 0x84(zero)")
            , testCase "Highest offset that fits the field" $ do
                assertBool "lw a0, 2047(zero) should parse" $ isRight (translate "lw a0, 2047(zero)")
            , testCase "Lowest offset that fits the field" $ do
                assertBool "lw a0, -2048(sp) should parse" $ isRight (translate "lw a0, -2048(sp)")
            , testCase "Offset one above the field" $ do
                assertBool "lw a0, 2048(zero) should be rejected" $ isLeft (translate "lw a0, 2048(zero)")
            , testCase "Offset one below the field" $ do
                assertBool "lw a0, -2049(sp) should be rejected" $ isLeft (translate "lw a0, -2049(sp)")
            , testCase "Store offset is checked too" $ do
                assertBool "sw a0, 4096(sp) should be rejected" $ isLeft (translate "sw a0, 4096(sp)")
            , testCase "Byte access offset is checked too" $ do
                assertBool "lb a0, 0x1FFF00(zero) should be rejected" $
                    isLeft (translate "lb a0, 0x1FFF00(zero)")
            , testCase "Rejection names the offset and the field" $ do
                case translate "lw a0, 0x1FFF00(zero)" of
                    Right m -> assertFailure $ "should not parse, got: " <> show m
                    Left err -> do
                        assertBool ("offset value in: " <> err) $ "2096896" `isInfixOf` err
                        assertBool ("field width in: " <> err) $ "12-bit" `isInfixOf` err
                        assertBool ("valid range in: " <> err) $ "-2048..2047" `isInfixOf` err
            ]
        , testGroup
            "Immediate and displacement fields"
            [ testCase "slti carries an I-type immediate, not a shift amount" $ do
                accepted "slti t0, t1, 100"
            , testCase "slti above the I-type field" $ do
                rejected "slti t0, t1, 2048" "-2048..2047"
            , testCase "addi below the I-type field" $ do
                rejected "addi t0, t1, -2049" "-2048..2047"
            , testCase "andi above the I-type field" $ do
                rejected "andi t0, t1, 0xfff" "-2048..2047"
            , testCase "ori above the I-type field" $ do
                rejected "ori t0, t1, 5000" "-2048..2047"
            , testCase "xori above the I-type field" $ do
                rejected "xori t0, t1, 99999" "-2048..2047"
            , testCase "highest shift amount that fits" $ do
                accepted "slli t0, t1, 31"
            , testCase "shift amount above the field" $ do
                rejected "slli t0, t1, 32" "0..31"
            , testCase "negative shift amount" $ do
                rejected "srli t0, t1, -1" "0..31"
            , testCase "highest lui immediate that fits" $ do
                accepted "lui t0, 0xfffff"
            , testCase "lui above the U-type field" $ do
                rejected "lui t0, 0x100000" "0..1048575"
            , testCase "lui is unsigned, so a negative immediate is rejected" $ do
                rejected "lui t0, -1" "0..1048575"
            , testCase "highest branch displacement that fits" $ do
                accepted "beq t0, t1, 4095"
            , testCase "branch displacement above the B-type field" $ do
                rejected "beq t0, t1, 4096" "-4096..4095"
            , testCase "branch displacement below the B-type field" $ do
                rejected "bnez t0, -4097" "-4096..4095"
            , testCase "highest jump displacement that fits" $ do
                accepted "j 1048575"
            , testCase "jump displacement above the J-type field" $ do
                rejected "j 1048576" "-1048576..1048575"
            , testCase "jal displacement below the J-type field" $ do
                rejected "jal ra, -1048577" "-1048576..1048575"
            , -- RISC-V keeps bit 0 of a displacement implicit; RISC-IV spends it.
              testCase "an odd displacement is allowed (RISC-IV specific)" $ do
                accepted "j 1048573"
                accepted "beq t0, t1, 4093"
            , testCase "a literal wider than the machine word" $ do
                assertBool "addi t0, zero, 4294967296 should be rejected" $
                    isLeft (translate "addi t0, zero, 4294967296")
            , testCase "a memory offset wider than the machine word" $ do
                assertBool "lw a0, 0x1_0000_0000(t1) should be rejected" $
                    isLeft (translate "lw a0, 0x1_0000_0000(t1)")
            , testCase "word rejection names the width and the range" $ do
                case translate "addi t0, zero, 4294967296" of
                    Right m -> assertFailure $ "should not parse, got: " <> show m
                    Left err -> do
                        assertBool ("word width in: " <> err) $ "32-bit machine word" `isInfixOf` err
                        assertBool ("valid range in: " <> err) $
                            "-2147483648..4294967295" `isInfixOf` err
            , testCase "an unsigned 32-bit pattern is the word its bits spell" $ do
                immediate "addi t0, zero, 0xFFFFFFFF" @?= Right (-1)
            , testCase "rejection names the position, the mnemonic, the field and the range" $ do
                derefField "beq t0, t1, 4096" >>= \case
                    Right m -> assertFailure $ "should be rejected, got: " <> show m
                    Left err -> do
                        assertBool ("source position in: " <> err) $ "1:13:" `isInfixOf` err
                        assertBool ("mnemonic in: " <> err) $ "beq" `isInfixOf` err
                        assertBool ("field in: " <> err) $ "B-type" `isInfixOf` err
                        assertBool ("displacement in: " <> err) $ "4096" `isInfixOf` err
                        assertBool ("valid range in: " <> err) $ "-4096..4095" `isInfixOf` err
            ]
        ]

-- | Parse one instruction and resolve its references, which is where the field
-- guards live. A guard that fires calls 'error', so it lands in 'Left' here.
derefField :: String -> IO (Either String (RiscIvIsa Int32 Int32))
derefField code =
    case translate code of
        Left err -> return $ Left err
        Right m -> do
            -- Walking @show@ forces every dereferenced field, the same way
            -- 'Wrench.Translator.Types.derefSection' does.
            let m' = derefMnemonic (const (Just 0)) 0 m
            outcome <- try @ErrorCall $ evaluate $ length (show m' :: String)
            return $ case outcome of
                Left (ErrorCallWithLocation msg _location) -> Left msg
                Right _ -> Right m'

accepted :: String -> IO ()
accepted code = do
    outcome <- derefField code
    case outcome of
        Right _ -> return ()
        Left err -> assertFailure $ code <> " should be accepted, got: " <> err

rejected :: String -> String -> IO ()
rejected code range = do
    outcome <- derefField code
    case outcome of
        Right m -> assertFailure $ code <> " should be rejected, got: " <> show m
        Left err -> assertBool ("expected range " <> range <> " in: " <> err) $ range `isInfixOf` err

-- | Parse a single instruction and resolve the immediate it carries. Only
--   literal (label-free) immediates are supported.
immediate :: String -> Either String Int32
immediate code = do
    instr <- translate code
    let resolve = deref' (const Nothing)
    case instr of
        Addi{k} -> Right $ resolve k
        Lui{k} -> Right $ resolve k
        _ -> Left ("no immediate in: " <> code)

-- | Run the canonical @lui@ + @addi@ pair for @x@ and return what lands in the
--   register, to check that the two directives compensate for each other.
reconstruct :: Int32 -> Either String Int32
reconstruct x = do
    let hex = "0x" <> showHex (fromIntegral x :: Word32) ""
    hi <- immediate ("lui t0, %hi(" <> hex <> ")")
    lo <- immediate ("addi t0, t0, %lo(" <> hex <> ")")
    return $ runInstruction Addi{rd = A1, rs1 = A0, k = lo} [(A0, runInstruction Lui{rd = A0, k = hi} [(A0, 0)] A0)] A1

initialState :: Int -> HashMap Register Int32 -> RiscIvIsa Int32 Int32 -> RiscIvSt Int32
initialState pc regs instr =
    RiscIvSt
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

runInstruction :: RiscIvIsa Int32 Int32 -> [(Register, Int32)] -> Register -> Int32
runInstruction instr initRegs result = do
    let st = initialState 0 (fromList initRegs) instr
        RiscIvSt{regs} = execState instructionStep st
    fromMaybe (error "Register not found") (regs !? result)

runInstructionWithMem :: RiscIvIsa Int32 Int32 -> [(Register, Int32)] -> [(Int, Word8)] -> Register -> Int32
runInstructionWithMem instr initRegs memWrites result = do
    let st = initialStateWithMem 0 (fromList initRegs) instr memWrites
        RiscIvSt{regs} = execState instructionStep st
    fromMaybe (error "Register not found") (regs !? result)

initialStateWithMem ::
    Int -> HashMap Register Int32 -> RiscIvIsa Int32 Int32 -> [(Int, Word8)] -> RiscIvSt Int32
initialStateWithMem pc regs instr memWrites =
    let baseMem =
            mkIoMem
                def
                ( Mem
                    { memoryData =
                        fromList $
                            [(i, Value 0) | i <- [0 .. 255]]
                                <> [ (pc, Instruction instr)
                                   , (pc + 1, InstructionPart)
                                   , (pc + 2, InstructionPart)
                                   , (pc + 3, InstructionPart)
                                   ]
                    , memorySize = 256
                    }
                )
        mem' = either error id $ foldlM (\m (i, b) -> writeByte m i b) baseMem memWrites
     in RiscIvSt
            { pc = pc
            , mem = mem'
            , regs = regs
            , stopped = False
            , internalError = Nothing
            }

translate :: String -> Either String (RiscIvIsa Int32 (Ref Int32))
translate code =
    case parse mnemonic "-" (code <> "\n") of
        Left err -> Left $ show err
        Right m -> Right m
