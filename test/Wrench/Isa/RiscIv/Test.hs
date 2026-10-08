module Wrench.Isa.RiscIv.Test (tests) where

import Data.Bits (complement)
import Data.Default
import Numeric (showHex)
import Relude
import Relude.Extra
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Text.Megaparsec (parse)
import Wrench.Isa.RiscIv
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Translator.Parser.Types (MnemonicParser (..))
import Wrench.Translator.Types (Ref, deref')

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
        ]

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
