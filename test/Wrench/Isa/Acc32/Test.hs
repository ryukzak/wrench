module Wrench.Isa.Acc32.Test (tests) where

import Control.Exception (ErrorCall (..), evaluate, try)
import Data.Default
import Data.List (isInfixOf)
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Wrench.Isa.Acc32
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Translator (translate)

tests :: TestTree
tests =
    testGroup
        "ISA"
        [ testCase "bgtz taken (acc > 0)" $ do
            runBranch (Bgz 100) 5 @?= 100
        , testCase "bgtz not taken (acc == 0)" $ do
            runBranch (Bgz 100) 0 @?= 10
        , testCase "bgtz not taken (acc < 0)" $ do
            runBranch (Bgz 100) (-1) @?= 10
        , testCase "bltz taken (acc < 0)" $ do
            runBranch (Blz 100) (-1) @?= 100
        , testCase "bltz not taken (acc == 0)" $ do
            runBranch (Blz 100) 0 @?= 10
        , testCase "bltz not taken (acc > 0)" $ do
            runBranch (Blz 100) 5 @?= 10
        , testCase "bgez taken (acc == 0)" $ do
            runBranch (Bgez 100) 0 @?= 100
        , testCase "bgez taken (acc > 0)" $ do
            runBranch (Bgez 100) 5 @?= 100
        , testCase "bgez not taken (acc < 0)" $ do
            runBranch (Bgez 100) (-1) @?= 10
        , testGroup
            "Absolute address field"
            [ testCase "an address that fits is accepted" $ do
                accepted ".text\n_start:\n    add cell\n    halt\n\n    .data\ncell:    .word 7\n"
            , testCase "rejection names the position, the mnemonic, the field and the range" $ do
                rejected
                    ".text\n_start:\n    add 0x10000\n    halt\n"
                    "test.s:3:9: add: address 65536 is not in 0..65535"
            , -- An address is unsigned, so the operand is not a signed 16-bit field:
              -- a negative one used to survive `signBitAnd` and read backwards.
              testCase "a negative address is rejected" $ do
                rejected
                    ".text\n_start:\n    sub -1\n    halt\n"
                    "test.s:3:9: sub: address -1 is not in 0..65535"
            , testCase "every arithmetic and bitwise operand is checked" $ do
                for_ ["add", "sub", "mul", "div", "rem", "shiftl", "shiftr", "and", "or", "xor"] $ \mnemonic -> do
                    outcome <- translateProgram $ ".text\n_start:\n    " <> mnemonic <> " 0x10000\n    halt\n"
                    case outcome of
                        Right n -> assertFailure $ mnemonic <> " should be rejected, got " <> show n <> " characters"
                        Left err -> do
                            assertBool (mnemonic <> " in: " <> err) $ (mnemonic <> ": address") `isInfixOf` err
                            assertBool ("range in: " <> err) $ "0..65535" `isInfixOf` err
            ]
        , testGroup
            "Relative offset field"
            [ testCase "an offset that fits is accepted" $ do
                accepted ".text\n_start:\n    load cell\n    halt\n\n    .data\ncell:    .word 7\n"
            , testCase "offset above the field" $ do
                rejected
                    ".text\n_start:\n    load 0x8000\n    halt\n"
                    "test.s:3:10: load: relative offset 32768 is not in -32768..32767"
            , testCase "offset below the field" $ do
                rejected
                    ".text\n_start:\n    store -32769\n    halt\n"
                    "test.s:3:11: store: relative offset -32769 is not in -32768..32767"
            , -- The offset is counted from the instruction, so what fails is the
              -- distance. It takes a memory the CLI would refuse (8 KiB cap) to
              -- push a label that far, hence the wider memory here.
              testCase "a label out of reach is rejected by its distance" $ do
                outcome <-
                    translateProgramWith
                        100000
                        ".text\n_start:\n    load far\n    halt\n\n    .data\n    .org 0x9000\nfar:    .word 7\n"
                case outcome of
                    Right n -> assertFailure $ "should be rejected, got " <> show n <> " characters"
                    Left err -> err @?= "test.s:3:10: load: relative offset 36864 is not in -32768..32767"
            ]
        , testCase "the absolute operands still take a full machine word" $ do
            accepted ".text\n_start:\n    load_addr 0x12345678\n    halt\n"
        ]

-- | Translate a whole program the way the CLI does, forcing the result so that
-- a field guard firing in 'derefMnemonic' lands here instead of staying a thunk.
translateProgram :: String -> IO (Either String Int)
translateProgram = translateProgramWith 1000

translateProgramWith :: Int -> String -> IO (Either String Int)
translateProgramWith memorySize src = do
    outcome <-
        try @ErrorCall
            $ evaluate
            $ length (show (translate @Acc32Isa @Int32 memorySize (repeat 0) "test.s" src) :: String)
    return $ case outcome of
        Left (ErrorCall msg) -> Left msg
        Right n -> Right n

accepted :: String -> IO ()
accepted src = do
    outcome <- translateProgram src
    case outcome of
        Right _ -> return ()
        Left err -> assertFailure $ "should be accepted, got: " <> err

rejected :: String -> String -> IO ()
rejected src expected = do
    outcome <- translateProgram src
    case outcome of
        Right n -> assertFailure $ "should be rejected, got " <> show n <> " characters"
        Left err -> err @?= expected

-- | Build an initial state with a LoadImm at address 0 (5 bytes) followed by
-- the branch instruction at address 5 (5 bytes). After two instructionStep
-- calls, the acc is set and the branch is evaluated.
mkState :: Acc32Isa Int32 Int32 -> Int32 -> Acc32St Int32
mkState branchInstr accVal =
    let loadInstr = LoadImm accVal
        mem =
            mkIoMem
                def
                ( Mem
                    { memoryData =
                        fromList $
                            [(0, Instruction loadInstr)]
                                <> [(i, InstructionPart) | i <- [1 .. 4]]
                                <> [(5, Instruction branchInstr)]
                                <> [(i, InstructionPart) | i <- [6 .. 9]]
                    , memorySize = 10
                    }
                )
     in initState 0 mem []

runBranch :: Acc32Isa Int32 Int32 -> Int32 -> Int
runBranch instr accVal =
    let st = mkState instr accVal
        st' = execState (instructionStep >> instructionStep) st
     in programCounter st'
