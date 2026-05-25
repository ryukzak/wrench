module Wrench.Isa.Wasm32.Test (tests) where

import Data.Default
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Text.Megaparsec (parse)
import Wrench.Isa.Wasm32
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Translator.Parser.Types (MnemonicParser (..))
import Wrench.Translator.Types (Ref)

tests :: TestTree
tests =
    testGroup
        "ISA"
        [ testCase "Parse keyword function metadata" $ do
            assertBool "keyword .func should parse" $
                isRight (translate ".func params $n result i32 locals $acc")
        , testCase "Parse numeric function metadata" $ do
            assertBool "numeric func should parse" $
                isRight (translate "func 2, 3, 1")
        , testCase "Binary operations pop right operand first" $ do
            operandStack (execute I32Sub [3, 10]) @?= [7]
        , testCase "Logical right shift treats negative value as unsigned" $ do
            operandStack (execute I32ShrU [4, -16]) @?= [0x0FFFFFFF]
        , testCase "Shift amount is masked to 5 bits" $ do
            operandStack (execute I32Shl [33, 1]) @?= [2]
        , testCase "Signed and unsigned comparisons differ" $ do
            operandStack (execute I32LtS [1, -1]) @?= [1]
            operandStack (execute I32LtU [1, -1]) @?= [0]
        , testCase "Select uses non-zero condition" $ do
            operandStack (execute Select [9, 10, 20]) @?= [20]
            operandStack (execute Select [0, 10, 20]) @?= [10]
        , testCase "Signed division by zero traps" $ do
            internalError (execute I32DivS [0, 42]) @?= Just "integer divide by zero"
        , testCase "Signed division overflow traps" $ do
            internalError (execute I32DivS [-1, minBound]) @?= Just "integer overflow"
        , testCase "Byte loads support signed and unsigned extension" $ do
            operandStack (executeWithBytes I32Load8S [(10, 0x80)] [10]) @?= [-128]
            operandStack (executeWithBytes I32Load8U [(10, 0x80)] [10]) @?= [128]
        , testCase "Byte store writes the low byte" $ do
            let State{mem} = executeWithBytes I32Store8 [] [0x12345641, 10]
            fmap snd (readByte mem 10) @?= Right 0x41
        , testCase "Function calls bind params and return results" $ do
            let State{operandStack, stopped, internalError} = runProgram functionProgram
            operandStack @?= [42]
            stopped @?= True
            internalError @?= Nothing
        , testCase "If/else executes the selected structured branch" $ do
            operandStack (runProgram ifElseProgram) @?= [2]
        , testCase "Loop branch keeps the loop frame and exits through block branch" $ do
            operandStack (runProgram loopProgram) @?= [0]
        ]

translate :: String -> Either String (Isa Int32 (Ref Int32))
translate code =
    case parse mnemonic "-" (code <> "\n") of
        Left err -> Left $ show err
        Right m -> Right m

execute :: Isa Int32 Int32 -> [Int32] -> Wasm32State Int32
execute instr stack = executeWithBytes instr [] stack

executeWithBytes :: Isa Int32 Int32 -> [(Int, Word8)] -> [Int32] -> Wasm32State Int32
executeWithBytes instr bytes stack =
    execState (instructionExecute 0 instr) (writeBytes bytes emptyState{operandStack = stack})

writeBytes :: [(Int, Word8)] -> Wasm32State Int32 -> Wasm32State Int32
writeBytes bytes st@State{mem} =
    st{mem = either error id $ foldlM (\m (addr, value) -> writeByte m addr value) mem bytes}

emptyState :: Wasm32State Int32
emptyState = programState []

programState :: [(Int, Isa Int32 Int32)] -> Wasm32State Int32
programState instrs =
    State
        { pc = 0
        , mem =
            mkIoMem
                def
                Mem
                    { memorySize = 512
                    , memoryData =
                        fromList $
                            [(addr, Value 0) | addr <- [0 .. 511]]
                                <> concatMap instructionCells instrs
                    }
        , operandStack = []
        , frames = []
        , controlStack = []
        , pendingCall = Nothing
        , stopped = False
        , internalError = Nothing
        }

instructionCells :: (Int, Isa Int32 Int32) -> [(Int, Cell (Isa Int32 Int32) Int32)]
instructionCells (addr, instr) =
    (addr, Instruction instr)
        : [(addr + offset, InstructionPart) | offset <- [1 .. byteSize instr - 1]]

runProgram :: [(Int, Isa Int32 Int32)] -> Wasm32State Int32
runProgram = go (200 :: Int) . programState
    where
        go 0 _ = error "test program did not halt"
        go limit st =
            case evalState instructionFetch st of
                Right _ -> go (limit - 1) (execState instructionStep st)
                Left err | err == halted -> st
                Left _ -> st

func :: [String] -> [String] -> Int -> Isa Int32 Int32
func params locals results =
    Func
        { funcParams = params
        , funcLocals = locals
        , funcResults = results
        }

functionProgram :: [(Int, Isa Int32 Int32)]
functionProgram =
    [ (0, func [] [] 0)
    , (4, I32Const 41)
    , (9, Call 15)
    , (14, Halt)
    , (15, func ["$x"] [] 1)
    , (19, LocalGet "$x")
    , (21, I32Const 1)
    , (26, I32Add)
    , (27, Return)
    ]

ifElseProgram :: [(Int, Isa Int32 Int32)]
ifElseProgram =
    [ (0, func [] [] 0)
    , (4, I32Const 0)
    , (9, If "$choose")
    , (11, I32Const 1)
    , (16, Else)
    , (17, I32Const 2)
    , (22, End)
    , (23, Halt)
    ]

loopProgram :: [(Int, Isa Int32 Int32)]
loopProgram =
    [ (0, func [] ["$n"] 0)
    , (4, I32Const 3)
    , (9, LocalSet "$n")
    , (11, Block "$done")
    , (13, Loop "$loop")
    , (15, LocalGet "$n")
    , (17, I32Eqz)
    , (18, BrIf "$done")
    , (20, LocalGet "$n")
    , (22, I32Const 1)
    , (27, I32Sub)
    , (28, LocalSet "$n")
    , (30, Br "$loop")
    , (32, End)
    , (33, End)
    , (34, LocalGet "$n")
    , (36, Halt)
    ]
