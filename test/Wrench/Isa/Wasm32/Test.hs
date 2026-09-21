module Wrench.Isa.Wasm32.Test (tests) where

import Data.Default
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Text.Megaparsec (parse)
import Wrench.Isa.Wasm32
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Translator (TranslatorResult (..))
import Wrench.Translator.Parser.Types (MnemonicParser (..))
import Wrench.Translator.Types (Ref)
import Prelude qualified

tests :: TestTree
tests =
    testGroup
        "ISA"
        [ testCase "Parse keyword function metadata" $ do
            assertBool "keyword .func should parse"
                $ isRight (parseSource ".func params $n result i32 locals $acc")
        , testCase "Parse numeric function metadata" $ do
            assertBool "numeric func should parse"
                $ isRight (parseSource "func 2, 3, 1")
        , testCase "Source metadata lowers away from executable memory" $ do
            let src = Prelude.unlines [".text", "_start:", "    .func", "    halt", "    .endfunc"]
            case translateWasm32 @Int32 64 "-" src of
                Left err -> assertFailureText err
                Right (TranslatorResult dump labels _stats, functions) -> do
                    HashMap.lookup "_start" labels @?= Just 0
                    IntMap.member 0 functions @?= True
                    prettyDump labels (dumpCells dump) @?= "mem[0..0]: \tHalt \t@_start\nmem[1..1]: \tReturn\nmem[2..63]: \t( 00 )"
        , testCase "Translation rejects unknown locals" $ do
            assertTranslateError
                "unknown local"
                [ ".text"
                , "_start:"
                , "    .func"
                , "    local.get $missing"
                , "    .endfunc"
                ]
        , testCase "Translation rejects duplicate locals" $ do
            assertTranslateError
                "duplicate local name"
                [ ".text"
                , "_start:"
                , "    .func params $n locals $n"
                , "    .endfunc"
                ]
        , testCase "Translation rejects unknown control labels" $ do
            assertTranslateError
                "unknown control label"
                [ ".text"
                , "_start:"
                , "    .func"
                , "    br missing"
                , "    .endfunc"
                ]
        , testCase "Translation rejects calls to non-functions" $ do
            assertTranslateError
                "call target does not point to .func"
                [ ".text"
                , "_start:"
                , "    .func"
                , "    call target"
                , "    .endfunc"
                , "target:"
                , "    i32.const 0"
                ]
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
            let State{operandStack, stopped, internalError} = runProgram functionTable functionProgram
            operandStack @?= [42]
            stopped @?= True
            internalError @?= Nothing
        , testCase "If/else executes the selected structured branch" $ do
            operandStack (runProgram ifElseTable ifElseProgram) @?= [2]
        , testCase "Loop branch keeps the loop frame and exits through block branch" $ do
            operandStack (runProgram loopTable loopProgram) @?= [0]
        ]

parseSource :: String -> Either String (Source Int32 (Ref Int32))
parseSource code =
    case parse mnemonic "-" (code <> "\n") of
        Left err -> Left $ show err
        Right m -> Right m

assertFailureText :: Text -> Assertion
assertFailureText = assertFailure . toString

assertTranslateError :: Text -> [String] -> Assertion
assertTranslateError needle lines' =
    case translateWasm32 @Int32 64 "-" (Prelude.unlines lines') of
        Right _ -> assertFailure $ "translation unexpectedly succeeded; expected " <> toString needle
        Left err -> assertBool ("expected " <> toString needle <> " in " <> toString err) $ needle `T.isInfixOf` err

execute :: Isa Int32 Int32 -> [Int32] -> Wasm32State Int32
execute instr = executeWithBytes instr []

executeWithBytes :: Isa Int32 Int32 -> [(Int, Word8)] -> [Int32] -> Wasm32State Int32
executeWithBytes instr bytes stack =
    execState (instructionExecute 0 instr) (writeBytes bytes emptyState{operandStack = stack})

writeBytes :: [(Int, Word8)] -> Wasm32State Int32 -> Wasm32State Int32
writeBytes bytes st@State{mem} =
    st{mem = either error id $ foldlM (\m (addr, value) -> writeByte m addr value) mem bytes}

emptyState :: Wasm32State Int32
emptyState = rawState []

rawState :: [(Int, Isa Int32 Int32)] -> Wasm32State Int32
rawState instrs =
    State
        { pc = 0
        , mem =
            programMemory instrs
        , operandStack = []
        , operandStackMax = 0
        , frames = []
        , framesMax = 0
        , controlStack = []
        , controlStackMax = 0
        , functions = IntMap.empty
        , stopped = False
        , internalError = Nothing
        }

runProgram :: FunctionTable -> [(Int, Isa Int32 Int32)] -> Wasm32State Int32
runProgram functionTable' instrs = go (200 :: Int) (programState functionTable' instrs)
    where
        go 0 _ = error "test program did not halt"
        go limit st =
            case evalState instructionFetch st of
                Right _ -> go (limit - 1) (execState instructionStep st)
                Left err | err == halted -> st
                Left _ -> st

programState :: FunctionTable -> [(Int, Isa Int32 Int32)] -> Wasm32State Int32
programState functionTable' instrs =
    either error id $ initWasm32State 0 (programMemory instrs) functionTable'

programMemory :: [(Int, Isa Int32 Int32)] -> IoMem (Isa Int32 Int32) Int32
programMemory instrs =
    mkIoMem
        def
        Mem
            { memorySize = 512
            , memoryData =
                fromList
                    $ [(addr, Value 0) | addr <- [0 .. 511]]
                    <> concatMap instructionCells instrs
            }

instructionCells :: (Int, Isa Int32 Int32) -> [(Int, Cell (Isa Int32 Int32) Int32)]
instructionCells (addr, instr) =
    (addr, Instruction instr)
        : [(addr + offset, InstructionPart) | offset <- [1 .. byteSize instr - 1]]

functionTable :: FunctionTable
functionTable =
    IntMap.fromList
        [ (0, FunctionMeta{fmParamCount = 0, fmLocalNames = [], fmResultCount = 0})
        , (11, FunctionMeta{fmParamCount = 1, fmLocalNames = ["$x"], fmResultCount = 1})
        ]

functionProgram :: [(Int, Isa Int32 Int32)]
functionProgram =
    [ (0, I32Const 41)
    , (5, Call 11)
    , (10, Halt)
    , (11, LocalGet 0)
    , (13, I32Const 1)
    , (18, I32Add)
    , (19, Return)
    ]

ifElseTable :: FunctionTable
ifElseTable =
    IntMap.fromList
        [(0, FunctionMeta{fmParamCount = 0, fmLocalNames = [], fmResultCount = 0})]

ifElseProgram :: [(Int, Isa Int32 Int32)]
ifElseProgram =
    [ (0, I32Const 0)
    , (5, If 0)
    , (7, I32Const 1)
    , (12, Else)
    , (13, I32Const 2)
    , (18, End)
    , (19, Halt)
    ]

loopTable :: FunctionTable
loopTable =
    IntMap.fromList
        [(0, FunctionMeta{fmParamCount = 0, fmLocalNames = ["$n"], fmResultCount = 0})]

loopProgram :: [(Int, Isa Int32 Int32)]
loopProgram =
    [ (0, I32Const 3)
    , (5, LocalSet 0)
    , (7, Block 0)
    , (9, Loop 1)
    , (11, LocalGet 0)
    , (13, I32Eqz)
    , (14, BrIf 0)
    , (16, LocalGet 0)
    , (18, I32Const 1)
    , (23, I32Sub)
    , (24, LocalSet 0)
    , (26, Br 1)
    , (28, End)
    , (29, End)
    , (30, LocalGet 0)
    , (32, Halt)
    ]
