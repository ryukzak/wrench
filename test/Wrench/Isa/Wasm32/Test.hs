module Wrench.Isa.Wasm32.Test (tests) where

import Data.Default
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Prelude qualified
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Text.Megaparsec (parse)
import Wrench.Isa.Wasm32
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Translator (TranslatorResult (..))
import Wrench.Translator.Parser.Types (MnemonicParser (..))
import Wrench.Translator.Types (CodeToken (..), Ref, Section (..))

tests :: TestTree
tests =
    testGroup
        "ISA"
        [ testCase "Parse keyword function metadata" $ do
            assertBool "keyword .func should parse" $
                isRight (parseSource ".func params $n result i32 locals $acc")
        , testCase "Parse numeric function metadata" $ do
            assertBool "numeric func should parse" $
                isRight (parseSource "func 2, 3, 1")
        , testCase ".func metadata lowers to an embedded FuncEnter" $ do
            let src = Prelude.unlines [".text", "_start:", "    .func", "    halt", "    .endfunc"]
            case translateWasm32 @Int32 64 (repeat 0) "-" src of
                Left err -> assertFailureText err
                Right (TranslatorResult dump labels _stats, functions) -> do
                    HashMap.lookup "_start" labels @?= Just 0
                    IntMap.member 0 functions @?= True
                    prettyDump labels (dumpCells dump)
                        @?= "mem[0..3]: \tFuncEnter 0 0 0 \t@_start\nmem[4..4]: \tHalt\nmem[5..5]: \tReturn\nmem[6..63]: \t( 00 )"
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
            operandStackOf (execute I32Sub [3, 10]) @?= [7]
        , testCase "Logical right shift treats negative value as unsigned" $ do
            operandStackOf (execute I32ShrU [4, -16]) @?= [0x0FFFFFFF]
        , testCase "Shift amount is masked to 5 bits" $ do
            operandStackOf (execute I32Shl [33, 1]) @?= [2]
        , testCase "Signed and unsigned comparisons differ" $ do
            operandStackOf (execute I32LtS [1, -1]) @?= [1]
            operandStackOf (execute I32LtU [1, -1]) @?= [0]
        , testCase "Select uses non-zero condition" $ do
            operandStackOf (execute Select [9, 10, 20]) @?= [20]
            operandStackOf (execute Select [0, 10, 20]) @?= [10]
        , testCase "Signed division by zero traps" $ do
            internalError (execute I32DivS [0, 42]) @?= Just "integer divide by zero"
        , testCase "Signed division overflow traps" $ do
            internalError (execute I32DivS [-1, minBound]) @?= Just "integer overflow"
        , testCase "Byte loads support signed and unsigned extension" $ do
            operandStackOf (executeWithBytes I32Load8S [(10, 0x80)] [10]) @?= [-128]
            operandStackOf (executeWithBytes I32Load8U [(10, 0x80)] [10]) @?= [128]
        , testCase "Byte store writes the low byte" $ do
            let State{mem} = executeWithBytes I32Store8 [] [0x12345641, 10]
            fmap snd (readByte mem 10) @?= Right 0x41
        , testCase "Function calls bind params and return results" $ do
            let st@State{stopped, internalError} = runProgram functionTable functionProgram
            operandStackAbove 0 st @?= [42]
            stopped @?= True
            internalError @?= Nothing
        , testCase "If/else executes the selected structured branch" $ do
            operandStackAbove 0 (runProgram ifElseTable ifElseProgram) @?= [2]
        , testCase "Loop branch keeps the loop frame and exits through block branch" $ do
            operandStackAbove 1 (runProgram loopTable loopProgram) @?= [0]
        , testCase "resolveWasmLabels assigns nested labels and resolves branches" $ do
            let code =
                    codeOf
                        [ Block "done"
                        , Loop "again"
                        , BrIf "done"
                        , Br "again"
                        , End
                        , End
                        ] ::
                        Section (Isa Text Int Int32 Int32) Int32 Text
            case resolveWasmLabels code of
                Left err -> assertFailureText err
                Right resolved -> mnemonicsOf resolved @?= [Block 0, Loop 1, BrIf 0, Br 1, End, End]
        , testCase "resolveWasmLabels resets label ids at each FuncEnter" $ do
            let code =
                    codeOf
                        [ FuncEnter 0 0 0
                        , Block "x"
                        , End
                        , FuncEnter 0 0 0
                        , Block "x"
                        , End
                        ] ::
                        Section (Isa Text Int Int32 Int32) Int32 Text
            case resolveWasmLabels code of
                Left err -> assertFailureText err
                Right resolved ->
                    mnemonicsOf resolved
                        @?= [FuncEnter 0 0 0, Block 0, End, FuncEnter 0 0 0, Block 0, End]
        , testCase "resolveWasmLabels rejects branches to unknown labels" $ do
            case resolveWasmLabels (codeOf [Br "missing"]) of
                Right _ -> assertFailure "resolution unexpectedly succeeded"
                Left err ->
                    assertBool ("expected unknown control label in " <> toString err) $
                        "unknown control label" `T.isInfixOf` err
        , testCase "resolveWasmLabels rejects unclosed blocks" $ do
            case resolveWasmLabels (codeOf [Block "open"]) of
                Right _ -> assertFailure "resolution unexpectedly succeeded"
                Left err ->
                    assertBool ("expected unclosed control label in " <> toString err) $
                        "unclosed structured control label" `T.isInfixOf` err
        , testCase "resolveWasmLocals assigns per-function indices and resolves references" $ do
            let code =
                    codeOf
                        [ FuncEnter 1 1 1
                        , LocalGet "x"
                        , LocalGet "y"
                        , I32Add
                        , LocalTee "y"
                        , Return
                        ] ::
                        Section (Isa Int Text Int32 Int32) Int32 Text
            case resolveWasmLocals [["x", "y"]] code of
                Left err -> assertFailureText err
                Right resolved ->
                    mnemonicsOf resolved
                        @?= [FuncEnter 1 1 1, LocalGet 0, LocalGet 1, I32Add, LocalTee 1, Return]
        , testCase "resolveWasmLocals resets the table at each FuncEnter" $ do
            let code =
                    codeOf
                        [ FuncEnter 1 0 0
                        , LocalGet "x"
                        , Return
                        , FuncEnter 1 0 0
                        , LocalGet "x"
                        , Return
                        ] ::
                        Section (Isa Int Text Int32 Int32) Int32 Text
            case resolveWasmLocals [["x"], ["x"]] code of
                Left err -> assertFailureText err
                Right resolved ->
                    mnemonicsOf resolved
                        @?= [FuncEnter 1 0 0, LocalGet 0, Return, FuncEnter 1 0 0, LocalGet 0, Return]
        , testCase "resolveWasmLocals rejects references to unknown locals" $ do
            let code =
                    codeOf [FuncEnter 1 0 0, LocalGet "missing"] ::
                        Section (Isa Int Text Int32 Int32) Int32 Text
            case resolveWasmLocals [["x"]] code of
                Right _ -> assertFailure "resolution unexpectedly succeeded"
                Left err ->
                    assertBool ("expected unknown local in " <> toString err) $
                        "unknown local" `T.isInfixOf` err
        , testCase "resolveWasmLabels and resolveWasmLocals compose in either order" $ do
            let code =
                    codeOf
                        [ FuncEnter 1 0 0
                        , Loop "again"
                        , LocalGet "n"
                        , BrIf "again"
                        , End
                        , Return
                        ] ::
                        Section (Isa Text Text Int32 Int32) Int32 Text
                expected = [FuncEnter 1 0 0, Loop 0, LocalGet 0, BrIf 0, End, Return]
            case resolveWasmLabels code >>= resolveWasmLocals [["n"]] of
                Left err -> assertFailureText err
                Right resolved -> mnemonicsOf resolved @?= expected
            case resolveWasmLocals [["n"]] code >>= resolveWasmLabels of
                Left err -> assertFailureText err
                Right resolved -> mnemonicsOf resolved @?= expected
        ]

codeOf :: [Isa cl vl Int32 Int32] -> Section (Isa cl vl Int32 Int32) Int32 Text
codeOf instrs = Code{org = Nothing, codeTokens = map Mnemonic instrs}

mnemonicsOf :: Section (Isa cl vl Int32 Int32) Int32 Text -> [Isa cl vl Int32 Int32]
mnemonicsOf Code{codeTokens} = [instr | Mnemonic instr <- codeTokens]
mnemonicsOf Data{} = []

parseSource :: String -> Either String (WasmToken Int32 (Ref Int32))
parseSource code =
    case parse mnemonic "-" (code <> "\n") of
        Left err -> Left $ show err
        Right m -> Right m

assertFailureText :: Text -> Assertion
assertFailureText = assertFailure . toString

assertTranslateError :: Text -> [String] -> Assertion
assertTranslateError needle lines' =
    case translateWasm32 @Int32 64 (repeat 0) "-" (Prelude.unlines lines') of
        Right _ -> assertFailure $ "translation unexpectedly succeeded; expected " <> toString needle
        Left err -> assertBool ("expected " <> toString needle <> " in " <> toString err) $ needle `T.isInfixOf` err

execute :: Isa Int Int Int32 Int32 -> [Int32] -> Wasm32State Int32
execute instr = executeWithBytes instr []

executeWithBytes :: Isa Int Int Int32 Int32 -> [(Int, Word8)] -> [Int32] -> Wasm32State Int32
executeWithBytes instr bytes stack =
    execState (instructionExecute 0 instr) (writeBytes bytes (seedStack stack emptyState))

writeBytes :: [(Int, Word8)] -> Wasm32State Int32 -> Wasm32State Int32
writeBytes bytes st@State{mem} =
    st{mem = either error id $ foldlM (\m (addr, value) -> writeByte m addr value) mem bytes}

-- | This memory is byte-addressed and an `Int32` occupies 4 bytes, not
-- one address, so every stack slot below is 4 addresses apart.
wordBytes :: Int
wordBytes = 4

-- | Seed the operand stack with @values@ (head = top, matching the old
-- `operandStack` list convention these tests were written against): pushed
-- in reverse, so the head ends up on top, right below `sp`.
seedStack :: [Int32] -> Wasm32State Int32 -> Wasm32State Int32
seedStack values st@State{mem, frameBase} =
    let addred = zip [frameBase, frameBase + wordBytes ..] (reverse values)
        mem' = either error id $ foldlM (\m (addr, v) -> writeWord m addr v) mem addred
     in st{mem = mem', sp = frameBase + length values * wordBytes}

-- | Read the current frame's operand region back as a list, head = top --
-- the inverse of 'seedStack', for asserting on results. Only valid when
-- `frameBase` is bare scratch space with no locals or control record
-- below `sp` (true right after 'seedStack', which is all `execute`/
-- `executeWithBytes` use this for).
operandStackOf :: Wasm32State Int32 -> [Int32]
operandStackOf State{mem, sp, frameBase} =
    [v | addr <- [sp - wordBytes, sp - 2 * wordBytes .. frameBase], Right (_, v) <- [readWord mem addr]]

-- | Same, but for a `runProgram` result, where `frameBase` is a real
-- call's locals base: skips the @numLocals@ declared locals and the
-- call's own control record (6 words -- 'Wasm32.recordWidth', not
-- exported), neither of which are operand values.
operandStackAbove :: Int -> Wasm32State Int32 -> [Int32]
operandStackAbove numLocals st@State{frameBase} =
    operandStackOf st{frameBase = frameBase + (numLocals + 6) * wordBytes}

emptyState :: Wasm32State Int32
emptyState = rawState []

rawState :: [(Int, Isa Int Int Int32 Int32)] -> Wasm32State Int32
rawState instrs =
    State
        { pc = 0
        , sp = stackBase
        , frameBase = stackBase
        , ctrlTop = -1
        , mem = programMemory instrs
        , spMax = stackBase
        , callDepth = 0
        , callDepthMax = 0
        , ctrlDepth = 0
        , ctrlDepthMax = 0
        , functions = IntMap.empty
        , stopped = False
        , internalError = Nothing
        }
    where
        stackBase = 256

runProgram :: FunctionTable -> [(Int, Isa Int Int Int32 Int32)] -> Wasm32State Int32
runProgram functionTable' instrs = go (200 :: Int) (programState functionTable' instrs)
    where
        go 0 _ = error "test program did not halt"
        go limit st =
            case evalState instructionFetch st of
                Right _ -> go (limit - 1) (execState instructionStep st)
                Left err | err == halted -> st
                Left _ -> st

programState :: FunctionTable -> [(Int, Isa Int Int Int32 Int32)] -> Wasm32State Int32
programState functionTable' instrs =
    either error id $ initWasm32State 0 (programMemory instrs) functionTable'

programMemory :: [(Int, Isa Int Int Int32 Int32)] -> IoMem (Isa Int Int Int32 Int32) Int32
programMemory instrs =
    mkIoMem
        def
        Mem
            { memorySize = 512
            , memoryData =
                fromList $
                    [(addr, Value 0) | addr <- [0 .. 511]]
                        <> concatMap instructionCells instrs
            }

instructionCells :: (Int, Isa Int Int Int32 Int32) -> [(Int, Cell (Isa Int Int Int32 Int32) Int32)]
instructionCells (addr, instr) =
    (addr, Instruction instr)
        : [(addr + offset, InstructionPart) | offset <- [1 .. byteSize instr - 1]]

-- Every function -- including the entry, per real translation -- starts
-- with a 'FuncEnter' occupying its first 4 bytes; `call` reads it
-- directly, so a called function's header must be accurate (the entry's
-- own header is placed for realism but never actually read).
functionTable :: FunctionTable
functionTable =
    IntMap.fromList
        [ (0, FunctionMeta{fmParamCount = 0, fmLocalNames = [], fmResultCount = 0})
        , (20, FunctionMeta{fmParamCount = 1, fmLocalNames = ["$x"], fmResultCount = 1})
        ]

functionProgram :: [(Int, Isa Int Int Int32 Int32)]
functionProgram =
    [ (0, FuncEnter 0 0 0)
    , (4, I32Const 41)
    , (9, Call 20)
    , (14, Halt)
    , (20, FuncEnter 1 0 1)
    , (24, LocalGet 0)
    , (26, I32Const 1)
    , (31, I32Add)
    , (32, Return)
    ]

ifElseTable :: FunctionTable
ifElseTable =
    IntMap.fromList
        [(0, FunctionMeta{fmParamCount = 0, fmLocalNames = [], fmResultCount = 0})]

ifElseProgram :: [(Int, Isa Int Int Int32 Int32)]
ifElseProgram =
    [ (0, FuncEnter 0 0 0)
    , (4, I32Const 0)
    , (9, If 0)
    , (11, I32Const 1)
    , (16, Else)
    , (17, I32Const 2)
    , (22, End)
    , (23, Halt)
    ]

loopTable :: FunctionTable
loopTable =
    IntMap.fromList
        [(0, FunctionMeta{fmParamCount = 0, fmLocalNames = ["$n"], fmResultCount = 0})]

loopProgram :: [(Int, Isa Int Int Int32 Int32)]
loopProgram =
    [ (0, FuncEnter 0 1 0)
    , (4, I32Const 3)
    , (9, LocalSet 0)
    , (11, Block 0)
    , (13, Loop 1)
    , (15, LocalGet 0)
    , (17, I32Eqz)
    , (18, BrIf 0)
    , (20, LocalGet 0)
    , (22, I32Const 1)
    , (27, I32Sub)
    , (28, LocalSet 0)
    , (30, Br 1)
    , (32, End)
    , (33, End)
    , (34, LocalGet 0)
    , (36, Halt)
    ]
