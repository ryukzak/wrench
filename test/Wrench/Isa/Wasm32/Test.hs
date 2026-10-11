module Wrench.Isa.Wasm32.Test (tests) where

import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Prelude qualified
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Wrench.Isa.Wasm32
import Wrench.Machine.Types
import Wrench.Translator (TranslatorResult (..), translate)

-- | What the golden corpus in @test\/golden\/wasm32@ cannot check: the
-- arithmetic and memory edge cases no program there reaches, and every way
-- the machine is meant to refuse a program.
tests :: TestTree
tests =
    testGroup
        "ISA"
        [ testCase "i32.div_s truncates toward zero and i32.rem_s follows the dividend" $ do
            runToStack ["i32.const -7", "i32.const 2", "i32.div_s"] >>= (@?= "-3")
            runToStack ["i32.const 7", "i32.const -2", "i32.div_s"] >>= (@?= "-3")
            runToStack ["i32.const -7", "i32.const 2", "i32.rem_s"] >>= (@?= "-1")
            runToStack ["i32.const 7", "i32.const -2", "i32.rem_s"] >>= (@?= "1")
        , testCase "A zero divisor traps, and so does the one unrepresentable quotient" $ do
            assertTrap "integer divide by zero" ["i32.const 1", "i32.const 0", "i32.div_s"]
            assertTrap "integer divide by zero" ["i32.const 1", "i32.const 0", "i32.rem_s"]
            assertTrap "integer overflow" ["i32.const -2147483648", "i32.const -1", "i32.div_s"]
            runToStack ["i32.const -2147483648", "i32.const -1", "i32.rem_s"] >>= (@?= "0")
        , testCase "Byte loads sign- or zero-extend a high-bit byte" $ do
            let store b = ["i32.const scratch", "i32.const " <> b, "i32.store8", "i32.const scratch"]
            runSourceToStack (withScratch $ store "0x80" <> ["i32.load8_u"]) >>= (@?= "128")
            runSourceToStack (withScratch $ store "0x80" <> ["i32.load8_s"]) >>= (@?= "-128")
        , testCase "A load/store offset indexes without an i32.add" $ do
            stack <-
                runSourceToStack $
                    Prelude.unlines
                        [ ".data"
                        , "a0: .word 10"
                        , "a1: .word 20"
                        , "a2: .word 30"
                        , ".text"
                        , "_start:"
                        , "    i32.const a0"
                        , "    i32.const 99"
                        , "    i32.store 0x4"
                        , "    i32.const a0"
                        , "    i32.load  8"
                        , "    i32.const a0"
                        , "    i32.load  4"
                        , "    i32.const a0"
                        , "    i32.load"
                        , "    halt"
                        ]
            stack @?= "10:99:30"
        , testCase "An immediate too wide for its field is rejected where it is written" $ do
            assertTranslationError "load/store offset 256" ["i32.const 0", "i32.load 256"]
            assertTranslationError "local.get index 300" ["local.get 300"]
            assertTranslationError "br depth -1" ["block", "br -1", "end"]
            assertTranslationError "call resultCount 300" ["i32.const 0", "call 0, 300"]
            assertTranslationError "locals.reserve count 300" ["locals.reserve 300"]
        , testCase "Unbalanced scopes stop the program before its first step" $ do
            assertTrap "no matching block/loop/if" ["end"]
            assertTrap "no matching end" ["block"]
            assertTrap "no matching end" ["loop", "block", "end"]
            assertTrap "no matching if" ["else", "end"]
        , testCase "A br depth deeper than its own nesting is rejected on load" $ do
            assertTrap "depth 3 names a scope that does not enclose it, only 1 open here" ["block", "br 3", "end"]
            assertTrap "depth 0 names a scope that does not enclose it, only 0 open here" ["br 0"]
            assertTrap
                "depth 0 names a scope that does not enclose it, only 0 open here"
                ["i32.const 1", "if", "br 0", "end"]
        , testCase "A br cannot reach past the enclosing function" $ do
            assertSourceTrap "depth reaches past the enclosing function" $
                Prelude.unlines
                    [ ".text"
                    , "_start:"
                    , "    block"
                    , "        i32.const f"
                    , "        call     0, 0"
                    , "f:"
                    , "        br       0"
                    , "    end"
                    , "    halt"
                    ]
        , testCase "A scope cannot span the gap between two code sections" $ do
            assertSourceTrap "no matching end" $
                Prelude.unlines
                    [ ".text"
                    , "helper:"
                    , "    block"
                    , ".data"
                    , "pad: .word 0"
                    , ".text"
                    , "_start:"
                    , "    end"
                    , "    halt"
                    ]
            stack <-
                runSourceToStack $
                    Prelude.unlines
                        [ ".text"
                        , "helper:"
                        , "    block"
                        , "        i32.const 1"
                        , "        br       0"
                        , "    end"
                        , "    return"
                        , ".data"
                        , "pad: .word 0"
                        , ".text"
                        , "_start:"
                        , "    block"
                        , "        i32.const 7"
                        , "        br       0"
                        , "    end"
                        , "    halt"
                        ]
            stack @?= "7"
        , testCase "Each stack is stopped at its own wall" $ do
            assertTrap
                "operand stack overflow: push to 0x1f3 would reach below the stack region at 0x1f4"
                pushForever
            assertSourceTrap "control stack overflow" $
                Prelude.unlines
                    [ ".text"
                    , "recurse:"
                    , "    local.get 0"
                    , "    i32.const recurse"
                    , "    call     1, 1"
                    , "    return"
                    , "_start:"
                    , "    i32.const 1"
                    , "    i32.const recurse"
                    , "    call     1, 1"
                    , "    halt"
                    ]
        , testCase "A port inside the stack region raises the operand floor above it" $ do
            assertSourceTrapIo
                (IntMap.singleton 0x1f4 ([], []))
                "operand stack overflow: push to 0x1f7 would reach below the stack region at 0x1f8"
                (toSource pushForever)
            assertSourceTrapIo
                (IntMap.singleton 0x80 ([], []))
                "operand stack overflow: push to 0x1f3 would reach below the stack region at 0x1f4"
                (toSource pushForever)
        , testCase "sp.init rejects a root outside the stack region" $ do
            assertTrap "sp.init: root 0x04 is outside the stack region" ["i32.const 4", "sp.init"]
            assertTrap "sp.init: root 0x7d0 is outside the stack region" ["i32.const 2000", "sp.init"]
        , testCase "sp.init resets the frame to having no locals" $ do
            locals <-
                runSourceToView "locals:dec" $
                    toSource ["locals.reserve 2", "i32.const 0x300", "sp.init"]
            locals @?= ""
        , testCase "locals.reserve zeroes its slots, and _start can use it" $ do
            runToStack ["locals.reserve 3", "local.get 0", "local.get 1", "local.get 2"] >>= (@?= "0:0:0")
            assertTrap "operand stack overflow" ["locals.reserve 200", "locals.reserve 200"]
        , testCase "Recursive call/return computes factorial(5)" $ do
            src <- readExample "factorial.s"
            out <- runSourceIoToView (IntMap.fromList [(0x80, ([5], [])), (0x84, ([], []))]) "io:0x84:dec" src
            out @?= "[] >>> [120]"
        , testCase "Indirect call through a parameter dispatches to the right target" $ do
            stack <-
                runSourceToStack $
                    Prelude.unlines
                        [ ".text"
                        , "apply_twice:"
                        , "    local.get 1"
                        , "    local.get 0"
                        , "    call     1, 1"
                        , "    local.get 0"
                        , "    call     1, 1"
                        , "    return"
                        , "double:"
                        , "    local.get 0"
                        , "    i32.const 2"
                        , "    i32.mul"
                        , "    return"
                        , "_start:"
                        , "    i32.const double"
                        , "    i32.const 3"
                        , "    i32.const apply_twice"
                        , "    call     2, 1"
                        , "    halt"
                        ]
            stack @?= "12"
        ]

-- | Read one program out of the golden corpus, so a test that wants a real
-- program does not carry a second copy of it.
readExample :: FilePath -> IO String
readExample name = decodeUtf8 @String <$> readFileBS ("test/golden/wasm32/" <> name)

runToStack :: [String] -> IO Text
runToStack instrs = runSourceToStack $ toSource instrs

runSourceToStack :: String -> IO Text
runSourceToStack = runSourceToView "stack:dec"

runSourceToView :: Text -> String -> IO Text
runSourceToView = runSourceIoToView mempty

runSourceIoToView :: IntMap ([Int32], [Int32]) -> Text -> String -> IO Text
runSourceIoToView streams view src =
    case runSourceIo streams src of
        Left err -> assertFailure (toString err) >> error "unreachable"
        Right st -> return $ reprState HashMap.empty st view

assertTrap :: Text -> [String] -> Assertion
assertTrap expected = assertSourceTrap expected . toSource

assertSourceTrap :: Text -> String -> Assertion
assertSourceTrap = assertSourceTrapIo mempty

assertSourceTrapIo :: IntMap ([Int32], [Int32]) -> Text -> String -> Assertion
assertSourceTrapIo streams expected src =
    case runSourceIo streams src of
        Left err | expected `T.isInfixOf` err -> return ()
        Left err -> assertFailure $ "expected " <> show expected <> ", got: " <> toString err
        Right _ -> assertFailure $ "expected a trap matching " <> show expected

assertTranslationError :: Text -> [String] -> Assertion
assertTranslationError expected instrs =
    case translate @Wasm32Isa @Int32 1000 (repeat 0) "-" (toSource instrs) of
        Left err | expected `T.isInfixOf` err -> return ()
        Left err -> assertFailure $ "expected " <> show expected <> ", got: " <> toString err
        Right _ -> assertFailure $ "expected a translation error matching " <> show expected

pushForever :: [String]
pushForever = ["block", "loop", "i32.const -1", "br 0", "end", "end"]

toSource :: [String] -> String
toSource instrs = Prelude.unlines $ [".text", "_start:"] <> map ("    " <>) instrs <> ["    halt"]

withScratch :: [String] -> String
withScratch instrs =
    Prelude.unlines $
        [".data", "scratch: .word 0", ".text", "_start:"]
            <> map ("    " <>) instrs
            <> ["    halt"]

runSourceIo :: IntMap ([Int32], [Int32]) -> String -> Either Text (Wasm32St Int32)
runSourceIo streams src = do
    TranslatorResult{dump, labels} <- translate @Wasm32Isa @Int32 1000 (repeat 0) "-" src
    pc <- maybeToRight "_start label should be defined." (HashMap.lookup "_start" labels)
    let ioDump = mkIoMem streams dump
        st0 = initState (fromEnum pc) ioDump (repeat 0)
    runToHalt (2000 :: Int) st0
    where
        runToHalt :: Int -> Wasm32St Int32 -> Either Text (Wasm32St Int32)
        runToHalt 0 _ = Left "test program did not halt"
        runToHalt limit st =
            case evalState instructionFetch st of
                Right _ -> runToHalt (limit - 1) (execState instructionStep st)
                Left err
                    | err == halted -> Right st
                    | otherwise -> Left err
