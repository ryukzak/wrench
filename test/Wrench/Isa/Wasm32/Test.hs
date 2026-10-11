module Wrench.Isa.Wasm32.Test (tests) where

import Data.Bits (shiftL)
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Prelude qualified
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Text.Megaparsec (parse)
import Wrench.Isa.Wasm32
import Wrench.Isa.Wasm32.ControlRecord (Scope (..), decodeScope, encodeScope)
import Wrench.Machine.Types
import Wrench.Translator (TranslatorResult (..), translate)
import Wrench.Translator.Parser.Types (MnemonicParser (..))
import Wrench.Translator.Types (Ref)

tests :: TestTree
tests =
    testGroup
        "ISA"
        [ testCase "i32.load8_u is not swallowed by the i32.load prefix" $ do
            parseOk "i32.load8_u" $ \i -> i @?= I32Load8U 0
            parseOk "i32.load8_s" $ \i -> i @?= I32Load8S 0
            parseOk "i32.load" $ \i -> i @?= I32Load 0
            parseOk "i32.store8" $ \i -> i @?= I32Store8 0
            parseOk "i32.store" $ \i -> i @?= I32Store 0
        , testCase "call parses target/paramCount/resultCount as a comma list" $ do
            parseOk "call 2, 1" $ \i -> i @?= Call 2 1
        , testCase "Byte loads sign/zero-extend a high-bit byte differently" $ do
            stack <-
                runSourceToStack $ withScratch ["i32.const scratch", "i32.const 0x80", "i32.store8", "i32.const scratch", "i32.load8_u"]
            stack @?= "128"
            stack' <-
                runSourceToStack $ withScratch ["i32.const scratch", "i32.const 0x80", "i32.store8", "i32.const scratch", "i32.load8_s"]
            stack' @?= "-128"
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
        , testCase "An offset too large for its field is rejected" $ do
            assertTranslationError "load/store offset 256" ["i32.const 0", "i32.load 256"]
        , testCase "Word store/load round-trips through memory" $ do
            stack <-
                runSourceToStack $
                    withScratch
                        [ "i32.const scratch"
                        , "i32.const scratch"
                        , "i32.load"
                        , "i32.const 1"
                        , "i32.add"
                        , "i32.store"
                        , "i32.const scratch"
                        , "i32.load"
                        ]
            stack @?= "1"
        , testCase "Signed division by zero traps" $ do
            assertHalts False ["i32.const 1", "i32.const 0", "i32.div_s"]
        , testCase "i32.div_s truncates toward zero, not toward -infinity" $ do
            stack <- runToStack ["i32.const -7", "i32.const 2", "i32.div_s"]
            stack @?= "-3"
            stack' <- runToStack ["i32.const 7", "i32.const -2", "i32.div_s"]
            stack' @?= "-3"
        , testCase "i32.rem_s takes the dividend's sign" $ do
            stack <- runToStack ["i32.const -7", "i32.const 2", "i32.rem_s"]
            stack @?= "-1"
            stack' <- runToStack ["i32.const 7", "i32.const -2", "i32.rem_s"]
            stack' @?= "1"
        , testCase "i32.div_s overflows at minBound / -1, but i32.rem_s does not" $ do
            assertHalts False ["i32.const -2147483648", "i32.const -1", "i32.div_s"]
            stack <- runToStack ["i32.const -2147483648", "i32.const -1", "i32.rem_s"]
            stack @?= "0"
        , testCase "Scope targets resolve to the matching end when the program loads" $ do
            assertResolved
                ["i32.const 0", "if", "dup", "else", "dup", "end"]
                [(5, 8), (7, 10)]
            assertResolved
                ["i32.const 0", "if", "dup", "end"]
                [(5, 8)]
            assertResolved
                ["block", "loop", "dup", "end", "end"]
                [(0, 4), (1, 3)]
        , testCase "Control records round-trip through their packed form" $ do
            forM_ sampleScopes $ \scope ->
                (encodeScope @Int32 scope >>= decodeScope @Int32) @?= Right scope
        , testCase "A control-record field rejects a value it can't hold" $ do
            isLeft (encodeScope @Int32 BlockScope{csEnd = 1 `shiftL` 22}) @?= True
            isLeft (encodeScope @Int32 (CallScope 0 256 0 0)) @?= True
            isRight (encodeScope @Int32 (CallScope 0 255 0 255)) @?= True
        , testCase "Out-of-range immediates are rejected where they are written" $ do
            assertTranslationError "local.get index 300" ["local.get 300"]
            assertTranslationError "br depth -1" ["block", "br -1", "end"]
            assertTranslationError "call resultCount 300" ["i32.const 0", "call 0, 300"]
        , testCase "A br depth deeper than its own nesting is rejected on load" $ do
            assertTrap "depth 3 names a scope that does not enclose it, only 1 open here" ["block", "br 3", "end"]
            assertTrap "depth 0 names a scope that does not enclose it, only 0 open here" ["br 0"]
            assertTrap
                "depth 0 names a scope that does not enclose it, only 0 open here"
                ["i32.const 1", "if", "br 0", "end"]
        , testCase "A br still cannot reach past the enclosing function" $ do
            let viaCall body =
                    Prelude.unlines $
                        [ ".text"
                        , "_start:"
                        , "    block"
                        ]
                            <> body
                            <> [ "    end"
                               , "    halt"
                               ]
            assertSourceTrap "depth reaches past the enclosing function" $
                viaCall
                    [ "        i32.const f"
                    , "        call     0, 0"
                    , "f:"
                    , "        br       0"
                    ]
            assertSourceTrap "depth reaches past the enclosing function" $
                viaCall
                    [ "        loop"
                    , "            i32.const g"
                    , "            call     0, 0"
                    , "g:"
                    , "            br       0"
                    , "        end"
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
        , testCase "Unbalanced scopes stop the program before its first step" $ do
            assertTrap "no matching block/loop/if" ["end"]
            assertTrap "no matching end" ["block"]
            assertTrap "no matching end" ["loop", "block", "end"]
            assertTrap "no matching if" ["else", "end"]
        , testCase "block/br_if breaks out to the enclosing block" $ do
            stack <-
                runToStack
                    [ "block"
                    , "loop"
                    , "i32.const 1"
                    , "br_if 1"
                    , "i32.const 99"
                    , "br 0"
                    , "end"
                    , "end"
                    , "i32.const 7"
                    ]
            stack @?= "7"
        , testCase "if/else selects the taken branch" $ do
            stack <- runToStack ["i32.const 0", "if", "i32.const 1", "else", "i32.const 2", "end"]
            stack @?= "2"
        , testCase "Pushing forever hits the operand stack's own wall" $ do
            assertTrap
                "operand stack overflow: push to 0x1f3 would reach below the stack region at 0x1f4"
                pushForever
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
        , testCase "Unbounded recursion hits the control stack's own wall" $ do
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
        , testCase "A callee's own operands are all visible in the stack view" $ do
            stack <-
                runSourceToStack $
                    Prelude.unlines
                        [ ".text"
                        , "g:"
                        , "    i32.const 77"
                        , "    local.get 0"
                        , "    i32.add"
                        , "    return"
                        , "_start:"
                        , "    i32.const 5"
                        , "    i32.const g"
                        , "    call     1, 1"
                        , "    halt"
                        ]
            stack @?= "82"
        , testCase "sp.init moves the root both stacks grow from" $ do
            let fact root =
                    Prelude.unlines $
                        [ ".text"
                        , "fact:"
                        , "    local.get 0"
                        , "    i32.const 1"
                        , "    i32.le_s"
                        , "    if"
                        , "        i32.const 1"
                        , "        return"
                        , "    end"
                        , "    local.get 0"
                        , "    local.get 0"
                        , "    i32.const 1"
                        , "    i32.sub"
                        , "    i32.const fact"
                        , "    call     1, 1"
                        , "    i32.mul"
                        , "    return"
                        , "_start:"
                        ]
                            <> root
                            <> [ "    i32.const 6"
                               , "    i32.const fact"
                               , "    call     1, 1"
                               , "    halt"
                               ]
            lots <- runSourceToStack $ fact ["    i32.const 0x280", "    sp.init"]
            lots @?= "720"
            assertSourceTrap "control stack overflow" $ fact ["    i32.const 0x3e0", "    sp.init"]
        , testCase "drop discards a value nothing else could reach" $ do
            stack <- runToStack ["i32.const 1", "i32.const 2", "drop"]
            stack @?= "1"
        , testCase "select picks by condition without branching" $ do
            onTrue <- runToStack ["i32.const 11", "i32.const 22", "i32.const 1", "select"]
            onTrue @?= "11"
            onFalse <- runToStack ["i32.const 11", "i32.const 22", "i32.const 0", "select"]
            onFalse @?= "22"
        , testCase "unreachable traps" $ do
            assertTrap "unreachable" ["unreachable"]
        , testCase "locals.reserve gives each activation its own scratch" $ do
            stack <-
                runSourceToStack $
                    Prelude.unlines
                        [ ".text"
                        , "fact:"
                        , "    locals.reserve 1"
                        , "    local.get 0"
                        , "    i32.const 1"
                        , "    i32.le_s"
                        , "    if"
                        , "        i32.const 1"
                        , "        return"
                        , "    end"
                        , "    local.get 0"
                        , "    local.set 1"
                        , "    local.get 0"
                        , "    i32.const 1"
                        , "    i32.sub"
                        , "    i32.const fact"
                        , "    call     1, 1"
                        , "    local.get 1"
                        , "    i32.mul"
                        , "    return"
                        , "_start:"
                        , "    i32.const 5"
                        , "    i32.const fact"
                        , "    call     1, 1"
                        , "    halt"
                        ]
            stack @?= "120"
        , testCase "locals.reserve zeroes its slots, and _start can use it" $ do
            stack <- runToStack ["locals.reserve 3", "local.get 0", "local.get 1", "local.get 2"]
            stack @?= "0:0:0"
        , testCase "locals.reserve is bounded by the stacks and by its field" $ do
            assertTrap "operand stack overflow" ["locals.reserve 200", "locals.reserve 200"]
            assertTranslationError "locals.reserve count 300" ["locals.reserve 300"]
        , testCase "Recursive call/return computes factorial(5)" $ do
            stack <- runSourceToStack factorialSrc
            stack @?= "120"
        , testCase "Indirect call through a parameter dispatches to the right target" $ do
            stack <- runSourceToStack applyTwiceSrc
            stack @?= "12"
        ]

sampleScopes :: [Scope]
sampleScopes =
    [ BlockScope{csEnd = 0x1234}
    , LoopScope{csStart = 0x1234, csEnd = 0x5678}
    , CallScope
        { csCallerFrameBase = 0x2468
        , csCallerLocalCount = 3
        , csReturnPc = 0x1357
        , csResultCount = 7
        }
    ]

parseOk :: String -> (Wasm32Isa Int32 (Ref Int32) -> Assertion) -> Assertion
parseOk code check =
    case parse mnemonic "-" (code <> "\n") of
        Left err -> assertFailure $ "parse failed: " <> show err
        Right i -> check i

runToStack :: [String] -> IO Text
runToStack instrs = runSourceToStack $ toSource instrs

runSourceToStack :: String -> IO Text
runSourceToStack = runSourceToView "stack:dec"

runSourceToView :: Text -> String -> IO Text
runSourceToView view src =
    case runSource src of
        Left err -> assertFailure (toString err) >> error "unreachable"
        Right st -> return $ reprState HashMap.empty st view

assertHalts :: Bool -> [String] -> Assertion
assertHalts expectSuccess instrs =
    case runSource (toSource instrs) of
        Left _ | not expectSuccess -> return ()
        Left err -> assertFailure $ "expected success, got: " <> toString err
        Right _ | expectSuccess -> return ()
        Right _ -> assertFailure "expected a trap, but the program ran to completion"

assertResolved :: [String] -> [(Int, Int)] -> Assertion
assertResolved instrs expected =
    case translate @Wasm32Isa @Int32 1000 (repeat 0) "-" (toSource instrs) of
        Left err -> assertFailure $ "translation failed: " <> toString err
        Right TranslatorResult{dump} ->
            let code = [(addr, i) | (addr, Instruction i) <- IntMap.toAscList (memoryData dump)]
             in resolveScopeTargets code @?= Right (IntMap.fromList expected)

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

runSource :: String -> Either Text (Wasm32St Int32)
runSource = runSourceIo mempty

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

factorialSrc :: String
factorialSrc =
    Prelude.unlines
        [ ".text"
        , ""
        , "factorial:"
        , "    local.get 0"
        , "    i32.const 1"
        , "    i32.le_s"
        , "    if"
        , "        i32.const 1"
        , "        return"
        , "    end"
        , "    local.get 0"
        , "    local.get 0"
        , "    i32.const 1"
        , "    i32.sub"
        , "    i32.const factorial"
        , "    call     1, 1"
        , "    i32.mul"
        , "    return"
        , ""
        , "_start:"
        , "    i32.const 5"
        , "    i32.const factorial"
        , "    call     1, 1"
        , "    halt"
        ]

applyTwiceSrc :: String
applyTwiceSrc =
    Prelude.unlines
        [ ".text"
        , ""
        , "apply_twice:"
        , "    local.get 1"
        , "    local.get 0"
        , "    call     1, 1"
        , "    local.get 0"
        , "    call     1, 1"
        , "    return"
        , ""
        , "double:"
        , "    local.get 0"
        , "    i32.const 2"
        , "    i32.mul"
        , "    return"
        , ""
        , "_start:"
        , "    i32.const double"
        , "    i32.const 3"
        , "    i32.const apply_twice"
        , "    call     2, 1"
        , "    halt"
        ]
