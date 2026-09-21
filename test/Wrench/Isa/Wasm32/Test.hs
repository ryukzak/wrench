{-# LANGUAGE TemplateHaskell #-}

module Wrench.Isa.Wasm32.Test (tests) where

import Data.FileEmbed (embedStringFile)
import Data.HashMap.Strict qualified as HashMap
import Prelude qualified
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Text.Megaparsec (parse)
import Wrench.Isa.Wasm32
import Wrench.Machine.Types
import Wrench.Translator (TranslatorResult (..), translate)
import Wrench.Translator.Parser.Types (MnemonicParser (..))
import Wrench.Translator.Types (Ref)

tests :: TestTree
tests =
    testGroup
        "ISA"
        [ testCase "i32.load8_u is not swallowed by the i32.load prefix" $ do
            -- Regression: choice tries "i32.load" before "i32.load8_u" in
            -- source order would have to backtrack past already-consumed
            -- input to try the longer alternative, and plain (non-`try`)
            -- choice alternatives don't -- see the parser's own comment.
            parseOk "i32.load8_u" $ \i -> i @?= I32Load8U
            parseOk "i32.load8_s" $ \i -> i @?= I32Load8S
            parseOk "i32.load" $ \i -> i @?= I32Load
            parseOk "i32.store8" $ \i -> i @?= I32Store8
            parseOk "i32.store" $ \i -> i @?= I32Store
        , testCase "call parses target/paramCount/resultCount as a comma list" $ do
            parseOk "call 2, 1" $ \i -> i @?= Call 2 1
        , testCase "Byte loads sign/zero-extend a high-bit byte differently" $ do
            -- A scratch address must be a `.data` cell, not a raw number
            -- landing inside the test's own code: reading an
            -- uninitialized instruction cell as data is rejected outright
            -- (writing to one first happens to succeed, converting it --
            -- but that's an implementation detail not worth depending
            -- on).
            stack <-
                runSourceToStack $ withScratch ["i32.const scratch", "i32.const 0x80", "i32.store8", "i32.const scratch", "i32.load8_u"]
            stack @?= "128"
            stack' <-
                runSourceToStack $ withScratch ["i32.const scratch", "i32.const 0x80", "i32.store8", "i32.const scratch", "i32.load8_s"]
            stack' @?= "-128"
        , testCase "Word store/load round-trips through memory" $ do
            -- i32.store expects [address, value] with value on top (real
            -- WebAssembly's own stack order -- see I32Store's haddock),
            -- so the destination has to be pushed *before* computing the
            -- new value, not after.
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
        , testCase "Recursive call/return computes factorial(5)" $ do
            stack <- runSourceToStack factorialSrc
            stack @?= "120"
        , testCase "Indirect call through a parameter dispatches to the right target" $ do
            stack <- runSourceToStack applyTwiceSrc
            stack @?= "12"
        ]

-- | Parse a single mnemonic line in isolation (no labels involved).
parseOk :: String -> (Isa Int32 (Ref Int32) -> Assertion) -> Assertion
parseOk code check =
    case parse mnemonic "-" (code <> "\n") of
        Left err -> assertFailure $ "parse failed: " <> show err
        Right i -> check i

-- | Assemble a tiny `_start`-only program from bare instruction lines,
-- run it to completion, and read back the `stack:dec` report view --
-- reuses the exact same view logic the CLI\/golden tests exercise,
-- rather than reaching into 'MachineState' fields the module doesn't
-- export.
runToStack :: [String] -> IO Text
runToStack instrs = runSourceToStack $ toSource instrs

runSourceToStack :: String -> IO Text
runSourceToStack src =
    case runSource src of
        Left err -> assertFailure (toString err) >> error "unreachable"
        Right st -> return $ reprState HashMap.empty st "stack:dec"

-- | Assert the program halts cleanly (@expectSuccess@) or hits an
-- internal error\/trap instead.
assertHalts :: Bool -> [String] -> Assertion
assertHalts expectSuccess instrs =
    case runSource (toSource instrs) of
        Left _ | not expectSuccess -> return ()
        Left err -> assertFailure $ "expected success, got: " <> toString err
        Right _ | expectSuccess -> return ()
        Right _ -> assertFailure "expected a trap, but the program ran to completion"

toSource :: [String] -> String
toSource instrs = Prelude.unlines $ [".text", "_start:"] <> map ("    " <>) instrs <> ["    halt"]

-- | Like 'toSource', but with a one-word `.data` cell named @scratch@ for
-- tests that need a real, freshly-zeroed memory address rather than a
-- bare number that might land inside the program's own code.
withScratch :: [String] -> String
withScratch instrs =
    Prelude.unlines $
        [".data", "scratch: .word 0", ".text", "_start:"]
            <> map ("    " <>) instrs
            <> ["    halt"]

-- | Parse, lower, and run a wasm32 program to completion (halt or trap),
-- returning the final machine state. 'Left' only for translation
-- failures or a trap\/internal error reached during execution --
-- distinguished from a clean halt via 'instructionFetch' the same way
-- 'Machine.instructionStep''s own default implementation does.
runSource :: String -> Either Text (Wasm32State Int32)
runSource src = do
    TranslatorResult{dump, labels} <- translate @Isa @Int32 1000 (repeat 0) "-" src
    pc <- maybeToRight "_start label should be defined." (HashMap.lookup "_start" labels)
    let ioDump = mkIoMem mempty dump
        st0 = initState (fromEnum pc) ioDump (repeat 0)
    runToHalt (2000 :: Int) st0
    where
        runToHalt :: Int -> Wasm32State Int32 -> Either Text (Wasm32State Int32)
        runToHalt 0 _ = Left "test program did not halt"
        runToHalt limit st =
            case evalState instructionFetch st of
                Right _ -> runToHalt (limit - 1) (execState instructionStep st)
                Left err
                    | err == halted -> Right st
                    | otherwise -> Left err

factorialSrc :: String
factorialSrc = $(embedStringFile "test/Wrench/Isa/Wasm32/fixtures/factorial.s")

applyTwiceSrc :: String
applyTwiceSrc = $(embedStringFile "test/Wrench/Isa/Wasm32/fixtures/apply_twice.s")
