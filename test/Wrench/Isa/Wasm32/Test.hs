{-# LANGUAGE QuasiQuotes #-}

module Wrench.Isa.Wasm32.Test (tests) where

import Control.Exception (ErrorCall (..), evaluate, try)
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Prelude qualified
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Wrench.Isa.Wasm32
import Wrench.Isa.Wasm32.State (Wasm32St)
import Wrench.Machine.Memory (memAccessCount)
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
            let store b = ["i32.const scratch", [i|i32.const #{b :: String}|], "i32.store8", "i32.const scratch"]
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
            assertTranslationError "i32.load offset 256 is not in 0..255" ["i32.const 0", "i32.load 256"]
            assertTranslationError "local.get index 300 is not in 0..255" ["local.get 300"]
            assertTranslationError "br depth -1 is not in 0..255" ["block", "br -1", "end"]
            assertTranslationError "call resultCount 300 is not in 0..255" ["i32.const 0", "call 0, 300"]
            assertTranslationError "locals.reserve count 300 is not in 0..255" ["locals.reserve 300"]
            assertTranslationError "4:14:" ["i32.const 0", "i32.load 256"]
            assertTranslationError "expected" ["block", "br start", "end"]
        , testCase "A scope with no matching end fails at the scope" $ do
            assertTrap "scope at 0x00 has no matching end" ["block"]
            assertTrap "has no matching end" ["loop", "block", "end"]
        , testCase "A br cannot reach past the enclosing function" $ do
            assertSourceTrap "depth reaches past the enclosing function" $
                Prelude.unlines
                    [ ".text"
                    , "_start:"
                    , "    block"
                    , "        i32.const f"
                    , "        call 0, 0"
                    , "f:"
                    , "        br 0"
                    , "    end"
                    , "    halt"
                    ]
        , testCase "A scope cannot span the gap between two code sections" $ do
            assertSourceTrap "has no matching end" $
                Prelude.unlines
                    [ ".text"
                    , "_start:"
                    , "    block"
                    , ".data"
                    , "pad: .word 0"
                    , ".text"
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
                        , "        br 0"
                        , "    end"
                        , "    return"
                        , ".data"
                        , "pad: .word 0"
                        , ".text"
                        , "_start:"
                        , "    block"
                        , "        i32.const 7"
                        , "        br 0"
                        , "    end"
                        , "    halt"
                        ]
            stack @?= "7"
        , testCase "sp.init resets the frame to having no locals" $ do
            locals <-
                runSourceToView "locals:dec" $
                    toSource ["locals.reserve 2", "i32.const 0x300", "sp.init"]
            locals @?= ""
        , testCase "A branch taken again reads nothing from the control stack" $ do
            let countdown n =
                    Prelude.unlines
                        [ ".data"
                        , [i|n: .word #{n :: Int}|]
                        , ".text"
                        , "_start:"
                        , "    loop"
                        , "        i32.const n"
                        , "        i32.const n i32.load i32.const 1 i32.sub"
                        , "        i32.store"
                        , "        i32.const n i32.load"
                        , "        br_if 0"
                        , "    end"
                        , "    halt"
                        ]
            three <- sourceReads (countdown 3)
            four <- sourceReads (countdown 4)
            four - three @?= 9 + 1 + 2
        , testCase "A write that changes a remembered record is what the next branch reads" $ do
            stack <-
                runSourceToStack $
                    Prelude.unlines
                        [ ".data"
                        , "again: .word 0"
                        , ".text"
                        , "_start:"
                        , "    i32.const 0x300 sp.init"
                        , "    loop"
                        , "        i32.const again i32.load"
                        , "        if"
                        , "            i32.const 0x300 i32.const out i32.store"
                        , "        end"
                        , "        i32.const again i32.const 1 i32.store"
                        , "        br 0"
                        , "    end"
                        , "out:"
                        , "    i32.const 42"
                        , "    halt"
                        ]
            stack @?= "42"
        , testCase "A record at an I/O port is never remembered, so a branch reads the port"
            $ assertSourceTrapIo (IntMap.fromList [(0x80, ([7], []))]) "input is depleted"
            $ toSource ["i32.const 0x80 sp.init", "loop", "br 0", "end"]
        , testCase "A pop of one of the last two values pushed reads no memory" $ do
            sourceReads (toSource ["i32.const 2", "i32.const 3", "i32.add"]) >>= (@?= 4)
            sourceReads (toSource ["i32.const 2", "i32.const 3", "i32.const 4", "drop", "drop", "drop"]) >>= (@?= 8)
        , testCase "A write into a held slot is what the next pop sees" $
            runToStack ["i32.const 5", "i32.const 9", "local.set 0", "i32.const 1", "i32.add"] >>= (@?= "10")
        , testCase "A slot at an I/O port is never held, so a pop there reads the input" $ do
            out <-
                runSourceIoToView (IntMap.fromList [(0x80, ([7], []))]) "io:0x80:dec" $
                    toSource ["i32.const 0x84", "sp.init", "i32.const 5", "i32.const 0", "i32.add"]
            out @?= "[] >>> [5,7]"
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
                        , "    call 1, 1"
                        , "    local.get 0"
                        , "    call 1, 1"
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
                        , "    call 2, 1"
                        , "    halt"
                        ]
            stack @?= "12"
        ]

-- | Read one program out of the golden corpus, so a test that wants a real
-- program does not carry a second copy of it.
readExample :: FilePath -> IO String
readExample name = decodeUtf8 @String <$> readFileBS [i|test/golden/wasm32/#{name}|]

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

-- | Memory reads over the whole run, the instruction fetches included.
sourceReads :: String -> IO Int
sourceReads src =
    case runSourceIo mempty src of
        Left err -> assertFailure (toString err) >> error "unreachable"
        Right st -> return $ acReads $ memAccessCount $ memoryDump st

assertTrap :: Text -> [String] -> Assertion
assertTrap expected = assertSourceTrap expected . toSource

assertSourceTrap :: Text -> String -> Assertion
assertSourceTrap = assertSourceTrapIo mempty

assertSourceTrapIo :: IntMap ([Int32], [Int32]) -> Text -> String -> Assertion
assertSourceTrapIo streams expected src =
    case runSourceIo streams src of
        Left err | expected `T.isInfixOf` err -> return ()
        Left err -> assertFailure [i|expected #{show expected :: Text}, got: #{err}|]
        Right _ -> assertFailure [i|expected a trap matching #{show expected :: Text}|]

-- | A field guard fires from 'resolveRef' as an @error@ while the translated
-- instructions are forced, so walking @show@ is what provokes it -- the same
-- thing @derefSection@ does for real.
assertTranslationError :: Text -> [String] -> Assertion
assertTranslationError expected instrs = do
    let translated = translate @Wasm32Isa @Int32 1000 (repeat 0) "-" (toSource instrs)
    outcome <- try @ErrorCall $ evaluate $ length (show translated :: String)
    case outcome of
        Left (ErrorCallWithLocation msg _location) -> expect (toText msg)
        Right _ -> case translated of
            Left err -> expect err
            Right _ -> assertFailure [i|expected a translation error matching #{show expected :: Text}|]
    where
        expect err
            | expected `T.isInfixOf` err = return ()
            | otherwise = assertFailure [i|expected #{show expected :: Text}, got: #{err}|]

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
