{-# LANGUAGE QuasiQuotes #-}

module Wrench.Isa.Wasm32.Test (tests) where

import Control.Exception (ErrorCall (..), evaluate, try)
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Hedgehog (Gen, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Prelude qualified
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Test.Tasty.Hedgehog (testPropertyNamed)
import Wrench.Isa.Wasm32
import Wrench.Isa.Wasm32.LocalNames (resolveLocalNames)
import Wrench.Isa.Wasm32.State (Scope (..), Wasm32St)
import Wrench.Machine.BitPack (packWords, unpackWords)
import Wrench.Machine.Types
import Wrench.Report (renderIsaView)
import Wrench.Translator (TranslatorResult (..), translate)

-- | What the golden corpus in @test\/golden\/wasm32@ cannot check: the
-- arithmetic and memory edge cases no program there reaches, and every way
-- the machine is meant to refuse a program.
tests :: TestTree
tests =
    testGroup
        "ISA"
        [ testGroup
            "Control record"
            [ testPropertyNamed "A record round-trips" "prop_scopeRoundTrip" $ property $ do
                scope <- forAll genScope
                (unpackWords =<< packWords @Int32 scope) === Right scope
            , testPropertyNamed "Any two words decode to a record that round-trips, or fail on the tag" "prop_scopeDecodeAny" $
                property $ do
                    words_ <- forAll $ Gen.list (Range.singleton 2) (Gen.int32 Range.constantBounded)
                    case unpackWords @Scope words_ of
                        Left err -> err === "record kind: unknown tag 3"
                        Right scope -> (unpackWords =<< packWords @Int32 scope) === Right scope
            , testPropertyNamed "A field too wide for the record is named in the error" "prop_scopeOverflow" $
                property $ do
                    (name, width, value, scope) <- forAll genOverflow
                    packWords @Int32 scope === Left [i|#{name}: #{value} doesn't fit in #{width} bits|]
            , testCase "An unknown tag is rejected" $
                unpackWords @Scope [-1 :: Int32, 0] @?= Left "record kind: unknown tag 3"
            ]
        , testCase "i32.div_s truncates toward zero and i32.rem_s follows the dividend" $ do
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
        , testCase "A local's name is the same instruction as its index" $ do
            let program uses =
                    Prelude.unlines $
                        [".text", "_start:", "    .frame () $a, $b", "    locals.reserve 2"] <> map ("    " <>) uses <> ["    halt"]
            byName <-
                runSourceToStack $ program ["i32.const 3 local.set $a", "i32.const 4 local.set $b", "local.get $a local.get 1 i32.sub"]
            byIndex <-
                runSourceToStack $ program ["i32.const 3 local.set 0", "i32.const 4 local.set 1", "local.get 0 local.get 1 i32.sub"]
            byName @?= "-1"
            byIndex @?= byName
        , testCase "The reserved locals are numbered after the parameters" $ do
            stack <-
                runSourceToStack $
                    Prelude.unlines
                        [ ".text"
                        , "f:"
                        , "    .frame ($a, $b) $c"
                        , "    locals.reserve 1"
                        , "    i32.const 30 local.set $c"
                        , "    local.get $a local.get $b local.get 2"
                        , "    halt"
                        , "_start:"
                        , "    i32.const 10 i32.const 20"
                        , "    i32.const f call 2, 0"
                        ]
            stack @?= "30:20:10"
        , testCase "The parentheses may follow .frame without a space" $ do
            stack <- runSourceToStack $ toSource [".frame() $a", "locals.reserve 1", "i32.const 7 local.set $a", "local.get $a"]
            stack @?= "7"
        , testCase "Names hold until the next label"
            $ assertSourceTranslationError "-:6:15:"
            $ Prelude.unlines [".text", "f:", "    .frame ($a)", "    local.get $a", "_start:", "    local.get $a", "    halt"]
        , testCase "A .frame out of form is rejected where it is written" $ do
            let malformed = "malformed .frame, expected distinct names as in .frame ($a, $b) $c"
            assertTranslationError malformed [".frame $a"]
            assertTranslationError "-:3:5:" [".frame $a"]
            assertTranslationError malformed [".frame"]
            assertTranslationError malformed [".frame ($a, $b"]
            assertTranslationError malformed [".frame ($a) ) $b"]
            assertTranslationError malformed [".frame ($a, b) $c"]
            assertTranslationError malformed [".frame ($a $b)"]
            assertTranslationError malformed [".frame ($a, $a)"]
            assertTranslationError malformed [".frame ($a) $a"]
        , testCase "A name the .frame does not list is left for the parser" $
            assertTranslationError "-:4:15:" [".frame ($a)", "local.get $x"]
        , testCase "A parse error on a line with a name shows the line as written" $
            assertTranslationError "local.get $a i32.konst" [".frame ($a)", "local.get $a i32.konst 1"]
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
        , testCase "Recursive call/return computes factorial(5)" $ do
            src <- readExample "factorial.s"
            case runSourceIo (IntMap.fromList [(0x80, ([5], [])), (0x84, ([], []))]) src of
                Left err -> assertFailure (toString err)
                Right st -> (reverse . snd <$> ioStreams st IntMap.!? 0x84) @?= Just [120]
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
        Right st -> either (\err -> assertFailure (toString err) >> error "unreachable") return (renderIsaView HashMap.empty st view)

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
assertTranslationError expected = assertSourceTranslationError expected . toSource

assertSourceTranslationError :: Text -> String -> Assertion
assertSourceTranslationError expected src = do
    let translated = translate @Wasm32Isa @Int32 [resolveLocalNames] 1000 (repeat 0) "-" src
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
    TranslatorResult{dump, labels} <- translate @Wasm32Isa @Int32 [resolveLocalNames] 1000 (repeat 0) "-" src
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

genScope :: Gen Scope
genScope =
    Gen.choice
        [ BlockScope <$> genField 22
        , LoopScope <$> genField 22 <*> genField 22
        , CallScope <$> genField 22 <*> genField 8 <*> genField 22 <*> genField 8
        ]
    where
        genField :: Int -> Gen Int
        genField width = Gen.int (Range.constant 0 (2 ^ width - 1))

-- | A record with one field pushed past its width: the field's name, width,
-- value, and the record.
genOverflow :: Gen (Text, Int, Int, Scope)
genOverflow = do
    scope <- genScope
    (name, width :: Int, set) <- Gen.element $ case scope of
        BlockScope{} -> [("csEnd", 22, \v -> BlockScope{csEnd = v})]
        LoopScope{csStart, csEnd} ->
            [ ("csStart", 22, \v -> LoopScope{csStart = v, csEnd})
            , ("csEnd", 22, \v -> LoopScope{csStart, csEnd = v})
            ]
        call@CallScope{} ->
            [ ("csCallerFrameBase", 22, \v -> call{csCallerFrameBase = v})
            , ("csCallerLocalCount", 8, \v -> call{csCallerLocalCount = v})
            , ("csReturnPc", 22, \v -> call{csReturnPc = v})
            , ("csResultCount", 8, \v -> call{csResultCount = v})
            ]
    value <- Gen.int (Range.linear (2 ^ width) (2 ^ (width + 4)))
    return (name, width, value, set value)
