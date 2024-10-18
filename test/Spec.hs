import Config
import Data.Default
import Data.Text (replace, toTitle)
import Isa.F18a qualified as F18a
import Isa.RiscIv qualified as RiscIv
import Isa.RiscIv.Test qualified
import Machine.Memory
import Machine.Types
import Relude
import System.FilePath
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Text.Pretty.Simple (pShowNoColor)
import Translator
import Wrench

main :: IO ()
main = defaultMain tests

memorySize :: Int
memorySize = 128

tests :: TestTree
tests =
    testGroup
        "ca-wrench"
        [ testGroup
            "Config"
            [ goldenConfig "test/golden/config/bad-no-limit.yaml"
            , goldenConfig "test/golden/config/bad-no-memory-size.yaml"
            , goldenConfig "test/golden/config/bad-too-much-limit.yaml"
            , goldenConfig "test/golden/config/only-strict.yaml"
            , goldenConfig "test/golden/config/smoke.yaml"
            ]
        , testGroup
            "RiscIv IV 32"
            [ testGroup
                "Translator"
                [ goldenTranslate RiscIv "test/golden/risc-iv-32/count.s"
                , goldenTranslate RiscIv "test/golden/risc-iv-32/factorial.s"
                , goldenTranslate RiscIv "test/golden/risc-iv-32/hello.s"
                ]
            , Isa.RiscIv.Test.tests
            , testGroup
                "Simulator"
                [ goldenSimulate RiscIv "test/golden/risc-iv-32/count.s" "test/golden/risc-iv-32/default.yaml"
                , goldenSimulate RiscIv "test/golden/risc-iv-32/hello.s" "test/golden/risc-iv-32/hello-const.yaml"
                , goldenSimulate RiscIv "test/golden/risc-iv-32/get-put-char.s" "test/golden/risc-iv-32/get-put-char-87.yaml"
                , goldenSimulate RiscIv "test/golden/risc-iv-32/get-put-char.s" "test/golden/risc-iv-32/get-put-char-abcd.yaml"
                , goldenSimulate RiscIv "test/golden/risc-iv-32/get-put-char.s" "test/golden/risc-iv-32/get-put-char-null.yaml"
                , testGroup
                    "Factorial"
                    [ goldenSimulate RiscIv "test/golden/risc-iv-32/factorial.s" "test/golden/risc-iv-32/factorial-input-5.yaml"
                    , goldenSimulate RiscIv "test/golden/risc-iv-32/factorial.s" "test/golden/risc-iv-32/factorial-input-5-fail-assert.yaml"
                    , goldenSimulate RiscIv "test/golden/risc-iv-32/factorial.s" "test/golden/risc-iv-32/factorial-input-7.yaml"
                    ]
                , testGroup
                    "Generated tests"
                    [ generatedTest RiscIv "factorial" "test/golden/risc-iv-32/factorial.s" [1 .. 6]
                    , generatedTest RiscIv "hello" "test/golden/risc-iv-32/hello.s" [1]
                    , generatedTest RiscIv "get_put_char" "test/golden/risc-iv-32/get-put-char.s" [1 .. 6]
                    , generatedTest RiscIv "logical_not" "test/golden/risc-iv-32/logical-not.s" [1 .. 2]
                    ]
                ]
            ]
        , testGroup
            "F18a"
            [ testGroup
                "Translator"
                [ goldenTranslate F18a "test/golden/f18a/logical-not.s"
                , goldenTranslate F18a "test/golden/f18a/hello.s"
                , goldenTranslate F18a "test/golden/f18a/div.s"
                ]
            , testGroup
                "F18a"
                [ goldenSimulate F18a "test/golden/f18a/div.s" "test/golden/f18a/div-27-4.yaml"
                , goldenSimulate F18a "test/golden/f18a/div.s" "test/golden/f18a/div-3-2.yaml"
                , goldenSimulate F18a "test/golden/f18a/div.s" "test/golden/f18a/div-2-3.yaml"
                ]
            , testGroup
                "Generated tests"
                [ generatedTest F18a "hello" "test/golden/f18a/hello.s" [1]
                , generatedTest F18a "get_put_char" "test/golden/f18a/get-put-char.s" [1 .. 6]
                , generatedTest F18a "logical_not" "test/golden/f18a/logical-not.s" [1 .. 2]
                , generatedTest F18a "factorial" "test/golden/f18a/factorial.s" [1 .. 6]
                ]
            ]
        ]

generatedTest :: Isa -> String -> FilePath -> [Int] -> TestTree
generatedTest isa name asmFn range = testGroup name testCases
    where
        testCases =
            [ goldenSimulate
                isa
                asmFn
                ("test/golden/variant-generator/" <> name <> "/" <> show i <> ".yaml")
            | i <- range
            ]

goldenConfig :: FilePath -> TestTree
goldenConfig fn =
    goldenVsString (fn2name fn) (fn <> ".result") $ do
        conf <- either pShowNoColor pShowNoColor <$> readConfig fn
        return $ encodeUtf8 conf

fn2name :: FilePath -> String
fn2name fn =
    toString
        $ toTitle
        $ replace "_" " "
        $ replace "-" " "
        $ toText
        $ dropExtension
        $ takeFileName fn

goldenTranslate :: Isa -> FilePath -> TestTree
goldenTranslate RiscIv fn =
    goldenVsString (fn2name fn) (fn <> ".risc-iv-32.result") $ do
        src <- decodeUtf8 <$> readFileBS fn
        case translate @RiscIv.Isa @Int32 memorySize fn src of
            Right (TranslatorResult dump labels) ->
                return $ encodeUtf8 $ intercalate "\n---\n" [prettyLabels labels, prettyDump labels dump]
            Left err ->
                error $ "Translation failed: " <> show err
goldenTranslate F18a fn =
    goldenVsString (fn2name fn) (fn <> ".f18a.result") $ do
        src <- decodeUtf8 <$> readFileBS fn
        case translate @F18a.Isa @Int32 memorySize fn src of
            Right (TranslatorResult dump labels) ->
                return $ encodeUtf8 $ intercalate "\n---\n" [prettyLabels labels, prettyDump labels dump]
            Left err ->
                error $ "Translation failed: " <> show err

goldenSimulate :: Isa -> FilePath -> FilePath -> TestTree
goldenSimulate RiscIv fn confFn =
    let resultFn = dropExtension confFn <> ".risc-iv-32.result"
     in goldenVsString (fn2name confFn) resultFn $ do
            let wrench' = wrench @RiscIv.Isa @RiscIv.Register @Int32 @(RiscIv.MachineState (IoMem (RiscIv.Isa Int32 Int32) Int32) Int32)
            src <- decodeUtf8 <$> readFileBS fn
            conf <- either (error . toText) id <$> readConfig confFn
            return $ encodeUtf8 $ case wrench' conf def{input = fn} src of
                Right Result{rTrace} -> rTrace
                Left e -> toString $ "error: " <> e
goldenSimulate F18a fn confFn =
    let resultFn = dropExtension confFn <> ".f18a.result"
     in goldenVsString (fn2name confFn) resultFn $ do
            let wrench' = wrench @F18a.Isa @F18a.Register @Int32 @(F18a.MachineState (IoMem (F18a.Isa Int32 Int32) Int32) Int32)
            src <- decodeUtf8 <$> readFileBS fn
            conf <- either (error . toText) id <$> readConfig confFn
            return $ encodeUtf8 $ case wrench' conf def{input = fn} src of
                Right Result{rTrace} -> rTrace
                Left e -> toString $ "error: " <> e
