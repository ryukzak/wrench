import Config
import Data.Default
import Data.Text (replace, toTitle)
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
                [ goldenSimulate
                    RiscIv
                    "test/golden/risc-iv-32/count.s"
                    "test/golden/risc-iv-32/default.yaml"
                , goldenSimulate
                    RiscIv
                    "test/golden/risc-iv-32/hello.s"
                    "test/golden/risc-iv-32/hello-const.yaml"
                , goldenSimulate
                    RiscIv
                    "test/golden/risc-iv-32/get-put-char.s"
                    "test/golden/risc-iv-32/get-put-char-87.yaml"
                , goldenSimulate
                    RiscIv
                    "test/golden/risc-iv-32/get-put-char.s"
                    "test/golden/risc-iv-32/get-put-char-abcd.yaml"
                , goldenSimulate
                    RiscIv
                    "test/golden/risc-iv-32/get-put-char.s"
                    "test/golden/risc-iv-32/get-put-char-null.yaml"
                , testGroup
                    "Factorial"
                    [ goldenSimulate
                        RiscIv
                        "test/golden/risc-iv-32/factorial.s"
                        "test/golden/risc-iv-32/factorial-input-5.yaml"
                    , goldenSimulate
                        RiscIv
                        "test/golden/risc-iv-32/factorial.s"
                        "test/golden/risc-iv-32/factorial-input-5-fail-assert.yaml"
                    , goldenSimulate
                        RiscIv
                        "test/golden/risc-iv-32/factorial.s"
                        "test/golden/risc-iv-32/factorial-input-7.yaml"
                    ]
                , testGroup
                    "Generated tests"
                    [ testGroup
                        "factorial"
                        [ goldenSimulate
                            RiscIv
                            "test/golden/risc-iv-32/factorial.s"
                            "test/golden/variant-generator/factorial/1.yaml"
                        , goldenSimulate
                            RiscIv
                            "test/golden/risc-iv-32/factorial.s"
                            "test/golden/variant-generator/factorial/2.yaml"
                        , goldenSimulate
                            RiscIv
                            "test/golden/risc-iv-32/factorial.s"
                            "test/golden/variant-generator/factorial/3.yaml"
                        , goldenSimulate
                            RiscIv
                            "test/golden/risc-iv-32/factorial.s"
                            "test/golden/variant-generator/factorial/4.yaml"
                        , goldenSimulate
                            RiscIv
                            "test/golden/risc-iv-32/factorial.s"
                            "test/golden/variant-generator/factorial/5.yaml"
                        , goldenSimulate
                            RiscIv
                            "test/golden/risc-iv-32/factorial.s"
                            "test/golden/variant-generator/factorial/6.yaml"
                        ]
                    , testGroup
                        "hello"
                        [ goldenSimulate
                            RiscIv
                            "test/golden/risc-iv-32/hello.s"
                            "test/golden/variant-generator/hello/1.yaml"
                        ]
                    , testGroup
                        "get_put_char"
                        [ goldenSimulate
                            RiscIv
                            "test/golden/risc-iv-32/get-put-char.s"
                            "test/golden/variant-generator/get_put_char/1.yaml"
                        , goldenSimulate
                            RiscIv
                            "test/golden/risc-iv-32/get-put-char.s"
                            "test/golden/variant-generator/get_put_char/2.yaml"
                        , goldenSimulate
                            RiscIv
                            "test/golden/risc-iv-32/get-put-char.s"
                            "test/golden/variant-generator/get_put_char/3.yaml"
                        , goldenSimulate
                            RiscIv
                            "test/golden/risc-iv-32/get-put-char.s"
                            "test/golden/variant-generator/get_put_char/4.yaml"
                        , goldenSimulate
                            RiscIv
                            "test/golden/risc-iv-32/get-put-char.s"
                            "test/golden/variant-generator/get_put_char/5.yaml"
                        , goldenSimulate
                            RiscIv
                            "test/golden/risc-iv-32/get-put-char.s"
                            "test/golden/variant-generator/get_put_char/6.yaml"
                        ]
                    , testGroup
                        "logical_not"
                        [ goldenSimulate
                            RiscIv
                            "test/golden/risc-iv-32/logical-not.s"
                            "test/golden/variant-generator/logical_not/1.yaml"
                        , goldenSimulate
                            RiscIv
                            "test/golden/risc-iv-32/logical-not.s"
                            "test/golden/variant-generator/logical_not/2.yaml"
                        ]
                    ]
                ]
            ]
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
goldenSimulate isa _fn _confFn = error $ "Unsupported architecture: " <> show isa
