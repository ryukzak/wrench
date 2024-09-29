import Config
import Data.Default
import Data.Text (replace, toTitle)
import Isa.Risc
import Isa.Risc.Test qualified
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
            "RISC V 32-bit like"
            [ testGroup
                "Translator"
                [ goldenTranslate "test/golden/risc-v-32-like/count.s"
                , goldenTranslate "test/golden/risc-v-32-like/factorial.s"
                , goldenTranslate "test/golden/risc-v-32-like/hello.s"
                ]
            , testGroup
                "Simulator"
                [ goldenSimulate
                    "test/golden/risc-v-32-like/count.s"
                    "test/golden/risc-v-32-like/default.yaml"
                , goldenSimulate
                    "test/golden/risc-v-32-like/hello.s"
                    "test/golden/risc-v-32-like/hello-const.yaml"
                , goldenSimulate
                    "test/golden/risc-v-32-like/get-put-char.s"
                    "test/golden/risc-v-32-like/get-put-char-87.yaml"
                , goldenSimulate
                    "test/golden/risc-v-32-like/get-put-char.s"
                    "test/golden/risc-v-32-like/get-put-char-abcd.yaml"
                , goldenSimulate
                    "test/golden/risc-v-32-like/get-put-char.s"
                    "test/golden/risc-v-32-like/get-put-char-null.yaml"
                , testGroup
                    "Factorial"
                    [ goldenSimulate
                        "test/golden/risc-v-32-like/factorial.s"
                        "test/golden/risc-v-32-like/factorial-input-5.yaml"
                    , goldenSimulate
                        "test/golden/risc-v-32-like/factorial.s"
                        "test/golden/risc-v-32-like/factorial-input-5-fail-assert.yaml"
                    , goldenSimulate
                        "test/golden/risc-v-32-like/factorial.s"
                        "test/golden/risc-v-32-like/factorial-input-7.yaml"
                    ]
                , testGroup
                    "Generated tests"
                    [ testGroup
                        "factorial"
                        [ goldenSimulate
                            "test/golden/risc-v-32-like/factorial.s"
                            "test/golden/variant-generator/factorial/1.yaml"
                        , goldenSimulate
                            "test/golden/risc-v-32-like/factorial.s"
                            "test/golden/variant-generator/factorial/2.yaml"
                        , goldenSimulate
                            "test/golden/risc-v-32-like/factorial.s"
                            "test/golden/variant-generator/factorial/3.yaml"
                        , goldenSimulate
                            "test/golden/risc-v-32-like/factorial.s"
                            "test/golden/variant-generator/factorial/4.yaml"
                        , goldenSimulate
                            "test/golden/risc-v-32-like/factorial.s"
                            "test/golden/variant-generator/factorial/5.yaml"
                        , goldenSimulate
                            "test/golden/risc-v-32-like/factorial.s"
                            "test/golden/variant-generator/factorial/6.yaml"
                        ]
                    , testGroup
                        "hello"
                        [ goldenSimulate
                            "test/golden/risc-v-32-like/hello.s"
                            "test/golden/variant-generator/hello/1.yaml"
                        ]
                    , testGroup
                        "get_put_char"
                        [ goldenSimulate
                            "test/golden/risc-v-32-like/get-put-char.s"
                            "test/golden/variant-generator/get_put_char/1.yaml"
                        , goldenSimulate
                            "test/golden/risc-v-32-like/get-put-char.s"
                            "test/golden/variant-generator/get_put_char/2.yaml"
                        , goldenSimulate
                            "test/golden/risc-v-32-like/get-put-char.s"
                            "test/golden/variant-generator/get_put_char/3.yaml"
                        , goldenSimulate
                            "test/golden/risc-v-32-like/get-put-char.s"
                            "test/golden/variant-generator/get_put_char/4.yaml"
                        , goldenSimulate
                            "test/golden/risc-v-32-like/get-put-char.s"
                            "test/golden/variant-generator/get_put_char/5.yaml"
                        , goldenSimulate
                            "test/golden/risc-v-32-like/get-put-char.s"
                            "test/golden/variant-generator/get_put_char/6.yaml"
                        ]
                    ]
                ]
            , Isa.Risc.Test.tests
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

goldenTranslate :: FilePath -> TestTree
goldenTranslate fn =
    goldenVsString (fn2name fn) (fn <> ".result") $ do
        src <- decodeUtf8 <$> readFileBS fn
        case translate @Risc @Int32 memorySize fn src of
            Right (TranslatorResult dump labels) ->
                return $ encodeUtf8 $ intercalate "\n---\n" [prettyLabels labels, prettyDump labels dump]
            Left err ->
                error $ "Translation failed: " <> show err

goldenSimulate :: FilePath -> FilePath -> TestTree
goldenSimulate fn confFn =
    let resultFn = dropExtension confFn <> ".result"
     in goldenVsString (fn2name confFn) resultFn $ do
            let wrench' = wrench @Risc @Register @Int32 @(MachineState (IoMem (Risc Int32 Int32) Int32) Int32)
            src <- decodeUtf8 <$> readFileBS fn
            conf <- either (error . toText) id <$> readConfig confFn
            return $ encodeUtf8 $ case wrench' conf def{input = fn} src of
                Right Result{rTrace} -> rTrace
                Left e -> toString $ "error: " <> e
