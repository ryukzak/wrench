import Config
import Data.Default
import Data.Text (replace, toTitle)
import Isa.Risc
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
                ]
            , testGroup
                "Simulator"
                [ goldenSimulate
                    "test/golden/risc-v-32-like/count.s"
                    "test/golden/risc-v-32-like/default.yaml"
                , goldenSimulate
                    "test/golden/risc-v-32-like/factorial.s"
                    "test/golden/risc-v-32-like/input-5.yaml"
                , goldenSimulate
                    "test/golden/risc-v-32-like/factorial.s"
                    "test/golden/risc-v-32-like/input-5-fail-assert.yaml"
                , goldenSimulate
                    "test/golden/risc-v-32-like/factorial.s"
                    "test/golden/risc-v-32-like/input-7.yaml"
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
    let resultFn = dropExtension fn <> "-" <> dropExtension (takeFileName confFn) <> ".result"
     in goldenVsString (fn2name fn <> ": " <> fn2name confFn) resultFn $ do
            let wrench' = wrench @Risc @Register @Int32 @(MachineState (IoMem (Risc Int32 Int32) Int32) Int32)
            src <- decodeUtf8 <$> readFileBS fn
            conf <- either (error . toText) id <$> readConfig confFn
            return $ encodeUtf8 $ case wrench' conf def{input = fn} src of
                Right Result{rTrace} -> rTrace
                Left e -> toString $ "error: " <> e
