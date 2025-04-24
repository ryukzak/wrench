import Data.Default
import Data.Text (replace, toTitle)
import Relude
import System.FilePath
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.Ingredients.Rerun (defaultMainWithRerun)
import Text.Pretty.Simple (pShowNoColor)
import Wrench.Config
import Wrench.Isa.Acc32 (Acc32State)
import Wrench.Isa.Acc32 qualified as Acc32
import Wrench.Isa.F32a (F32aState)
import Wrench.Isa.F32a qualified as F32a
import Wrench.Isa.RiscIv (RiscIvState)
import Wrench.Isa.RiscIv qualified as RiscIv
import Wrench.Isa.RiscIv.Test qualified
import Wrench.Machine.Memory
import Wrench.Machine.Memory.Test qualified
import Wrench.Machine.Types
import Wrench.Machine.Types.Test qualified
import Wrench.Report.Test qualified
import Wrench.Translator
import Wrench.Translator.Parser.Types
import Wrench.Translator.Types
import Wrench.Wrench

main :: IO ()
main = defaultMainWithRerun tests

tests :: TestTree
tests =
    testGroup
        "Wrench"
        [ testGroup
            "Config"
            [ goldenConfig "test/golden/config/bad_no_limit.yaml"
            , goldenConfig "test/golden/config/bad_no_memory_size.yaml"
            , goldenConfig "test/golden/config/bad_too_much_limit.yaml"
            , goldenConfig "test/golden/config/only_strict.yaml"
            , goldenConfig "test/golden/config/smoke.yaml"
            ]
        , testGroup "Report" [Wrench.Report.Test.tests]
        , Wrench.Machine.Types.Test.tests
        , Wrench.Machine.Memory.Test.tests
        , testGroup
            "RiscIv IV 32"
            [ testGroup
                "Translator"
                [ goldenTranslate RiscIv "test/golden/risc-iv-32/count.s"
                , goldenTranslate RiscIv "test/golden/risc-iv-32/factorial.s"
                , goldenTranslate RiscIv "test/golden/risc-iv-32/hello.s"
                , goldenTranslate RiscIv "test/golden/risc-iv-32/all.s"
                , goldenTranslate RiscIv "test/golden/risc-iv-32/lui_addi.s"
                ]
            , Wrench.Isa.RiscIv.Test.tests
            , testGroup
                "Simulator"
                [ goldenSimulate RiscIv "test/golden/risc-iv-32/count.s" "test/golden/risc-iv-32/count.yaml"
                , goldenSimulate RiscIv "test/golden/risc-iv-32/get_put_char.s" "test/golden/risc-iv-32/get_put_char_87.yaml"
                , goldenSimulate RiscIv "test/golden/risc-iv-32/get_put_char.s" "test/golden/risc-iv-32/get_put_char_abcd.yaml"
                , goldenSimulate RiscIv "test/golden/risc-iv-32/get_put_char.s" "test/golden/risc-iv-32/get_put_char_null.yaml"
                , goldenSimulate RiscIv "test/golden/risc-iv-32/get_put_char.s" "test/golden/risc-iv-32/get_put_char_nothing.yaml"
                , goldenSimulate RiscIv "test/golden/risc-iv-32/ble_bleu.s" "test/golden/risc-iv-32/ble_bleu.yaml"
                , goldenSimulate RiscIv "test/golden/risc-iv-32/lui_addi.s" "test/golden/risc-iv-32/lui_addi.yaml"
                , goldenSimulate RiscIv "test/golden/risc-iv-32/sb.s" "test/golden/risc-iv-32/sb.yaml"
                , testGroup
                    "Factorial"
                    [ goldenSimulate RiscIv "test/golden/risc-iv-32/factorial.s" "test/golden/risc-iv-32/factorial_input_5.yaml"
                    , goldenSimulate RiscIv "test/golden/risc-iv-32/factorial.s" "test/golden/risc-iv-32/factorial_input_5_fail_assert.yaml"
                    , goldenSimulate RiscIv "test/golden/risc-iv-32/factorial.s" "test/golden/risc-iv-32/factorial_input_7.yaml"
                    ]
                , testGroup
                    "Factorial Rec"
                    [ goldenSimulate
                        RiscIv
                        "test/golden/risc-iv-32/factorial_rec.s"
                        "test/golden/risc-iv-32/factorial_rec_input_5.yaml"
                    ]
                , testGroup
                    "Generated tests"
                    [ generatedTest RiscIv "factorial" 11
                    , generatedTest' RiscIv "factorial_rec" "factorial" 11
                    , generatedTest RiscIv "get_put_char" 12
                    , generatedTest RiscIv "hello" 1
                    , generatedTest RiscIv "logical_not" 2
                    ]
                ]
            ]
        , testGroup
            "F32a"
            [ testGroup
                "Translator"
                [ goldenTranslate F32a "test/golden/f32a/logical_not.s"
                , goldenTranslate F32a "test/golden/f32a/hello.s"
                , goldenTranslate F32a "test/golden/f32a/div.s"
                , goldenTranslate F32a "test/golden/f32a/factorial.s"
                , goldenTranslate F32a "test/golden/f32a/jmp_and_call.s"
                ]
            , testGroup
                "F32a"
                [ goldenSimulate F32a "test/golden/f32a/div.s" "test/golden/f32a/div_27_4.yaml"
                , goldenSimulate F32a "test/golden/f32a/div.s" "test/golden/f32a/div_3_2.yaml"
                , goldenSimulate F32a "test/golden/f32a/div.s" "test/golden/f32a/div_2_3.yaml"
                , goldenSimulate F32a "test/golden/f32a/carry.s" "test/golden/f32a/carry.yaml"
                , goldenSimulate F32a "test/golden/f32a/factorial.s" "test/golden/f32a/factorial.yaml"
                , goldenSimulate F32a "test/golden/f32a/jmp_and_call.s" "test/golden/f32a/jmp_and_call.yaml"
                , goldenSimulate F32a "test/golden/f32a/get_put_char.s" "test/golden/f32a/get_put_char_nothing.yaml"
                ]
            , testGroup
                "Generated tests"
                [ generatedTest F32a "factorial" 11
                , generatedTest F32a "get_put_char" 12
                , generatedTest F32a "hello" 1
                , generatedTest F32a "logical_not" 2
                ]
            ]
        , testGroup
            "Acc32"
            [ testGroup
                "Translator"
                [ goldenTranslate Acc32 "test/golden/acc32/logical_not.s"
                , goldenTranslate Acc32 "test/golden/acc32/hello.s"
                , goldenTranslate Acc32 "test/golden/acc32/get_put_char.s"
                , goldenTranslate Acc32 "test/golden/acc32/factorial.s"
                , goldenTranslate Acc32 "test/golden/acc32/all.s"
                , goldenTranslate Acc32 "test/golden/acc32/relative.s"
                , goldenTranslate Acc32 "test/golden/acc32/label_like_instr.s"
                ]
            , testGroup
                "Acc32"
                [ goldenSimulate Acc32 "test/golden/acc32/error_sym.s" "test/golden/acc32/error_sym.yaml"
                , goldenSimulate Acc32 "test/golden/acc32/overflow.s" "test/golden/acc32/overflow.yaml"
                , goldenSimulate Acc32 "test/golden/acc32/get_put_char.s" "test/golden/acc32/get_put_char_nothing.yaml"
                ]
            , testGroup
                "Generated tests"
                [ generatedTest Acc32 "factorial" 11
                , generatedTest Acc32 "get_put_char" 12
                , generatedTest Acc32 "hello" 1
                , generatedTest Acc32 "logical_not" 2
                , generatedTest Acc32 "dup" 1
                ]
            ]
        ]

isaPath :: (IsString a) => Isa -> a
isaPath isa = case isa of
    RiscIv -> "risc-iv-32"
    F32a -> "f32a"
    Acc32 -> "acc32"

generatedTest' :: Isa -> String -> String -> Int -> TestTree
generatedTest' isa sname vname n = testGroup sname testCases
    where
        testCases =
            [ goldenSimulate
                isa
                ("test/golden/" <> isaPath isa <> "/" <> sname <> ".s")
                ("test/golden/generated/" <> vname <> "/" <> show i <> ".yaml")
            | i <- [1 .. n]
            ]

generatedTest :: Isa -> String -> Int -> TestTree
generatedTest isa name = generatedTest' isa name name

goldenConfig :: FilePath -> TestTree
goldenConfig fn =
    goldenVsString (fn2name fn) (fn <> ".result") $ do
        conf <- either pShowNoColor pShowNoColor <$> readConfig fn
        return $ encodeUtf8 (conf <> "\n")

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
goldenTranslate RiscIv fn = goldenTranslate' @RiscIv.Isa RiscIv fn
goldenTranslate F32a fn = goldenTranslate' @F32a.Isa F32a fn
goldenTranslate Acc32 fn = goldenTranslate' @Acc32.Isa Acc32 fn

goldenTranslate' ::
    forall (isa :: Type -> Type -> Type).
    ( ByteLength (isa Int32 (Ref Int32))
    , ByteLength (isa Int32 Int32)
    , DerefMnemonic (isa Int32) Int32
    , MnemonicParser (isa Int32 (Ref Int32))
    , Show (isa Int32 Int32)
    ) =>
    Isa -> FilePath -> TestTree
goldenTranslate' isa fn =
    goldenVsString (fn2name fn) (fn <> "." <> isaPath isa <> ".result") $ do
        src <- decodeUtf8 <$> readFileBS fn
        case translate @isa @Int32 Nothing fn src of
            Right (TranslatorResult dump labels) ->
                return $ encodeUtf8 $ intercalate "\n---\n" [prettyLabels labels, prettyDump labels $ dumpCells dump, ""]
            Left err ->
                error $ "Translation failed: " <> show err

goldenSimulate :: Isa -> FilePath -> FilePath -> TestTree
goldenSimulate isa =
    case isa of
        RiscIv -> goldenSimulate' (wrench @(RiscIvState Int32)) ".risc-iv-32.result"
        F32a -> goldenSimulate' (wrench @(F32aState Int32)) ".f32a.result"
        Acc32 -> goldenSimulate' (wrench @(Acc32State Int32)) ".acc32.result"
    where
        goldenSimulate' wr ext fn confFn =
            let resultFn = dropExtension confFn <> ext
             in goldenVsString (fn2name confFn) resultFn $ do
                    let wrench' = wr
                    src <- decodeUtf8 <$> readFileBS fn
                    conf <- either (error . toText) id <$> readConfig confFn
                    return $ encodeUtf8 $ case wrench' conf def{input = fn} src of
                        Right Result{rTrace} -> rTrace
                        Left e -> "error: " <> e
