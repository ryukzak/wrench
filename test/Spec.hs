import Data.Default
import Data.Text (replace, toTitle)
import Relude
import System.FilePath
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Rerun (defaultMainWithRerun)
import Text.Pretty.Simple (pShowNoColor)
import Wrench.Config
import Wrench.Isa.Acc32 (Acc32State)
import Wrench.Isa.Acc32 qualified as Acc32
import Wrench.Isa.F32a (F32aState)
import Wrench.Isa.F32a qualified as F32a
import Wrench.Isa.M68k (M68kState)
import Wrench.Isa.M68k qualified as M68k
import Wrench.Isa.M68k.Test qualified
import Wrench.Isa.RiscIv (RiscIvState)
import Wrench.Isa.RiscIv qualified as RiscIv
import Wrench.Isa.RiscIv.Test qualified
import Wrench.Isa.VliwIv (VliwIvState)
import Wrench.Isa.VliwIv qualified as VliwIv
import Wrench.Isa.VliwIv.Test qualified
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
                    , goldenSimulateFail
                        RiscIv
                        "test/golden/risc-iv-32/factorial.s"
                        "test/golden/risc-iv-32/factorial_input_5_fail_assert.yaml"
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
                "Simulator"
                [ goldenSimulate F32a "test/golden/f32a/div.s" "test/golden/f32a/div_27_4.yaml"
                , goldenSimulate F32a "test/golden/f32a/div.s" "test/golden/f32a/div_3_2.yaml"
                , goldenSimulateFail F32a "test/golden/f32a/div.s" "test/golden/f32a/div_2_3.yaml"
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
        , testGroup
            "M68k"
            [ testGroup
                "Translator"
                [ goldenTranslate M68k "test/golden/m68k/factorial.s"
                , goldenTranslate M68k "test/golden/m68k/get_put_char.s"
                , goldenTranslate M68k "test/golden/m68k/hello.s"
                , goldenTranslate M68k "test/golden/m68k/logical_not.s"
                , goldenTranslate M68k "test/golden/m68k/shift_commands.s"
                ]
            , Wrench.Isa.M68k.Test.tests
            , testGroup
                "Simulator"
                [ goldenSimulate M68k "test/golden/m68k/shift_commands.s" "test/golden/m68k/shift_commands.yaml"
                , goldenSimulate M68k "test/golden/m68k/hello-byte.s" "test/golden/m68k/hello.yaml"
                ]
            , testGroup
                "Generated tests"
                [ generatedTest M68k "factorial" 11
                , generatedTest M68k "get_put_char" 12
                , generatedTest M68k "hello" 1
                , generatedTest M68k "logical_not" 2
                ]
            ]
        , testGroup
            "VLIW-IV"
            [ testGroup
                "Translator"
                [ goldenTranslate VliwIv "test/golden/vliw-iv/count.s"
                , goldenTranslate VliwIv "test/golden/vliw-iv/factorial.s"
                , goldenTranslate VliwIv "test/golden/vliw-iv/hello.s"
                , goldenTranslate VliwIv "test/golden/vliw-iv/all.s"
                , goldenTranslate VliwIv "test/golden/vliw-iv/lui_addi.s"
                ]
            , Wrench.Isa.VliwIv.Test.tests
            , testGroup
                "Simulator"
                [ goldenSimulate VliwIv "test/golden/vliw-iv/count.s" "test/golden/vliw-iv/count.yaml"
                , goldenSimulate VliwIv "test/golden/vliw-iv/get_put_char.s" "test/golden/vliw-iv/get_put_char_87.yaml"
                , goldenSimulate VliwIv "test/golden/vliw-iv/get_put_char.s" "test/golden/vliw-iv/get_put_char_abcd.yaml"
                , goldenSimulate VliwIv "test/golden/vliw-iv/get_put_char.s" "test/golden/vliw-iv/get_put_char_null.yaml"
                , goldenSimulate VliwIv "test/golden/vliw-iv/get_put_char.s" "test/golden/vliw-iv/get_put_char_nothing.yaml"
                , goldenSimulate VliwIv "test/golden/vliw-iv/ble_bleu.s" "test/golden/vliw-iv/ble_bleu.yaml"
                , goldenSimulate VliwIv "test/golden/vliw-iv/lui_addi.s" "test/golden/vliw-iv/lui_addi.yaml"
                , goldenSimulate VliwIv "test/golden/vliw-iv/sb.s" "test/golden/vliw-iv/sb.yaml"
                , goldenSimulate VliwIv "test/golden/vliw-iv/hello.s" "test/golden/vliw-iv/hello.yaml"
                , testGroup
                    "Factorial"
                    [ goldenSimulate VliwIv "test/golden/vliw-iv/factorial.s" "test/golden/vliw-iv/factorial_input_5.yaml"
                    , goldenSimulateFail
                        VliwIv
                        "test/golden/vliw-iv/factorial.s"
                        "test/golden/vliw-iv/factorial_input_5_fail_assert.yaml"
                    , goldenSimulate VliwIv "test/golden/vliw-iv/factorial.s" "test/golden/vliw-iv/factorial_input_7.yaml"
                    ]
                , testGroup
                    "Factorial Rec"
                    [ goldenSimulate
                        VliwIv
                        "test/golden/vliw-iv/factorial_rec.s"
                        "test/golden/vliw-iv/factorial_rec_input_5.yaml"
                    ]
                , testGroup
                    "Generated tests"
                    [ generatedTest VliwIv "factorial" 11
                    , generatedTest' VliwIv "factorial_rec" "factorial" 11
                    , generatedTest VliwIv "get_put_char" 12
                    , generatedTest VliwIv "hello" 1
                    , generatedTest VliwIv "logical_not" 2
                    ]
                ]
            ]
        ]

isaPath :: (IsString a) => Isa -> a
isaPath isa = case isa of
    RiscIv -> "risc-iv-32"
    F32a -> "f32a"
    Acc32 -> "acc32"
    M68k -> "m68k"
    VliwIv -> "vliw-iv"

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
goldenTranslate M68k fn = goldenTranslate' @M68k.Isa M68k fn
goldenTranslate VliwIv fn = goldenTranslate' @VliwIv.Isa VliwIv fn

goldenTranslate' ::
    forall (isa :: Type -> Type -> Type).
    ( ByteSize (isa Int32 (Ref Int32))
    , ByteSize (isa Int32 Int32)
    , DerefMnemonic (isa Int32) Int32
    , MnemonicParser (isa Int32 (Ref Int32))
    , Show (isa Int32 Int32)
    ) =>
    Isa
    -> FilePath
    -> TestTree
goldenTranslate' isa fn =
    goldenVsString (fn2name fn) (fn <> "." <> isaPath isa <> ".result") $ do
        src <- decodeUtf8 <$> readFileBS fn
        case translate @isa @Int32 1000 fn src of
            Right (TranslatorResult dump labels) ->
                return $ encodeUtf8 $ intercalate "\n---\n" [prettyLabels labels, prettyDump labels $ dumpCells dump, ""]
            Left err ->
                error $ "Translation failed: " <> show err

goldenSimulate :: Isa -> FilePath -> FilePath -> TestTree
goldenSimulate = goldenSimulate' False

goldenSimulateFail :: Isa -> FilePath -> FilePath -> TestTree
goldenSimulateFail = goldenSimulate' True

goldenSimulate' :: Bool -> Isa -> FilePath -> FilePath -> TestTree
goldenSimulate' shouldFail isa =
    case isa of
        RiscIv -> goldenSimulateInner (wrench @(RiscIvState Int32)) ".risc-iv-32.result" shouldFail
        F32a -> goldenSimulateInner (wrench @(F32aState Int32)) ".f32a.result" shouldFail
        Acc32 -> goldenSimulateInner (wrench @(Acc32State Int32)) ".acc32.result" shouldFail
        M68k -> goldenSimulateInner (wrench @(M68kState Int32)) ".m68k.result" shouldFail
        VliwIv -> goldenSimulateInner (wrench @(VliwIvState Int32)) ".vliw-iv.result" shouldFail
    where
        goldenSimulateInner wrench' ext shouldFail' fn confFn =
            let testName = "Test case: " <> fn2name confFn
                goldenPath = dropExtension confFn <> ext
                action = do
                    src <- decodeUtf8 <$> readFileBS fn
                    conf <- either (error . toText) id <$> readConfig confFn
                    return $ wrench' def{input = fn} conf src
                stringAction = do
                    result <- action
                    return $ encodeUtf8 $ case result of
                        Right Result{rTrace} -> rTrace
                        Left e -> "error: " <> e
             in testGroup
                    testName
                    [ testCase "Check simulation report" $ do
                        result <- action
                        assertBool "Simulation report received" $ isRight result
                        case result of
                            Right Result{rSuccess}
                                | shouldFail' -> assertBool "Simulation report success" $ not rSuccess
                                | otherwise -> assertBool "Simulation report success" rSuccess
                            Left _ -> return ()
                    , goldenVsString "golden check" goldenPath stringAction
                    ]
