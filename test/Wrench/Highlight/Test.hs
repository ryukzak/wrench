module Wrench.Highlight.Test (tests) where

import Data.List (isSuffixOf)
import Data.Text qualified as T
import Relude
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))
import Text.Megaparsec (ParseErrorBundle, parse)
import Wrench.Highlight
import Wrench.Isa (Isa (..), allIsa, isaName, parsesAs)
import Wrench.Isa.F32a (f32aMnemonics)
import Wrench.Isa.F32a qualified as F32a
import Wrench.Isa.Wasm32 (wasm32Mnemonics)
import Wrench.Translator.Parser.Types (MnemonicParser (..))
import Wrench.Translator.Types (Ref)

tests :: TestTree
tests =
    testGroup
        "Highlight"
        [ testCase "Tokens concatenate back to every example and golden source" $
            forM_ allIsa $ \isa ->
                forM_ ["example", "test/golden"] $ \root -> do
                    let dir = root </> toString (isaName isa)
                    exists <- doesDirectoryExist dir
                    files <- if exists then filter (".s" `isSuffixOf`) <$> listDirectory dir else return []
                    forM_ files $ \fn -> do
                        src <- decodeUtf8 <$> readFileBS (dir </> fn)
                        forM_ (lines src) $ \line ->
                            assertEqual (dir </> fn) line (T.concat $ map snd $ tokenizeLine isa line)
        , testCase "risc-iv-32" $ do
            significant RiscIv "_start:  lw t0, -4(t0)  ; load"
                @?= [ (Label, "_start:")
                    , (Mnemonic, "lw")
                    , (Register, "t0")
                    , (Literal, "-4")
                    , (Register, "t0")
                    , (Comment, "; load")
                    ]
            significant RiscIv "lui t0, %hi(input_addr)"
                @?= [(Mnemonic, "lui"), (Register, "t0"), (Reloc, "%hi"), (Ref, "input_addr")]
            significant RiscIv "buf: .byte 'a;b', 0x1_0"
                @?= [(Label, "buf:"), (Directive, ".byte"), (Literal, "'a;b'"), (Literal, "0x1_0")]
        , testCase "vliw-iv slots" $
            significant VliwIv "mul t2, t2, t1 / addi t1, t1, -1 / / j loop"
                @?= [ (Mnemonic, "mul")
                    , (Register, "t2")
                    , (Register, "t2")
                    , (Register, "t1")
                    , (Separator, "/")
                    , (Mnemonic, "addi")
                    , (Register, "t1")
                    , (Register, "t1")
                    , (Literal, "-1")
                    , (Separator, "/")
                    , (Separator, "/")
                    , (Mnemonic, "j")
                    , (Ref, "loop")
                    ]
        , testCase "m68k" $
            significant M68k "move.l D0, -(A7)"
                @?= [(Mnemonic, "move.l"), (Register, "D0"), (Register, "A7")]
        , testCase "wasm32" $ do
            significant Wasm32 "i32.const factorial ; call"
                @?= [(Mnemonic, "i32.const"), (Ref, "factorial"), (Comment, "; call")]
            significant Wasm32 "local.get 0 i32.const 1 i32.sub"
                @?= [(Mnemonic, "local.get"), (Literal, "0"), (Mnemonic, "i32.const"), (Literal, "1"), (Mnemonic, "i32.sub")]
            significant Wasm32 ".frame () $n, $acc"
                @?= [(Directive, ".frame"), (Register, "$n"), (Register, "$acc")]
            significant Wasm32 "local.get $n"
                @?= [(Mnemonic, "local.get"), (Register, "$n")]
        , testCase "f32a" $ do
            significant F32a "while: -1 + ; \\ loop"
                @?= [(Label, "while:"), (Literal, "-1"), (Mnemonic, "+"), (Mnemonic, ";"), (Comment, "\\ loop")]
            significant F32a "@p input_addr a! factorial ;"
                @?= [(Mnemonic, "@p"), (Ref, "input_addr"), (Mnemonic, "a!"), (Call, "factorial"), (Mnemonic, ";")]
            significant F32a "if end lit buf next loop"
                @?= [(Mnemonic, "if"), (Ref, "end"), (Mnemonic, "lit"), (Ref, "buf"), (Mnemonic, "next"), (Ref, "loop")]
            significant F32a "buf: .byte 'a\\n'"
                @?= [(Label, "buf:"), (Directive, ".byte"), (Literal, "'a\\n'")]
        , testCase "Every listed mnemonic parses" $ do
            let f32aInstruction m = any (isInstruction . parseF32a . (m <>)) ["\n", " _start\n", " _start ;\n"]
                isInstruction = \case
                    Right (F32a.Call _) -> False
                    Right _ -> True
                    Left _ -> False
                wasm32Parses m = any (\args -> parsesAs (".text\n_start:\n    " <> m <> args <> "\n") Wasm32) ["", " 0", " 0, 0"]
            forM_ f32aMnemonics $ \m -> assertBool ("f32a: " <> toString m) (f32aInstruction m)
            forM_ wasm32Mnemonics $ \m -> assertBool ("wasm32: " <> toString m) (wasm32Parses m)
            assertBool "f32a control" (not $ f32aInstruction "bogus")
            assertBool "wasm32 control" (not $ wasm32Parses "i32.bogus")
        , testCase "Only labels are label references in examples" $
            forM_ [F32a, Wasm32] $ \isa -> do
                let dir = "example" </> toString (isaName isa)
                files <- filter (".s" `isSuffixOf`) <$> listDirectory dir
                forM_ files $ \fn -> do
                    tokens <- concatMap (tokenizeLine isa) . lines . decodeUtf8 <$> readFileBS (dir </> fn)
                    let labels = [T.dropEnd 1 t | (Label, t) <- tokens]
                    forM_ [t | (kind, t) <- tokens, kind `elem` [Ref, Call]] $ \t ->
                        assertBool (dir </> fn <> ": " <> toString t) (t `elem` labels)
        , testCase "report lines" $ do
            significantReport RiscIv "8:\tLw {rd = T0, offsetRs1 = MemRef {mrOffset = 0, mrReg = T0}}\t@_start"
                @?= [ (Separator, "8:")
                    , (Mnemonic, "Lw")
                    , (Register, "T0")
                    , (Mnemonic, "MemRef")
                    , (Literal, "0")
                    , (Register, "T0")
                    , (Label, "@_start")
                    ]
            significantReport RiscIv "mem[0x000..0x003] (4 B): \t48 65 0a ff\t@\"buf\""
                @?= [ (Separator, "mem[0x000..0x003] (4 B):")
                    , (Literal, "48")
                    , (Literal, "65")
                    , (Literal, "0a")
                    , (Literal, "ff")
                    , (Label, "@\"buf\"")
                    ]
            significantReport Wasm32 "0x00c:\toutput_addr" @?= [(Separator, "0x00c:"), (Label, "output_addr")]
            significantReport Wasm32 "0x008:\t_start" @?= [(Separator, "0x008:"), (Label, "_start")]
            significantReport RiscIv "mem:io-ranges:  0x80..0x87 (8 B)"
                @?= [(Ref, "mem:io-ranges:"), (Literal, "0x80"), (Literal, "0x87"), (Literal, "8")]
            significantReport Wasm32 "numio[0x84]: [] >>> [65]" @?= [(Ref, "numio[0x84]:"), (Literal, "65")]
            significantReport Wasm32 "# Check results" @?= [(Directive, "# Check results")]
        , testCase "Report tokens concatenate back"
            $ forM_
                [ "mem[0x000..0x004] (5 B): \tI32Const 132 \t@_start"
                , "128:132"
                , "symio[0x84]: \"\" >>> \"Hello\\n\\0World\""
                , "[unknown-view <T0>] -1 @"
                ]
            $ \l -> T.concat (map snd $ tokenizeReportLine RiscIv l) @?= l
        ]
    where
        significant isa = filter ((/= Plain) . fst) . tokenizeLine isa
        significantReport isa = filter ((/= Plain) . fst) . tokenizeReportLine isa

parseF32a :: Text -> Either (ParseErrorBundle String Void) (F32a.F32aIsa Int32 (Ref Int32))
parseF32a = parse mnemonic "" . toString
