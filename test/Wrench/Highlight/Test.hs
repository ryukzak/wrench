module Wrench.Highlight.Test (tests) where

import Data.List (isSuffixOf)
import Data.Text qualified as T
import Relude
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, (@?=))
import Wrench.Highlight
import Wrench.Isa (Isa (..), allIsa, isaName)

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
        , testCase "wasm32" $
            significant Wasm32 "i32.const factorial ; call"
                @?= [(Mnemonic, "i32.const"), (Ref, "factorial"), (Comment, "; call")]
        , testCase "f32a" $ do
            significant F32a "while: -1 + ; \\ loop"
                @?= [(Label, "while:"), (Literal, "-1"), (Mnemonic, "+"), (Mnemonic, ";"), (Comment, "\\ loop")]
            significant F32a "buf: .byte 'a\\n'"
                @?= [(Label, "buf:"), (Directive, ".byte"), (Literal, "'a\\n'")]
        ]
    where
        significant isa = filter ((/= Plain) . fst) . tokenizeLine isa
