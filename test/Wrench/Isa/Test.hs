module Wrench.Isa.Test (tests) where

import Data.Text qualified as T
import Relude
import System.FilePath (takeDirectory, takeFileName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension)
import Test.Tasty.HUnit
import Wrench.Isa

tests :: TestTree
tests =
    testGroup
        "Isa detection"
        [ testCase "Every example and golden source is detected from its text alone" $ do
            files <- sort . concat <$> mapM (findByExtension [".s"]) ["example", "test/golden"]
            assertBool "no sources found to detect" $ not (null files)
            failures <- catMaybes <$> mapM mismatch files
            unless (null failures) $ assertFailure $ toString $ T.intercalate "\n" failures
        , testGroup
            "Sources no single architecture owns"
            [ undetectable
                "A bare halt parses under every architecture but VLIW-IV"
                [ "    .text"
                , "_start:"
                , "    halt"
                ]
            , undetectable
                "A mnemonic that does not exist parses under none"
                [ "    .text"
                , "_start:"
                , "    addi t0, t0, 1"
                , "    frobnicate t0"
                , "    halt"
                ]
            ]
        , testGroup
            "Architectures that look alike"
            [ detects
                "RISC-IV and VLIW-IV share their whole vocabulary; the slots tell them apart"
                RiscIv
                [ "    .text"
                , "_start:"
                , "    addi t0, t0, 1"
                , "    halt"
                ]
            , detects
                "The same instruction in slots is VLIW-IV"
                VliwIv
                [ "    .text"
                , "_start:"
                , "    addi t0, t0, 1 / nop / nop / nop"
                , "    addi t0, t0, 1 / nop / nop / halt"
                ]
            , detects
                "The .l size and the capitalised registers are M68k"
                M68k
                [ "    .text"
                , "_start:"
                , "    move.l   (A6), D0"
                , "    add.l    D1, D0"
                , "    halt"
                ]
            ]
        , detects
            "An unresolved label does not cost the source its architecture"
            RiscIv
            [ "    .text"
            , "_start:"
            , "    addi t0, t0, 1"
            , "    j nowhere"
            , "    halt"
            ]
        ]

detects :: String -> Isa -> [Text] -> TestTree
detects what expected src =
    testCase what $ case detectIsa (unlines src) of
        Right isa -> assertEqual "detected isa" (isaName expected) (isaName isa)
        Left reason -> assertFailure $ "not detected: " <> toString reason

undetectable :: String -> [Text] -> TestTree
undetectable what src =
    testCase what $ case detectIsa (unlines src) of
        Left _ -> pass
        Right isa -> assertFailure $ "unexpectedly detected " <> toString (isaName isa)

-- | The detection failure a sample file causes, if any.
mismatch :: FilePath -> IO (Maybe Text)
mismatch fn = do
    src <- decodeUtf8 <$> readFileBS fn
    return $ case (expectedIsa fn, detectIsa src) of
        (Nothing, Left _) -> Nothing
        (Nothing, Right isa) -> complain $ "expected no detection, got " <> isaName isa
        (Just expected, Right isa)
            | isa == expected -> Nothing
            | otherwise -> complain $ "expected " <> isaName expected <> ", got " <> isaName isa
        (Just _, Left reason) -> complain reason
    where
        complain what = Just $ toText fn <> ": " <> what

-- | The ISA a sample file is filed under, or 'Nothing' for the ambiguous ones.
expectedIsa :: FilePath -> Maybe Isa
expectedIsa fn
    | fn `elem` ambiguous = Nothing
    | otherwise = readMaybe $ takeFileName $ takeDirectory fn

-- | Sample programs more than one architecture can parse.
ambiguous :: [FilePath]
ambiguous =
    [ "example/risc-iv-32/random-memory-init.s"
    , "test/golden/acc32/dup.s"
    , "test/golden/acc32/label_like_instr.s"
    , "test/golden/risc-iv-32/memory_init.s"
    ]
