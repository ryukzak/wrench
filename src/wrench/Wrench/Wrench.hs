{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Wrench.Wrench (
    Options (..),
    Result (..),
    prettyLabels,
    runWrenchIO,
    wrench,
    Isa (..),
) where

import Data.Default (Default (..), def)
import Data.Text qualified as T
import Relude
import Relude.Extra
import Text.Pretty.Simple
import Wrench.Config
import Wrench.Isa.Acc32 (Acc32State)
import Wrench.Isa.F32a (F32aState)
import Wrench.Isa.RiscIv (RiscIvState)
import Wrench.Machine
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Report
import Wrench.Translator
import Wrench.Translator.Parser.Types
import Wrench.Translator.Types
import Prelude (Read (..))

data Options = Options
    { input :: FilePath
    , isa :: String
    , configFile :: Maybe FilePath
    , onlyTranslation :: Bool
    , verbose :: Bool
    }
    deriving (Show)

instance Default Options where
    def = Options "" "risc-iv-32" Nothing False False

data Isa = RiscIv | F32a | Acc32
    deriving (Show)

instance Read Isa where
    readsPrec _ "risc-iv-32" = [(RiscIv, "")]
    readsPrec _ "risc-iv" = [(RiscIv, "")]
    readsPrec _ "f32a" = [(F32a, "")]
    readsPrec _ "acc32" = [(Acc32, "")]
    readsPrec _ _ = []

data Result mem w = Result
    { rTrace :: Text
    , rLabels :: HashMap String w
    , rSuccess :: Bool
    , rDump :: mem
    }
    deriving (Show)

-- TODO: Remove hardcoded limits for limits. Get them from CLI.
-- Purpose: limit limits in wrench-serv.

maxLimit :: Int
maxLimit = 8000000

maxMemorySize :: Int
maxMemorySize = 8192

prettyLabels :: (MachineWord w) => HashMap String w -> String
prettyLabels rLabels =
    intercalate "\n"
        $ map (\(l, w) -> show w <> ":\t" <> l)
        $ sortOn snd (toPairs rLabels)

runWrenchIO :: Options -> IO ()
runWrenchIO opts@Options{input, configFile, isa, verbose} = do
    when verbose $ pPrint opts
    conf@Config{cLimit, cMemorySize} <- case configFile of
        Just fn -> either (error . toText) id <$> readConfig fn
        Nothing -> return def

    when verbose $ do
        pPrint conf
        putStrLn "---"
    when (cLimit > maxLimit) $ error "limit too high"
    when (cMemorySize > maxMemorySize) $ error "memory size too high"

    src <- (<> "\n") . decodeUtf8 <$> readFileBS input
    case readMaybe isa of
        Just RiscIv -> wrenchIO @(RiscIvState Int32) conf opts src
        Just F32a -> wrenchIO @(F32aState Int32) conf opts src
        Just Acc32 -> wrenchIO @(Acc32State Int32) conf opts src
        Nothing -> error $ "unknown isa:" <> toText isa

wrenchIO ::
    forall st isa_ w isa1 isa2.
    ( ByteLength isa1
    , ByteLength isa2
    , DerefMnemonic (isa_ w) w
    , InitState (IoMem isa2 w) st
    , Machine st isa2 w
    , MachineWord w
    , MnemonicParser isa1
    , Show (isa_ w w)
    , StateInterspector st (IoMem isa2 w) isa2 w
    , isa1 ~ isa_ w (Ref w)
    , isa2 ~ isa_ w w
    ) =>
    Config
    -> Options
    -> [Char]
    -> IO ()
wrenchIO conf@Config{} opts@Options{isa, onlyTranslation} src =
    case wrench @st conf opts src of
        Right Result{rLabels, rTrace, rSuccess, rDump} -> do
            if onlyTranslation
                then translationResult rLabels rDump
                else do
                    putText rTrace
                    if rSuccess then exitSuccess else exitFailure
        Left e -> wrenchError e
    where
        translationResult rLabels rDump = do
            putStrLn $ prettyLabels rLabels
            putStrLn "---"
            putStrLn $ prettyDump rLabels rDump
        wrenchError e = do
            putStrLn $ "error (" <> isa <> "): " <> toString e
            exitFailure

wrench ::
    forall st isa_ w isa1 isa2.
    ( ByteLength isa1
    , ByteLength isa2
    , DerefMnemonic (isa_ w) w
    , InitState (IoMem isa2 w) st
    , Machine st isa2 w
    , MachineWord w
    , MnemonicParser isa1
    , StateInterspector st (IoMem isa2 w) isa2 w
    , isa1 ~ isa_ w (Ref w)
    , isa2 ~ isa_ w w
    ) =>
    Config
    -> Options
    -> String
    -> Either Text (Result (IntMap (Cell isa2 w)) w)
wrench Config{cMemorySize, cLimit, cInputStreamsFlat, cReports} Options{input = fn, verbose} src = do
    trResult@TranslatorResult{dump, labels} <- translate cMemorySize fn src

    pc <- maybeToRight "_start label should be defined." (labels !? "_start")
    let ioDump =
            IoMem
                { mIoStreams = bimap (map int2mword) (map int2mword) <$> fromMaybe mempty cInputStreamsFlat
                , mIoCells = dump
                }
        st :: st = initState (fromEnum pc) ioDump
        -- TODO: Add config field for stateRecordLimits
        stateRecordLimits = 10000

    traceLog <- powerOn cLimit stateRecordLimits labels st

    let reports = maybe [] (map (prepareReport trResult verbose traceLog)) cReports
        isSuccess = all fst reports
        reportTexts = map snd reports

    return
        $ Result
            { rTrace = unlines $ map (T.strip . ("---\n" <>)) reportTexts
            , rLabels = labels
            , rSuccess = isSuccess
            , rDump = dumpCells dump
            }
    where
        int2mword x
            | fromEnum (minBound :: w) <= x && x <= fromEnum (maxBound :: w) =
                toEnum x
            | fromEnum (minBound :: Unsign w) <= x && x <= fromEnum (maxBound :: Unsign w) =
                toSign $ toEnum x
            | otherwise =
                error $ "integer value out of machine word range: " <> show x
