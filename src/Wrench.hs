{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Wrench (
    Options (..),
    Result (..),
    prettyLabels,
    wrenchIO,
    wrench,
    Isa (..),
) where

import Config
import Data.Default (Default (..), def)
import Data.Text qualified as T
import Isa.Acc32 qualified as Acc32
import Isa.F32a qualified as F32a
import Isa.RiscIv qualified as RiscIv
import Machine
import Machine.Memory
import Machine.Types
import Relude
import Relude.Extra
import Report
import Text.Pretty.Simple
import Translator
import Translator.Parser.Types
import Translator.Types
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

maxLimit :: Int
maxLimit = 600000

maxMemorySize :: Int
maxMemorySize = 8192

prettyLabels :: (MachineWord w) => HashMap String w -> String
prettyLabels rLabels =
    intercalate "\n"
        $ map (\(l, w) -> show w <> ":\t" <> l)
        $ sortOn snd (toPairs rLabels)

wrenchIO :: Options -> IO ()
wrenchIO opts@Options{input, configFile, isa, onlyTranslation, verbose} = do
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
        Just RiscIv ->
            case wrench @RiscIv.Isa @Int32 @(RiscIv.MachineState (IoMem (RiscIv.Isa Int32 Int32) Int32) Int32)
                conf
                opts
                src of
                Right Result{rLabels, rTrace, rSuccess, rDump} -> do
                    if onlyTranslation
                        then translationResult rLabels rDump
                        else do
                            putText rTrace
                            if rSuccess then exitSuccess else exitFailure
                Left e -> wrenchError e
        Just F32a ->
            case wrench @F32a.Isa @Int32 @(F32a.MachineState (IoMem (F32a.Isa Int32 Int32) Int32) Int32) conf opts src of
                Right Result{rLabels, rTrace, rSuccess, rDump} -> do
                    if onlyTranslation
                        then translationResult rLabels rDump
                        else do
                            putText rTrace
                            if rSuccess then exitSuccess else exitFailure
                Left e -> wrenchError e
        Just Acc32 ->
            case wrench @Acc32.Isa @Int32 @(Acc32.MachineState (IoMem (Acc32.Isa Int32 Int32) Int32) Int32) conf opts src of
                Right Result{rLabels, rTrace, rSuccess, rDump} -> do
                    if onlyTranslation
                        then translationResult rLabels rDump
                        else do
                            putText rTrace
                            if rSuccess then exitSuccess else exitFailure
                Left e -> wrenchError e
        Nothing -> error $ "unknown isa:" <> toText isa
    where
        translationResult rLabels rDump = do
            putStrLn $ prettyLabels rLabels
            putStrLn "---"
            putStrLn $ prettyDump rLabels rDump
        wrenchError e = do
            putStrLn $ "error (" <> isa <> "): " <> toString e
            exitFailure

wrench ::
    forall isa_ w st isa1 isa2.
    ( ByteLength isa1
    , ByteLength isa2
    , DerefMnemonic (isa_ w) w
    , InitState (IoMem isa2 w) st
    , Machine st isa2 w
    , MachineWord w
    , MnemonicParser isa1
    , StateInterspector st isa2 w
    , isa1 ~ isa_ w (Ref w)
    , isa2 ~ isa_ w w
    ) =>
    Config
    -> Options
    -> String
    -> Either Text (Result (IntMap (Cell isa2 w)) w)
wrench Config{cMemorySize, cLimit, cInputStreamsFlat, cReports} Options{input = fn, verbose} src = do
    trResult@TranslatorResult{dump, labels} <- translate (Just cMemorySize) fn src

    pc <- maybeToRight "_start label should be defined." (labels !? "_start")
    let ioDump =
            IoMem
                { mIoStreams = bimap (map int2mword) (map int2mword) <$> fromMaybe mempty cInputStreamsFlat
                , mIoCells = dump
                }
        st = initState (fromEnum pc) ioDump

    (traceLog :: [Trace st isa2]) <- powerOn cLimit labels st

    let reports = maybe [] (map (prepareReport trResult verbose traceLog)) cReports
        isSuccess = all fst reports
        reportTexts = map snd reports <> map ("ERROR: " <>) (getErrors traceLog)

    return
        $ Result
            { rTrace = unlines $ map (T.strip . ("---\n" <>)) reportTexts
            , rLabels = labels
            , rSuccess = isSuccess
            , rDump = dump
            }
    where
        int2mword x
            | fromEnum (minBound :: w) <= x && x <= fromEnum (maxBound :: w) =
                toEnum x
            | fromEnum (minBound :: Unsign w) <= x && x <= fromEnum (maxBound :: Unsign w) =
                toSign $ toEnum x
            | otherwise =
                error $ "integer value out of machine word range: " <> show x
