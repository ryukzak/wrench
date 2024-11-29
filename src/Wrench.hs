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
import Data.Default
import Data.String
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
    readsPrec _ "f32a" = [(F32a, "")]
    readsPrec _ "acc32" = [(Acc32, "")]
    readsPrec _ _ = []

data Result mem w = Result
    { rTrace :: String
    , rLabels :: HashMap String w
    , rSuccess :: Bool
    , rDump :: mem
    }
    deriving (Show)

maxLimit :: Int
maxLimit = 10000

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
    src <- decodeUtf8 <$> readFileBS input

    case readMaybe isa of
        Just RiscIv ->
            case wrench @RiscIv.Isa @RiscIv.Register @Int32 @(RiscIv.MachineState (IoMem (RiscIv.Isa Int32 Int32) Int32) Int32)
                conf
                opts
                src of
                Right Result{rLabels, rTrace, rSuccess, rDump} -> do
                    if onlyTranslation
                        then do
                            putStrLn $ prettyLabels rLabels
                            putStrLn "---"
                            putStrLn $ prettyDump rLabels rDump
                        else do
                            putStrLn rTrace
                            if rSuccess then exitSuccess else exitFailure
                Left e -> do
                    putStrLn $ "error: " <> toString e
                    exitFailure
        Just F32a ->
            case wrench @F32a.Isa @F32a.Register @Int32 @(F32a.MachineState (IoMem (F32a.Isa Int32 Int32) Int32) Int32) conf opts src of
                Right Result{rLabels, rTrace, rSuccess, rDump} -> do
                    if onlyTranslation
                        then do
                            putStrLn $ prettyLabels rLabels
                            putStrLn "---"
                            putStrLn $ prettyDump rLabels rDump
                        else do
                            putStrLn rTrace
                            if rSuccess then exitSuccess else exitFailure
                Left e -> do
                    putStrLn $ "error: " <> toString e
                    exitFailure
        Just Acc32 ->
            case wrench @Acc32.Isa @Acc32.Register @Int32 @(Acc32.MachineState (IoMem (Acc32.Isa Int32 Int32) Int32) Int32) conf opts src of
                Right Result{rLabels, rTrace, rSuccess, rDump} -> do
                    if onlyTranslation
                        then do
                            putStrLn $ prettyLabels rLabels
                            putStrLn "---"
                            putStrLn $ prettyDump rLabels rDump
                        else do
                            putStrLn rTrace
                            if rSuccess then exitSuccess else exitFailure
                Left e -> do
                    putStrLn $ "error: " <> toString e
                    exitFailure
        Nothing -> error $ "unknown isa:" <> toText isa

wrench ::
    forall isa_ r w st isa1 isa2.
    ( isa1 ~ isa_ w (Ref w)
    , isa2 ~ isa_ w w
    , InitState (IoMem isa2 w) st
    , MnemonicParser isa1
    , StateInterspector st isa2 w r
    , ViewState st
    , DerefMnemonic (isa_ w) w
    , Show isa2
    , Hashable r
    , Read r
    , ByteLength isa1
    , ByteLength isa2
    , MachineWord w
    , Machine st isa2 w
    ) =>
    Config
    -> Options
    -> String
    -> Either Text (Result (IntMap (Cell isa2 w)) w)
wrench Config{cMemorySize, cLimit, cInputStreamsFlat, cReports} Options{input = fn, verbose} src = do
    TranslatorResult{dump, labels} <- translate (Just cMemorySize) fn src

    pc <- maybeToRight "_start label should be defined." (labels !? "_start")
    let ioDump =
            IoMem
                { mIoStreams = bimap (map toEnum) (map toEnum) <$> fromMaybe mempty cInputStreamsFlat
                , mIoCells = dump
                }
        st = initState (fromEnum pc) ioDump

    (traceLog :: [Trace st isa2]) <- powerOn cLimit labels st

    let reports = maybe [] (map (prepareReport verbose traceLog)) cReports
        isSuccess = all fst reports
    return
        $ Result
            { rTrace = intercalate "\n---\n" $ map snd reports
            , rLabels = labels
            , rSuccess = isSuccess
            , rDump = dump
            }
