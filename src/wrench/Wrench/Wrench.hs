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
import Wrench.Isa.M68k (M68kState)
import Wrench.Isa.RiscIv (RiscIvState)
import Wrench.Isa.VliwIv (VliwIvState)
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
    , maxInstructionLimit :: Int
    , maxMemoryLimit :: Int
    , maxStateLogLimit :: Int
    }
    deriving (Show)

instance Default Options where
    def =
        Options
            { input = ""
            , isa = "risc-iv-32"
            , configFile = Nothing
            , onlyTranslation = False
            , verbose = False
            , maxInstructionLimit = 8000000
            , maxMemoryLimit = 8192
            , maxStateLogLimit = 10000
            }

data Isa = VliwIv | RiscIv | F32a | Acc32 | M68k
    deriving (Show)

instance Read Isa where
    readsPrec _ "vliw-iv" = [(VliwIv, "")]
    readsPrec _ "risc-iv-32" = [(RiscIv, "")]
    readsPrec _ "risc-iv" = [(RiscIv, "")]
    readsPrec _ "f32a" = [(F32a, "")]
    readsPrec _ "acc32" = [(Acc32, "")]
    readsPrec _ "m68k" = [(M68k, "")]
    readsPrec _ _ = []

data Result mem w = Result
    { rTrace :: Text
    , rLabels :: HashMap String w
    , rSuccess :: Bool
    , rDump :: mem
    }
    deriving (Show)

prettyLabels :: (MachineWord w) => HashMap String w -> String
prettyLabels rLabels =
    intercalate "\n"
        $ map (\(l, w) -> show w <> ":\t" <> l)
        $ sortOn snd (toPairs rLabels)

runWrenchIO :: Options -> IO ()
runWrenchIO opts@Options{input, configFile, isa, verbose, maxInstructionLimit, maxMemoryLimit} = do
    when verbose $ pPrint opts
    conf@Config{cLimit, cMemorySize} <- case configFile of
        Just fn -> either (error . toText) id <$> readConfig fn
        Nothing -> return def

    when verbose $ do
        pPrint conf
        putStrLn "---"
    when (cLimit > maxInstructionLimit) $ error "limit too high"
    when (cMemorySize > maxMemoryLimit) $ error "memory size too high"

    src <- (<> "\n") . decodeUtf8 <$> readFileBS input
    case readMaybe isa of
        Just RiscIv -> wrenchIO @(RiscIvState Int32) opts conf src
        Just VliwIv -> wrenchIO @(VliwIvState Int32) opts conf src
        Just F32a -> wrenchIO @(F32aState Int32) opts conf src
        Just Acc32 -> wrenchIO @(Acc32State Int32) opts conf src
        Just M68k -> wrenchIO @(M68kState Int32) opts conf src
        Nothing -> error $ "unknown isa:" <> toText isa

wrenchIO ::
    forall st isa_ w isa1 isa2.
    ( ByteSize isa1
    , ByteSize isa2
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
    Options
    -> Config
    -> [Char]
    -> IO ()
wrenchIO opts@Options{isa, onlyTranslation} conf@Config{} src =
    case wrench @st opts conf src of
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
    ( ByteSize isa1
    , ByteSize isa2
    , DerefMnemonic (isa_ w) w
    , InitState (IoMem isa2 w) st
    , Machine st isa2 w
    , MachineWord w
    , MnemonicParser isa1
    , StateInterspector st (IoMem isa2 w) isa2 w
    , isa1 ~ isa_ w (Ref w)
    , isa2 ~ isa_ w w
    ) =>
    Options
    -> Config
    -> String
    -> Either Text (Result (IntMap (Cell isa2 w)) w)
wrench Options{input = fn, verbose, maxStateLogLimit} Config{cMemorySize, cLimit, cMemoryMappedIoFlat, cReports, cSeed} src = do
    trResult@TranslatorResult{dump, labels} <- translate cMemorySize fn src

    pc <- maybeToRight "_start label should be defined." (labels !? "_start")
    let mIoStreams = bimap (map int2mword) (map int2mword) <$> fromMaybe mempty cMemoryMappedIoFlat
        ioDump = mkIoMem mIoStreams dump cSeed
        st :: st = initState (fromEnum pc) ioDump

    traceLog <- powerOn cLimit maxStateLogLimit labels st

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
