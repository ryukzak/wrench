module Wrench.Config (
    Config (..),
    SpiClockModeConf (..),
    SpiPortBitConf (..),
    SpiPinsConfFlat (..),
    readConfig,
) where

import Data.Aeson (FromJSON (..), Value (..), genericParseJSON, withObject, (.:), (.:?))
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Default
import Data.Text qualified as T
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Relude
import Relude.Extra
import Relude.Unsafe qualified as Unsafe
import Wrench.Report

throwE :: (Monad m) => e -> ExceptT e m a
throwE = ExceptT . return . Left

readConfig :: FilePath -> IO (Either String Config)
readConfig path = runExceptT $ do
    result <- liftIO $ decodeFileEither path
    conf@Config{cMemoryMappedIo, cSpi} <- case result of
        Left e -> throwE $ prettyPrintParseException e
        Right conf -> return conf
    let conf' =
            (conf <> def)
                { cMemoryMappedIoFlat = fmap flattenIoStream cMemoryMappedIo
                , cSpiFlat = fmap flattenSpiInputs cSpi
                , cSpiModeFlat = fmap flattenSpiModes cSpi
                , cSpiPinsFlat = fmap flattenSpiPins cSpi
                }
    return conf'

-----------------------------------------------------------

data Config = Config
    { cLimit :: Int
    -- ^ The maximum number of instructions to execute.
    , cMemorySize :: Int
    -- ^ The size of the memory in bytes.
    , cMemoryMappedIo :: Maybe (HashMap String [Input])
    -- ^ Optional memory-mapped IO configuration, mapping stream address (decimal or hex format) to lists of inputs.
    , cMemoryMappedIoFlat :: Maybe (IntMap ([Int], [Int]))
    -- ^ (generated) Flattened memory-mapped IO configuration, mapping addresses to pairs of input and output lists.
    , cSpi :: Maybe (HashMap String SpiConfig)
    -- ^ Optional SPI configuration, mapping device base address to scheduled input values.
    , cSpiFlat :: Maybe (IntMap [(Int, Int)])
    -- ^ (generated) Flattened SPI configuration, mapping device base address to (value, tick) pairs.
    , cSpiModeFlat :: Maybe (IntMap SpiClockModeConf)
    -- ^ (generated) Flattened SPI mode number per device.
    , cSpiPinsFlat :: Maybe (IntMap SpiPinsConfFlat)
    -- ^ (generated) Flattened SPI pin/register layout per device.
    , cReports :: Maybe [ReportConf]
    -- ^ Optional list of report configurations.
    , cSeed :: Maybe Int
    -- ^ Optional seed for random number generation.
    }
    deriving (Generic, Show)

instance Default Config where
    def =
        Config
            { cLimit = 1000
            , cMemorySize = 512
            , cMemoryMappedIo = Nothing
            , cMemoryMappedIoFlat = Nothing
            , cSpi = Nothing
            , cSpiFlat = Nothing
            , cSpiModeFlat = Nothing
            , cSpiPinsFlat = Nothing
            , cReports =
                Just
                    [ ReportConf
                        { rcName = Just "Executed Instruction Log"
                        , rcSlice = AllSlice
                        , rcAssert = Nothing
                        , rcView = Just "{pc}: {instruction} {pc:label}\n"
                        }
                    ]
            , cSeed = Nothing
            }

instance Semigroup Config where
    a <> b =
        Config
            { cMemorySize = cMemorySize a
            , cMemoryMappedIo = cMemoryMappedIo a <|> cMemoryMappedIo b
            , cMemoryMappedIoFlat = cMemoryMappedIoFlat a <|> cMemoryMappedIoFlat b
            , cSpi = cSpi a <|> cSpi b
            , cSpiFlat = cSpiFlat a <|> cSpiFlat b
            , cSpiModeFlat = cSpiModeFlat a <|> cSpiModeFlat b
            , cSpiPinsFlat = cSpiPinsFlat a <|> cSpiPinsFlat b
            , cLimit = cLimit a
            , cReports = cReports a <|> cReports b
            , cSeed = cSeed a <|> cSeed b
            }

instance FromJSON Config where
    parseJSON = genericParseJSON $ aesonDrop 1 snakeCase

-----------------------------------------------------------

data Input = Num Int | Chars [Int] String
    deriving (Show)

instance FromJSON Input where
    parseJSON (String t) = return $ Chars (map ord $ toString t) (toString t)
    parseJSON (Number n) = return $ Num (round n) -- Int case
    parseJSON _ = fail "Expected a Char, String, or Int"

flattenIoStream :: HashMap String [Input] -> IntMap ([Int], [Int])
flattenIoStream memory_mapped_io =
    fromList $ map (\(addr, is) -> (Unsafe.read addr, (flatInputs is, []))) $ toPairs memory_mapped_io
    where
        flatInputs = concatMap (\case Num n -> [n]; Chars ns _ -> ns)

data SpiConfig = SpiConfig
    { scMode :: SpiClockModeConf
    , scCsBit :: Maybe SpiPortBitConf
    , scClkBit :: Maybe SpiPortBitConf
    , scMosiBit :: Maybe SpiPortBitConf
    , scMisoBit :: Maybe SpiPortBitConf
    , scInput :: [SpiInput]
    }
    deriving (Generic, Show)

instance FromJSON SpiConfig where
    parseJSON = genericParseJSON $ aesonDrop 2 snakeCase

data SpiClockModeConf = SpiCfgMode0 | SpiCfgMode1 | SpiCfgMode2 | SpiCfgMode3
    deriving (Eq, Show)

instance FromJSON SpiClockModeConf where
    parseJSON value = do
        mode <- parseJSON value
        case (mode :: Int) of
            0 -> pure SpiCfgMode0
            1 -> pure SpiCfgMode1
            2 -> pure SpiCfgMode2
            3 -> pure SpiCfgMode3
            _ -> fail "invalid spi mode, expected 0|1|2|3"

data SpiPinsConfFlat = SpiPinsConfFlat
    { spfCsBit :: Maybe SpiPortBitConf
    , spfClkBit :: Maybe SpiPortBitConf
    , spfMosiBit :: Maybe SpiPortBitConf
    , spfMisoBit :: Maybe SpiPortBitConf
    }
    deriving (Eq, Generic, Show)

instance FromJSON SpiPinsConfFlat where
    parseJSON = genericParseJSON $ aesonDrop 3 snakeCase

data SpiPortBitConf = SpiPortBitConf
    { spbcAddr :: Int
    , spbcBit :: Int
    }
    deriving (Eq, Show)

instance FromJSON SpiPortBitConf where
    parseJSON (String t) =
        let chunks = T.splitOn ":" t
         in case chunks of
                [addrRaw, bitRaw] ->
                    case (readMaybe (toString addrRaw), readMaybe (toString bitRaw)) of
                        (Just addr, Just bit) -> pure SpiPortBitConf{spbcAddr = addr, spbcBit = bit}
                        _ -> fail "invalid spi pin mapping, expected <addr>:<bit>"
                _ -> fail "invalid spi pin mapping, expected <addr>:<bit>"
    parseJSON _ = fail "invalid spi pin mapping, expected string <addr>:<bit>"

data SpiInput
    = SpiByteAt Int Int
    | SpiBytesAt Int [Int]
    | SpiWordAt Int Int
    deriving (Show)

instance FromJSON SpiInput where
    parseJSON = withObject "SpiInput" $ \obj -> do
        at <- obj .: "at"
        byte <- obj .:? "byte"
        bytes <- obj .:? "bytes"
        word <- obj .:? "word"
        case catMaybes [SpiByteAt at <$> byte, SpiBytesAt at <$> bytes, SpiWordAt at <$> word] of
            [entry] -> pure entry
            [] -> fail "spi.input entry must define exactly one of: byte|bytes|word"
            _ -> fail "spi.input entry has multiple payload fields; use only one of: byte|bytes|word"

flattenSpiInputs :: HashMap String SpiConfig -> IntMap [(Int, Int)]
flattenSpiInputs spi =
    fromList
        $ map (\(addr, cfg@SpiConfig{scInput}) -> (Unsafe.read addr, concatMap (flattenInput cfg) scInput))
        $ toPairs spi
    where
        flattenInput _cfg (SpiByteAt at byte) = [(byte, at)]
        flattenInput _cfg (SpiBytesAt at bytes) = zip bytes [at ..]
        flattenInput _cfg (SpiWordAt at word) = [(word, at)]

flattenSpiModes :: HashMap String SpiConfig -> IntMap SpiClockModeConf
flattenSpiModes spi =
    fromList $ map (\(addr, SpiConfig{scMode}) -> (Unsafe.read addr, scMode)) $ toPairs spi

flattenSpiPins :: HashMap String SpiConfig -> IntMap SpiPinsConfFlat
flattenSpiPins spi =
    fromList
        $ map
            ( \(addr, SpiConfig{scCsBit, scClkBit, scMosiBit, scMisoBit}) ->
                ( Unsafe.read addr
                , SpiPinsConfFlat
                    { spfCsBit = scCsBit
                    , spfClkBit = scClkBit
                    , spfMosiBit = scMosiBit
                    , spfMisoBit = scMisoBit
                    }
                )
            )
            (toPairs spi)
