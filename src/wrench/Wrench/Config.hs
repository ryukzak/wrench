module Wrench.Config (
    Config (..),
    SpiModeConf (..),
    readConfig,
) where

import Data.Aeson (FromJSON (..), Value (..), genericParseJSON)
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Default
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
    maybe (return ()) validateSpiConfigs cSpi
    let conf' =
            (conf <> def)
                { cMemoryMappedIoFlat = fmap flattenIoStream cMemoryMappedIo
                , cSpiFlat = fmap (fst . flattenSpiWithDiv) cSpi
                , cSpiClkDiv = fmap (snd . flattenSpiWithDiv) cSpi
                , cSpiModeFlat = fmap flattenSpiModes cSpi
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
    , cSpiClkDiv :: Maybe (IntMap Int)
    -- ^ (generated) Flattened SPI clock divisors, mapping device base address to clk_div values.
    , cSpiModeFlat :: Maybe (IntMap SpiModeConf)
    -- ^ (generated) Flattened SPI modes, mapping device base address to mode.
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
            , cSpiClkDiv = Nothing
            , cSpiModeFlat = Nothing
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
            , cSpiClkDiv = cSpiClkDiv a <|> cSpiClkDiv b
            , cSpiModeFlat = cSpiModeFlat a <|> cSpiModeFlat b
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
    { scInput :: [SpiInput]
    , scClkDiv :: Maybe Int
    , scMode :: Maybe SpiModeConf
    }
    deriving (Generic, Show)

instance FromJSON SpiConfig where
    parseJSON = genericParseJSON $ aesonDrop 2 snakeCase

data SpiModeConf = SpiModeHardware | SpiModeSoftware
    deriving (Eq, Show)

instance FromJSON SpiModeConf where
    parseJSON (String "hardware") = return SpiModeHardware
    parseJSON (String "software") = return SpiModeSoftware
    parseJSON _ = fail "invalid spi mode, expect: hardware|software"

data SpiInput = SpiInput Input Int
    deriving (Show)

instance FromJSON SpiInput where
    parseJSON value = do
        ((input, tick) :: (Input, Int)) <- parseJSON value
        return $ SpiInput input tick

flattenSpiWithDiv :: HashMap String SpiConfig -> (IntMap [(Int, Int)], IntMap Int)
flattenSpiWithDiv spi =
    let spiData =
            fromList
                $ map (\(addr, cfg@SpiConfig{scInput}) -> (Unsafe.read addr, concatMap (flattenInput cfg) scInput))
                $ toPairs spi
        spiDiv = fromList $ map (\(addr, SpiConfig{scClkDiv}) -> (Unsafe.read addr, fromMaybe 1 scClkDiv)) $ toPairs spi
     in (spiData, spiDiv)
    where
        flattenInput _cfg (SpiInput (Num n) tick) = [(n, tick)]
        flattenInput _cfg (SpiInput (Chars ns _) tick) = zip ns [tick ..]

flattenSpiModes :: HashMap String SpiConfig -> IntMap SpiModeConf
flattenSpiModes spi =
    fromList $ map (\(addr, SpiConfig{scMode}) -> (Unsafe.read addr, fromMaybe SpiModeSoftware scMode)) $ toPairs spi

validateSpiConfigs :: (Monad m) => HashMap String SpiConfig -> ExceptT String m ()
validateSpiConfigs spi =
    forM_ (toPairs spi) $ \(addr, SpiConfig{scMode, scClkDiv}) ->
        case (fromMaybe SpiModeSoftware scMode, scClkDiv) of
            (SpiModeSoftware, Just _) ->
                throwE $ "spi[" <> addr <> "]: clk_div is allowed only for mode=hardware"
            _ ->
                return ()
