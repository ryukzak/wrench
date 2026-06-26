module Wrench.Config (
    Config (..),
    SignalPin (..),
    SpiConfig (..),
    SpiConfs,
    SpiRole (..),
    flattenSpiInputs,
    flattenSpiModes,
    flattenSpiPins,
    readConfig,
) where

import Data.Aeson (FromJSON (..), Value (..), genericParseJSON, withObject, (.:), (.:?))
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Default
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Relude
import Relude.Extra
import Relude.Unsafe qualified as Unsafe
import Wrench.Machine.Types (SpiClockMode, SpiPinsConf (..))
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
    , cSpi :: Maybe SpiConfs
    -- ^ Optional SPI configuration, mapping device id to scheduled input values.
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

type SpiConfs = HashMap String SpiConfig

data SpiConfig = SpiConfig
    { scMode :: SpiClockMode
    , scRole :: SpiRole
    , scCsBit :: SignalPin
    , scClkBit :: SignalPin
    , scMosiBit :: SignalPin
    , scMisoBit :: SignalPin
    , scInput :: [SpiInput]
    }
    deriving (Generic, Show)

instance FromJSON SpiConfig where
    parseJSON = genericParseJSON $ aesonDrop 2 snakeCase

data SpiRole = SpiSlave | SpiMaster
    deriving (Eq, Show)

instance FromJSON SpiRole where
    parseJSON value = do
        role <- parseJSON value
        case (role :: Text) of
            "slave" -> pure SpiSlave
            "master" -> pure SpiMaster
            _ -> fail "invalid spi role, expected slave|master"

data SignalPin = SignalPin
    { spAddress :: Int
    , spBit :: Int
    }
    deriving (Eq, Generic, Show)

instance FromJSON SignalPin where
    parseJSON = genericParseJSON $ aesonDrop 2 snakeCase

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

flattenSpiInputs :: SpiConfs -> IntMap [(Int, Int, Int)]
flattenSpiInputs spi =
    fromList
        $ map (\(addr, cfg@SpiConfig{scInput}) -> (Unsafe.read addr, concatMap (flattenInput cfg) scInput))
        $ toPairs spi
    where
        flattenInput _cfg (SpiByteAt at byte) = [(byte, at, spiByteBits)]
        flattenInput _cfg (SpiBytesAt at bytes) =
            zipWith (\i byte -> (byte, at + i * spiByteBits, spiByteBits)) [0 ..] bytes
        flattenInput _cfg (SpiWordAt at word) = [(word, at, spiWordBits)]

spiByteBits :: Int
spiByteBits = 8

spiWordBits :: Int
spiWordBits = 32

validateSpiConfigs :: (Monad m) => SpiConfs -> ExceptT String m ()
validateSpiConfigs spi = do
    mapM_ validateSpiRole (toPairs spi)
    validateSpiInputs (flattenSpiInputs spi)

validateSpiRole :: (Monad m) => (String, SpiConfig) -> ExceptT String m ()
validateSpiRole (deviceId, SpiConfig{scRole}) =
    case scRole of
        SpiSlave -> return ()
        SpiMaster ->
            throwE
                $ "spi["
                <> deviceId
                <> "]: role=master is not supported yet; use role=slave"

validateSpiInputs :: (Monad m) => IntMap [(Int, Int, Int)] -> ExceptT String m ()
validateSpiInputs spi =
    mapM_ validateDeviceInputs (toPairs spi)

validateDeviceInputs :: (Monad m) => (Int, [(Int, Int, Int)]) -> ExceptT String m ()
validateDeviceInputs (deviceId, inputs) = do
    mapM_ (validateSpiInputValue deviceId) inputs
    case findInputOverlap (sortOn inputTick inputs) of
        Nothing -> return ()
        Just (firstInput, secondInput) ->
            throwE
                $ "spi["
                <> show deviceId
                <> "].input: value at tick "
                <> show (inputTick secondInput)
                <> " overlaps value at tick "
                <> show (inputTick firstInput)

validateSpiInputValue :: (Monad m) => Int -> (Int, Int, Int) -> ExceptT String m ()
validateSpiInputValue deviceId (value, tick, bits)
    | bits == spiByteBits && (value < 0 || value > 255) =
        throwE
            $ "spi["
            <> show deviceId
            <> "].input: byte value at tick "
            <> show tick
            <> " is out of range"
    | otherwise = return ()

findInputOverlap :: [(Int, Int, Int)] -> Maybe ((Int, Int, Int), (Int, Int, Int))
findInputOverlap [] = Nothing
findInputOverlap [_] = Nothing
findInputOverlap (firstInput : secondInput : rest)
    | inputTick secondInput < inputEndTick firstInput = Just (firstInput, secondInput)
    | otherwise = findInputOverlap (secondInput : rest)

inputTick :: (Int, Int, Int) -> Int
inputTick (_, tick, _) = tick

inputEndTick :: (Int, Int, Int) -> Int
inputEndTick (_, tick, bits) = tick + bits

flattenSpiModes :: SpiConfs -> IntMap SpiClockMode
flattenSpiModes spi =
    fromList $ map (\(addr, SpiConfig{scMode}) -> (Unsafe.read addr, scMode)) $ toPairs spi

flattenSpiPins :: SpiConfs -> IntMap SpiPinsConf
flattenSpiPins spi =
    fromList
        $ map
            ( \(addr, SpiConfig{scCsBit, scClkBit, scMosiBit, scMisoBit}) ->
                ( Unsafe.read addr
                , SpiPinsConf
                    { spCsAddr = spAddress scCsBit
                    , spCsBit = spBit scCsBit
                    , spClkAddr = spAddress scClkBit
                    , spClkBit = spBit scClkBit
                    , spMosiAddr = spAddress scMosiBit
                    , spMosiBit = spBit scMosiBit
                    , spMisoAddr = spAddress scMisoBit
                    , spMisoBit = spBit scMisoBit
                    }
                )
            )
            (toPairs spi)
