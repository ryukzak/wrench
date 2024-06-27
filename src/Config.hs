module Config (
    Config (..),
    readConfig,
    prettyConfig,
)
where

import Data.Aeson (FromJSON (..), Value (..), genericParseJSON)
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Default
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Relude
import Relude.Extra
import Relude.Unsafe qualified as Unsafe
import Report

throwE :: (Monad m) => e -> ExceptT e m a
throwE = ExceptT . return . Left

readConfig :: FilePath -> IO (Either String Config)
readConfig path = runExceptT $ do
    result <- liftIO $ decodeFileEither path
    conf@Config{cInputStreams} <- case result of
        Left e -> throwE $ prettyPrintParseException e
        Right conf -> return conf
    let conf' = (conf <> def){cInputStreamsFlat = fmap flattenIoStream cInputStreams}
    return conf'

-----------------------------------------------------------

data Config = Config
    { cLimit :: Int
    , cMemorySize :: Int
    , cInputStreams :: Maybe (HashMap String [Input])
    , cInputStreamsFlat :: Maybe (IntMap ([Int], [Int]))
    , cReports :: Maybe [ReportConf]
    }
    deriving (Show, Generic)

prettyConfig :: Config -> String
prettyConfig Config{cLimit, cInputStreamsFlat, cReports} =
    intercalate
        "\n"
        [ "Config"
        , "    cLimit = " <> show cLimit
        , "    cInputStreamsFlat:" <> case cInputStreamsFlat of
            Nothing -> "Nothing"
            Just io -> "\n" <> intercalate "\n" (map (\(addr, (is, os)) -> "        - " <> show addr <> ": " <> show is <> " >>> " <> show os) $ toPairs io)
        , "    cReports = " <> case cReports of
            Nothing -> "Nothing"
            Just rs -> "\n" <> intercalate "\n" (map (\r -> "        - " <> prettyReportConf r) rs)
        , "    }"
        ]

prettyReportConf :: ReportConf -> String
prettyReportConf ReportConf{rcSlice, rcFilter, rcInspector, rcAssert} =
    intercalate "\n" $ ("slice = " <> show rcSlice)
        : map
            ("          " <>)
            [ "filter = " <> show rcFilter
            , "statePrinter = " <> show rcInspector
            , "assert = " <> show rcAssert
            ]

instance Default Config where
    def =
        Config
            { cLimit = 1000
            , cMemorySize = 512
            , cInputStreams = Nothing
            , cInputStreamsFlat = Nothing
            , cReports =
                Just
                    [ ReportConf
                        { rcName = Just "InstructionLog"
                        , rcSlice = AllSlice
                        , rcFilter = [IsInstruction]
                        , rcInspector = Nothing
                        , rcAssert = Nothing
                        }
                    ]
            }

instance Semigroup Config where
    a <> b =
        Config
            { cMemorySize = cMemorySize a
            , cInputStreams = cInputStreams a <|> cInputStreams b
            , cInputStreamsFlat = cInputStreamsFlat a <|> cInputStreamsFlat b
            , cLimit = cLimit a
            , cReports = cReports a <|> cReports b
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
flattenIoStream input_streams =
    fromList $ map (\(addr, is) -> (Unsafe.read addr, (flatInputs is, []))) $ toPairs input_streams
    where
        flatInputs = concatMap (\case Num n -> [n]; Chars ns _ -> ns)
