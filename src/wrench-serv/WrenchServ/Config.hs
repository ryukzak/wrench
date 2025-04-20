module WrenchServ.Config (Config (..), mask, initConfig) where

import Data.List.Split (splitOn)
import Relude
import Relude.Unsafe qualified as Unsafe

data Config = Config
    { cPort :: Int
    , cWrenchPath :: FilePath
    , cWrenchArgs :: [String]
    , cStoragePath :: FilePath
    , cVariantsPath :: FilePath
    , cLogLimit :: Int
    , cMixpanelToken :: Maybe Text
    , cMixpanelProjectId :: Maybe Text
    }
    deriving (Show)

initConfig :: IO Config
initConfig = do
    cPort <- maybe 8080 Unsafe.read <$> lookupEnv "PORT"
    (cWrenchPath : cWrenchArgs) <- maybe ["stack", "exec", "wrench", "--"] (splitOn " ") <$> lookupEnv "WRENCH_EXEC"
    cStoragePath <- fromMaybe "uploads" <$> lookupEnv "STORAGE_PATH"
    cVariantsPath <- fromMaybe "variants" <$> lookupEnv "VARIANTS"
    cLogLimit <- maybe 10000 Unsafe.read <$> lookupEnv "LOG_LIMIT"
    cMixpanelToken <- fmap toText <$> lookupEnv "MIXPANEL_TOKEN"
    cMixpanelProjectId <- fmap toText <$> lookupEnv "MIXPANEL_PROJECT_ID"

    unless
        ( (isJust cMixpanelToken && isJust cMixpanelProjectId)
            || (isNothing cMixpanelToken && isNothing cMixpanelProjectId)
        )
        $ error "Mixpanel misconfiguration"

    return
        Config
            { cPort
            , cWrenchPath
            , cWrenchArgs
            , cStoragePath
            , cVariantsPath
            , cLogLimit
            , cMixpanelToken
            , cMixpanelProjectId
            }

mask :: Config -> Config
mask conf@Config{cMixpanelToken} =
    conf
        { cMixpanelToken = fmap (const "<REDACTED>") cMixpanelToken
        }
