module WrenchServ.Config (Config (..), mask, initConfig) where

import Data.List.Split (splitOn)
import Relude
import Relude.Unsafe qualified as Unsafe
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

data Config = Config
    { cPort :: Int
    , cWrenchPath :: FilePath
    , cWrenchArgs :: [String]
    , cStoragePath :: FilePath
    , cVariantsPath :: FilePath
    , cLogLimit :: Int
    , cVariants :: [String]
    }
    deriving (Show)

initConfig :: IO Config
initConfig = do
    cPort <- maybe 8080 Unsafe.read <$> lookupEnv "PORT"
    (cWrenchPath : cWrenchArgs) <- maybe ["stack", "exec", "wrench", "--"] (splitOn " ") <$> lookupEnv "WRENCH_EXEC"
    cStoragePath <- fromMaybe "uploads" <$> lookupEnv "STORAGE_PATH"
    cVariantsPath <- fromMaybe "variants" <$> lookupEnv "VARIANTS"
    cLogLimit <- maybe 10000 Unsafe.read <$> lookupEnv "LOG_LIMIT"
    cVariants <- listVariants cVariantsPath

    return
        Config
            { cPort
            , cWrenchPath
            , cWrenchArgs
            , cStoragePath
            , cVariantsPath
            , cLogLimit
            , cVariants
            }

mask :: Config -> Config
mask conf@Config{} = conf

listVariants :: FilePath -> IO [String]
listVariants path = do
    contents <- listDirectory path
    variants <- filterM (doesDirectoryExist . (path </>)) contents
    return $ sort variants
