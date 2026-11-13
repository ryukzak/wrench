module WrenchServ.Config (Config (..), Limits (..), mask, initConfig) where

import Data.List.Split (splitOn)
import Relude
import Relude.Unsafe qualified as Unsafe
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Wrench.Wrench (Limits (..))

data Config = Config
    { cPort :: Int
    , cWrenchPath :: FilePath
    , cWrenchArgs :: [String]
    , cWrenchFmtPath :: FilePath
    , cWrenchFmtArgs :: [String]
    , cStoragePath :: FilePath
    , cVariantsPath :: FilePath
    , cExamplesPath :: FilePath
    , cLogLimit :: Int
    , cVariants :: [String]
    , cScratchLimits :: Limits
    }
    deriving (Show)

initConfig :: IO Config
initConfig = do
    cPort <- maybe 8080 Unsafe.read <$> lookupEnv "PORT"
    (cWrenchPath : cWrenchArgs) <- maybe ["stack", "exec", "wrench", "--"] (splitOn " ") <$> lookupEnv "WRENCH_EXEC"
    (cWrenchFmtPath : cWrenchFmtArgs) <-
        maybe ["stack", "exec", "wrench-fmt", "--"] (splitOn " ") <$> lookupEnv "WRENCH_FMT_EXEC"
    cStoragePath <- fromMaybe "uploads" <$> lookupEnv "STORAGE_PATH"
    cVariantsPath <- fromMaybe "variants" <$> lookupEnv "VARIANTS"
    cExamplesPath <- fromMaybe "examples" <$> lookupEnv "EXAMPLES_PATH"
    cLogLimit <- maybe 10000 Unsafe.read <$> lookupEnv "LOG_LIMIT"
    cVariants <- listVariants cVariantsPath
    lInstruction <- maybe 1000 Unsafe.read <$> lookupEnv "SCRATCH_INSTRUCTION_LIMIT"
    lMemory <- maybe 512 Unsafe.read <$> lookupEnv "SCRATCH_MEMORY_LIMIT"
    lStateLog <- maybe 100 Unsafe.read <$> lookupEnv "SCRATCH_STATE_LOG_LIMIT"
    let cScratchLimits = Limits{lInstruction, lMemory, lStateLog}

    return
        Config
            { cPort
            , cWrenchPath
            , cWrenchArgs
            , cWrenchFmtPath
            , cWrenchFmtArgs
            , cStoragePath
            , cVariantsPath
            , cExamplesPath
            , cLogLimit
            , cVariants
            , cScratchLimits
            }

mask :: Config -> Config
mask conf@Config{} = conf

listVariants :: FilePath -> IO [String]
listVariants path = do
    contents <- listDirectory path
    variants <- filterM (doesDirectoryExist . (path </>)) contents
    return $ sort variants
