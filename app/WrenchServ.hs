{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.List.Split
import Data.Text (isSuffixOf, replace)
import Data.Time
import Data.UUID.V4 (nextRandom)
import Lucid (Html, toHtmlRaw)
import Network.Wai.Handler.Warp (run)
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant
import Servant.HTML.Lucid
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeFileName, (</>))
import System.Process (readProcessWithExitCode)
import Web.FormUrlEncoded (FromForm)

type API =
    "submit" :> ReqBody '[FormUrlEncoded] SubmitForm :> Post '[JSON] (Headers '[Header "Location" String] NoContent)
        :<|> "submit-form" :> Get '[HTML] (Html ())
        :<|> "result" :> Capture "guid" String :> Get '[HTML] (Html ())
        :<|> Get '[JSON] (Headers '[Header "Location" String] NoContent)

data SubmitForm = SubmitForm
    { name :: String
    , asm :: String
    , config :: String
    , comment :: String
    , variant :: Maybe String
    }
    deriving (Show, Generic, FromForm)

data Config = Config
    { cPort :: Int
    , cWrenchPath :: FilePath
    , cWrenchArgs :: [String]
    , cStoragePath :: FilePath
    , cVariantsPath :: FilePath
    }
    deriving (Show)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    cPort <- maybe 8080 Unsafe.read <$> lookupEnv "PORT"
    (cWrenchPath : cWrenchArgs) <- maybe ["stack", "exec", "wrench", "--"] (splitOn " ") <$> lookupEnv "WRENCH_EXEC"
    cStoragePath <- fromMaybe "uploads" <$> lookupEnv "STORAGE_PATH"
    cVariantsPath <- fromMaybe "variants" <$> lookupEnv "VARIANTS"
    let conf = Config{cPort, cWrenchPath, cWrenchArgs, cStoragePath, cVariantsPath}
    print conf
    putStrLn $ "Starting server on port " <> show cPort
    run cPort (app conf)

app :: Config -> Application
app conf = serve api (server conf)

api :: Proxy API
api = Proxy

server :: Config -> Server API
server conf =
    submitForm conf
        :<|> formPage conf
        :<|> resultPage conf
        :<|> redirectToForm

formPage :: Config -> Handler (Html ())
formPage Config{cVariantsPath} = do
    variants <- liftIO $ listVariants cVariantsPath
    let options = map (\v -> "<option value=\"" <> toText v <> "\">" <> toText v <> "</option>") variants
    template <- liftIO (decodeUtf8 <$> readFileBS "static/form.html")
    version <- liftIO $ toText <$> getWrenchVersion
    let renderTemplate = replace "{{variants}}" (mconcat options) . replace "{{version}}" version
    return $ toHtmlRaw $ renderTemplate template

listVariants :: FilePath -> IO [String]
listVariants path = do
    contents <- listDirectory path
    filterM (doesDirectoryExist . (path </>)) contents

submitForm :: Config -> SubmitForm -> Handler (Headers '[Header "Location" String] NoContent)
submitForm conf@Config{cStoragePath, cVariantsPath} SubmitForm{name, asm, config, comment, variant} = do
    guid <- liftIO nextRandom
    let dir = cStoragePath <> "/" <> show guid

    liftIO $ createDirectoryIfMissing True dir
    let asmFile = dir <> "/source.s"
        configFile = dir <> "/config.yaml"
    liftIO $ writeFile asmFile asm
    liftIO $ writeFile configFile config
    liftIO $ writeFile (dir <> "/name.txt") name
    liftIO $ writeFile (dir <> "/comment.txt") comment
    liftIO $ writeFile (dir <> "/variant.txt") $ fromMaybe "-" variant

    currentTime <- liftIO getCurrentTime
    version <- liftIO getWrenchVersion
    (exitCode, stdout_, stderr_) <- liftIO $ runSimulation conf asmFile configFile
    liftIO
        $ writeFile
            (dir <> "/status.log")
            ("$ date\n" <> show currentTime <> "\n$ wrench --version\n" <> version <> "\n" <> show exitCode <> "\n" <> stderr_)
    liftIO $ writeFile (dir <> "/result.log") stdout_

    case variant of
        Nothing -> do
            liftIO $ writeFile (dir <> "/test_cases_status.log") ""
            liftIO $ writeFile (dir <> "/test_cases_result.log") ""
        Just variant' -> do
            variantDir <- liftIO $ sort . map takeFileName <$> listFiles (cVariantsPath </> variant')
            let yamlFiles = filter (isSuffixOf ".yaml" . toText) variantDir
            forM_ yamlFiles $ \yamlFile -> do
                let fn = cVariantsPath </> variant' </> yamlFile
                (tcExitCode, tcStdout, tcStderr) <- liftIO $ runSimulation conf asmFile fn
                liftIO $ appendFile (dir <> "/test_cases_status.log") (yamlFile <> ": " <> show tcExitCode <> "\n" <> tcStderr)
                when (tcExitCode /= ExitSuccess) $ do
                    simConf <- liftIO $ decodeUtf8 <$> readFileBS fn
                    liftIO $ writeFile (dir <> "/test_cases_result.log") (simConf <> "\n\n===\n\n" <> tcStdout)
                    return ()

    let location = "/result/" <> show guid
    throwError $ err301{errHeaders = [("Location", location)]}

getWrenchVersion :: IO String
getWrenchVersion = do
    (code, out, _) <- readProcessWithExitCode "wrench" ["--version"] ""
    if code == ExitSuccess
        then return out
        else return "unknown"

runSimulation :: Config -> FilePath -> FilePath -> IO (ExitCode, String, String)
runSimulation Config{cWrenchPath, cWrenchArgs} asmFile configFile = do
    let wrenchCmd = cWrenchPath <> toString (unwords (map toText cWrenchArgs))
    putStrLn ("process: " <> wrenchCmd <> " " <> asmFile <> " -c " <> configFile)
    readProcessWithExitCode cWrenchPath (cWrenchArgs <> [asmFile, "-c", configFile]) ""

resultPage :: Config -> String -> Handler (Html ())
resultPage Config{cStoragePath} guid = do
    let dir = cStoragePath <> "/" <> guid

    nameContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/name.txt"))
    commentContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/comment.txt"))
    asmContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/source.s"))
    configContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/config.yaml"))
    logContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/result.log"))
    status <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/status.log"))
    testCaseStatus <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/test_cases_status.log"))
    testCaseResult <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/test_cases_result.log"))

    template <- liftIO (decodeUtf8 <$> readFileBS "static/result.html")

    let renderTemplate =
            replace "{{name}}" nameContent
                . replace "{{comment}}" commentContent
                . replace "{{assembler_code}}" asmContent
                . replace "{{yaml_content}}" configContent
                . replace "{{status}}" status
                . replace "{{result}}" logContent
                . replace "{{test_cases_status}}" testCaseStatus
                . replace "{{test_cases_result}}" testCaseResult

    return $ toHtmlRaw $ renderTemplate template

listFiles :: FilePath -> IO [FilePath]
listFiles path = do
    contents <- listDirectory path
    filterM doesFileExist (map (path </>) contents)

redirectToForm :: Handler (Headers '[Header "Location" String] NoContent)
redirectToForm = throwError $ err301{errHeaders = [("Location", "/submit-form")]}
