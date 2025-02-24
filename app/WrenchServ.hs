{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.List.Split
import Data.Text (isSuffixOf, replace)
import Data.Text qualified as T
import Data.Time
import Data.UUID.V4 (nextRandom)
import Lucid (Html, toHtmlRaw)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
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
    { name :: Text
    , asm :: Text
    , config :: Text
    , comment :: Text
    , variant :: Maybe Text
    , isa :: Text
    }
    deriving (FromForm, Generic, Show)

data Config = Config
    { cPort :: Int
    , cWrenchPath :: FilePath
    , cWrenchArgs :: [String]
    , cStoragePath :: FilePath
    , cVariantsPath :: FilePath
    , cLogLimit :: Int
    }
    deriving (Show)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    cPort <- maybe 8080 Unsafe.read <$> lookupEnv "PORT"
    (cWrenchPath : cWrenchArgs) <- maybe ["stack", "exec", "wrench", "--"] (splitOn " ") <$> lookupEnv "WRENCH_EXEC"
    cStoragePath <- fromMaybe "uploads" <$> lookupEnv "STORAGE_PATH"
    cVariantsPath <- fromMaybe "variants" <$> lookupEnv "VARIANTS"
    cLogLimit <- maybe 10000 Unsafe.read <$> lookupEnv "LOG_LIMIT"
    let conf = Config{cPort, cWrenchPath, cWrenchArgs, cStoragePath, cVariantsPath, cLogLimit}
    print conf
    putStrLn $ "Starting server on port " <> show cPort
    run cPort (logStdoutDev $ app conf)

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
submitForm conf@Config{cStoragePath, cVariantsPath, cLogLimit} SubmitForm{name, asm, config, comment, variant, isa} = do
    guid <- liftIO nextRandom
    let dir = cStoragePath <> "/" <> show guid

    liftIO $ createDirectoryIfMissing True dir
    let asmFile = dir <> "/source.s"
        configFile = dir <> "/config.yaml"
    liftIO $ writeFileText asmFile asm
    liftIO $ writeFileText configFile config
    liftIO $ writeFileText (dir <> "/name.txt") name
    liftIO $ writeFileText (dir <> "/comment.txt") comment
    liftIO $ writeFileText (dir <> "/variant.txt") $ fromMaybe "-" variant
    liftIO $ writeFileText (dir <> "/isa.txt") isa

    currentTime <- liftIO getCurrentTime
    version <- liftIO getWrenchVersion
    (exitCode, stdout_, stderr_) <- liftIO $ runSimulation isa conf asmFile configFile
    liftIO
        $ writeFile
            (dir <> "/status.log")
            ("$ date\n" <> show currentTime <> "\n$ wrench --version\n" <> version <> "\n" <> show exitCode <> "\n" <> stderr_)
    let stdoutText = toText stdout_
        stdoutText' =
            if T.length stdoutText > cLogLimit
                then "LOG TOO LONG, CROPPED\n\n" <> T.drop (T.length stdoutText - cLogLimit) stdoutText
                else toText stdout_
    liftIO $ writeFileText (dir <> "/result.log") stdoutText'

    (_exitCode, stdoutDump, _stderrDump) <- liftIO $ dumpOutput isa conf asmFile
    liftIO $ writeFile (dir <> "/dump.log") stdoutDump

    varChecks <- case variant of
        Nothing -> return []
        Just variant' -> do
            variantDir <- liftIO $ sort . map takeFileName <$> listFiles (cVariantsPath </> toString variant')
            let yamlFiles = filter (isSuffixOf ".yaml" . toText) variantDir
            forM yamlFiles $ \yamlFile -> do
                let fn = cVariantsPath </> toString variant' </> yamlFile
                (tcExitCode, tcStdout, tcStderr) <- liftIO $ runSimulation isa conf asmFile fn
                return (yamlFile, fn, tcExitCode, tcStdout, tcStderr)

    liftIO $ writeFile (dir <> "/test_cases_status.log") ""

    forM_ varChecks $ \(yamlFile, _fn, tcExitCode, _tcStdout, tcStderr) -> do
        liftIO
            $ appendFile
                (dir <> "/test_cases_status.log")
                (yamlFile <> ": " <> show tcExitCode <> "\n" <> tcStderr)

    liftIO $ writeFile (dir <> "/test_cases_result.log") ""

    let fails = take 1 $ filter (\(_, _, x, _, _) -> x /= ExitSuccess) varChecks

    forM_ fails $ \(yamlFile, fn, _tcExitCode, tcStdout, tcStderr) -> do
        simConf <- liftIO $ decodeUtf8 <$> readFileBS fn
        liftIO
            $ writeFile
                (dir <> "/test_cases_result.log")
                (yamlFile <> "\n" <> simConf <> "\n\n===\n\n" <> tcStdout <> "\n" <> tcStderr)

    let location = "/result/" <> show guid
    throwError $ err301{errHeaders = [("Location", location)]}

getWrenchVersion :: IO String
getWrenchVersion = do
    (code, out, _) <- readProcessWithExitCode "wrench" ["--version"] ""
    if code == ExitSuccess
        then return out
        else return "unknown"

runSimulation :: Text -> Config -> FilePath -> FilePath -> IO (ExitCode, String, String)
runSimulation isa Config{cWrenchPath, cWrenchArgs} asmFile configFile = do
    let args = cWrenchArgs <> ["--isa", toString isa, asmFile, "-c", configFile]
    putStrLn ("process: " <> cWrenchPath <> " " <> show args)
    readProcessWithExitCode cWrenchPath args ""

dumpOutput :: Text -> Config -> FilePath -> IO (ExitCode, String, String)
dumpOutput isa Config{cWrenchPath, cWrenchArgs} asmFile = do
    let args = cWrenchArgs <> ["--isa", toString isa, asmFile, "-S"]
    putStrLn ("process: " <> cWrenchPath <> " " <> show args)
    readProcessWithExitCode cWrenchPath args ""

maybeReadFile :: FilePath -> IO (Maybe Text)
maybeReadFile path = do
    doesFileExist path >>= \case
        True -> Just . decodeUtf8 <$> readFileBS path
        False -> return Nothing

resultPage :: Config -> String -> Handler (Html ())
resultPage Config{cStoragePath} guid = do
    let dir = cStoragePath <> "/" <> guid

    nameContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/name.txt"))
    variantContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/variant.txt"))
    commentContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/comment.txt"))
    asmContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/source.s"))
    configContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/config.yaml"))
    logContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/result.log"))
    status <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/status.log"))
    testCaseStatus <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/test_cases_status.log"))
    testCaseResult <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/test_cases_result.log"))
    dump <- liftIO (fromMaybe "TOO OLD WRENCH" <$> maybeReadFile (dir <> "/dump.log"))

    template <- liftIO (decodeUtf8 <$> readFileBS "static/result.html")

    let renderTemplate =
            replace "{{name}}" nameContent
                . replace "{{variant}}" variantContent
                . replace "{{comment}}" commentContent
                . replace "{{assembler_code}}" asmContent
                . replace "{{yaml_content}}" configContent
                . replace "{{status}}" status
                . replace "{{result}}" logContent
                . replace "{{test_cases_status}}" testCaseStatus
                . replace "{{test_cases_result}}" testCaseResult
                . replace "{{dump}}" dump

    return $ toHtmlRaw $ renderTemplate template

listFiles :: FilePath -> IO [FilePath]
listFiles path = do
    contents <- listDirectory path
    filterM doesFileExist (map (path </>) contents)

redirectToForm :: Handler (Headers '[Header "Location" String] NoContent)
redirectToForm = throwError $ err301{errHeaders = [("Location", "/submit-form")]}
