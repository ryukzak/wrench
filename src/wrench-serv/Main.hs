{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Crypto.Hash.SHA1 qualified as SHA1
import Data.ByteString qualified as B
import Data.Text (isSuffixOf, replace)
import Data.Text qualified as T
import Data.Time (getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Lucid (Html, renderText, toHtml, toHtmlRaw)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Numeric (showHex)
import Relude
import Servant
import Servant.HTML.Lucid (HTML)
import System.Directory (doesFileExist, listDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeFileName, (</>))
import Wrench.Misc (wrenchVersion)
import WrenchServ.Config
import WrenchServ.Simulation
import WrenchServ.Statistics

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    conf@Config{cPort} <- initConfig
    print $ mask conf
    putStrLn $ "Starting server on port " <> show cPort
    run cPort (logStdoutDev $ app conf)

app :: Config -> Application
app conf = serve (Proxy :: Proxy API) (server conf)

type API =
    "submit-form" :> GetForm
        :<|> "submit" :> SubmitForm
        :<|> "result" :> GetReport
        :<|> "assets" :> Raw
        :<|> Get '[JSON] (Headers '[Header "Location" Text] NoContent)

server :: Config -> Server API
server conf =
    getForm conf
        :<|> submitForm conf
        :<|> getReport conf
        :<|> serveDirectoryWebApp "static/assets"
        :<|> redirectToForm

type GetForm = Header "Cookie" Text :> Get '[HTML] (Html ())

getForm :: Config -> Maybe Text -> Handler (Html ())
getForm conf@Config{cVariants} cookie = do
    let options = map (\v -> "<option value=\"" <> toText v <> "\">" <> toText v <> "</option>") cVariants
    template <- liftIO (decodeUtf8 <$> readFileBS "static/form.html")
    let renderedTemplate =
            foldl'
                (\st (pat, new) -> replace pat new st)
                template
                [ ("{{variants}}", mconcat options)
                , ("{{version}}", wrenchVersion)
                , ("{{tracker}}", postHogTracker)
                ]
    liftIO $ do
        track <- getTrack cookie
        posthogId <- getPosthogIdFromCookie cookie (track <> "_mp")
        trackEvent
            conf
            GetFormEvent
                { mpVersion = wrenchVersion
                , mpTrack = track
                , mpPosthogId = posthogId
                }
    return $ toHtmlRaw renderedTemplate

type SubmitForm =
    Header "Cookie" Text
        :> ReqBody '[FormUrlEncoded] SimulationRequest
        :> Post '[JSON] (Headers '[Header "Location" Text, Header "Set-Cookie" Text] NoContent)

now :: IO Int
now = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> getCurrentTime

submitForm ::
    Config
    -> Maybe Text
    -> SimulationRequest
    -> Handler (Headers '[Header "Location" Text, Header "Set-Cookie" Text] NoContent)
submitForm conf@Config{cStoragePath, cVariantsPath} cookie task@SimulationRequest{name, variant, isa, asm, config} = do
    startAt <- liftIO now
    guid <- liftIO nextRandom
    liftIO $ spitSimulationRequest cStoragePath guid task

    let dir = cStoragePath <> "/" <> show guid
        asmFile = dir <> "/source.s"
        configFile = dir <> "/config.yaml"

    let simulationTask = SimulationTask{stIsa = isa, stAsmFn = asmFile, stConfFn = configFile, stGuid = guid}

    liftIO $ spitDump conf simulationTask

    SimulationResult{srOutput, srStatusLog, srSuccess = userSimSuccess} <- liftIO $ doSimulation conf simulationTask

    liftIO $ writeFileText (dir <> "/status.log") srStatusLog
    liftIO $ writeFileText (dir <> "/result.log") srOutput

    varChecks <- case variant of
        Nothing -> return []
        Just variant' -> do
            yamlFiles <- liftIO $ listTextCases (cVariantsPath </> toString variant')
            liftIO $ forM yamlFiles $ \yamlFile -> do
                doSimulation conf simulationTask{stConfFn = cVariantsPath </> toString variant' </> yamlFile}

    liftIO $ writeFile (dir <> "/test_cases_status.log") ""
    forM_ varChecks $ \(SimulationResult{srTestCaseStatus}) -> do
        let tsStatus = dir <> "/test_cases_status.log"
        liftIO $ appendFileText tsStatus srTestCaseStatus

    liftIO $ writeFile (dir <> "/test_cases_result.log") ""

    let wins = filter (\(SimulationResult{srExitCode}) -> srExitCode == ExitSuccess) varChecks
        fails = filter (\(SimulationResult{srExitCode}) -> srExitCode /= ExitSuccess) varChecks
    forM_ (take 1 fails) $ \(SimulationResult{srTestCase}) -> do
        let testCaseLogFn = dir <> "/test_cases_result.log"
        liftIO $ writeFileText testCaseLogFn srTestCase

    endAt <- liftIO now
    track <- liftIO $ getTrack cookie
    posthogId <- liftIO $ getPosthogIdFromCookie cookie (track <> "_mp")
    let event =
            SimulationEvent
                { mpGuid = guid
                , mpName = name
                , mpIsa = isa
                , mpVariant = variant
                , mpVersion = wrenchVersion
                , mpTrack = track
                , mpAsmSha1 = sha1 asm
                , mpYamlSha1 = sha1 config
                , mpWinCount = length wins
                , mpFailCount = length fails
                , mpPosthogId = posthogId
                , mpSuccess = userSimSuccess
                , mpDuration = toEnum $ endAt - startAt
                , mpVariantSuccess =
                    if not $ null varChecks
                        then Just $ null fails
                        else Nothing
                }
    liftIO $ trackEvent conf event
    throwError
        $ err301{errHeaders = [("Location", "/result/" <> show guid), ("Set-Cookie", encodeUtf8 $ trackCookie track)]}

type GetReport =
    Header "Cookie" Text
        :> Capture "guid" UUID
        :> Get '[HTML] (Headers '[Header "Set-Cookie" Text] (Html ()))

getReport :: Config -> Maybe Text -> UUID -> Handler (Headers '[Header "Set-Cookie" Text] (Html ()))
getReport conf@Config{cStoragePath} cookie guid = do
    let dir = cStoragePath <> "/" <> show guid

    nameContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/name.txt"))
    variantContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/variant.txt"))
    commentContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/comment.txt"))
    asmContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/source.s"))
    configContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/config.yaml"))
    logContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/result.log"))
    status <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/status.log"))
    testCaseStatus <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/test_cases_status.log"))
    testCaseResult <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/test_cases_result.log"))
    reportWrenchVersion <- liftIO $ do
        exist <- doesFileExist (dir <> "/wrench-version.txt")
        if exist
            then decodeUtf8 <$> readFileBS (dir <> "/wrench-version.txt")
            else return "< 0.2.11"
    dump <- liftIO (fromMaybe "TOO OLD WRENCH" <$> maybeReadFile (dir <> "/dump.log"))

    template <- liftIO (decodeUtf8 <$> readFileBS "static/result.html")

    let formatCodeWithLineNumbers :: Text -> Text
        formatCodeWithLineNumbers code =
            let codeLines = T.lines code
                lineCount = length codeLines
                -- Generate line numbers
                lineNumbers = T.concat $ map (\i -> "<div class=\"line-number\">" <> show i <> "</div>") [1..lineCount]
                -- Generate code lines
                codeContent = T.concat $ map (\line -> "<div class=\"code-line\">" <> escapeHtml line <> "</div>") codeLines
                -- Combine into container
                container = "<div class=\"code-container\"><div class=\"line-numbers\">" <> lineNumbers <> "</div><div class=\"code-content\">" <> codeContent <> "</div></div>"
            in container

    let renderTemplate =
            foldl'
                (\st (pat, new) -> replace pat new st)
                (replace "{{tracker}}" postHogTracker template)
                [ ("{{name}}", escapeHtml nameContent)
                , ("{{variant}}", escapeHtml variantContent)
                , ("{{comment}}", escapeHtml commentContent)
                , ("{{assembler_code}}", formatCodeWithLineNumbers asmContent)
                , ("{{yaml_content}}", formatCodeWithLineNumbers configContent)
                , ("{{status}}", escapeHtml status)
                , ("{{result}}", escapeHtml logContent)
                , ("{{test_cases_status}}", escapeHtml testCaseStatus)
                , ("{{test_cases_result}}", escapeHtml testCaseResult)
                , ("{{dump}}", escapeHtml dump)
                ]

    track <- liftIO $ getTrack cookie
    posthogId <- liftIO $ getPosthogIdFromCookie cookie (track <> "_mp")
    let event =
            ReportViewEvent
                { mpGuid = guid
                , mpName = nameContent
                , mpVersion = wrenchVersion
                , mpTrack = track
                , mpPosthogId = posthogId
                , mpWrenchVersion = reportWrenchVersion
                }
    liftIO $ trackEvent conf event
    return $ addHeader (trackCookie track) $ toHtmlRaw renderTemplate

redirectToForm :: Handler (Headers '[Header "Location" Text] NoContent)
redirectToForm = throwError $ err301{errHeaders = [("Location", "/submit-form")]}

listTextCases :: FilePath -> IO [FilePath]
listTextCases path = do
    contents <- listDirectory path
    files <- filterM doesFileExist (map (path </>) contents)
    return $ sort $ filter (isSuffixOf ".yaml" . toText) $ map takeFileName files

maybeReadFile :: FilePath -> IO (Maybe Text)
maybeReadFile path = do
    doesFileExist path >>= \case
        True -> Just . decodeUtf8 <$> readFileBS path
        False -> return Nothing

escapeHtml :: Text -> Text
escapeHtml = toText . renderText . toHtml

sha1 :: Text -> Text
sha1 text =
    let noSpaceText = T.replace " " "" $ T.replace "\n" "" $ T.replace "\t" " " text
        ctx0 = SHA1.init
        ctx = SHA1.update ctx0 $ encodeUtf8 noSpaceText
     in toText $ B.foldr showHex "" $ SHA1.hash $ SHA1.finalize ctx
