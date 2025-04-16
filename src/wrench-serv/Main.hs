{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Exception (catch)
import Crypto.Hash.SHA1 qualified as SHA1
import Data.Aeson
import Data.ByteString qualified as B
import Data.List.Split (splitOn)
import Data.Text (isSuffixOf, replace)
import Data.Text qualified as T
import Data.Text.Encoding.Base64 (encodeBase64)
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as V
import Lucid (Html, renderText, toHtml, toHtmlRaw)
import Network.HTTP.Conduit qualified as HTTP
import Network.HTTP.Simple qualified as HTTP
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Numeric (showHex)
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant
import Servant.HTML.Lucid (HTML)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeFileName, (</>))
import System.Process (readProcessWithExitCode)
import Web.Cookie (parseCookies)
import Web.FormUrlEncoded (FromForm)
import Wrench.Misc (wrenchVersion)
import WrenchServ.Statistics (MixpanelEvent (..))

type API =
    "submit"
        :> Header "Cookie" Text
        :> ReqBody '[FormUrlEncoded] SimulationRequest
        :> Post '[JSON] (Headers '[Header "Location" String, Header "Set-Cookie" String] NoContent)
        :<|> "submit-form" :> Get '[HTML] (Html ())
        :<|> "result" :> Header "Cookie" Text :> Capture "guid" UUID :> Get '[HTML] (Headers '[Header "Set-Cookie" String] (Html ()))
        :<|> "assets" :> Raw
        :<|> Get '[JSON] (Headers '[Header "Location" String] NoContent)

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

mask :: Config -> Config
mask conf@Config{cMixpanelToken} =
    conf
        { cMixpanelToken = fmap (const "<REDACTED>") cMixpanelToken
        }

trackEvent :: Config -> MixpanelEvent -> IO ()
trackEvent Config{cMixpanelToken = Nothing, cMixpanelProjectId = Nothing} _ = return ()
trackEvent Config{cMixpanelToken = Just token, cMixpanelProjectId = Just projectId} event = do
    now <- getCurrentTime
    let timestamp = fromInteger $ floor $ utcTimeToPOSIXSeconds now
        eventName = case event of
            SimulationEvent{} -> "Simulation"
            ReportViewEvent{} -> "ReportView"
        baseProperties =
            fromList
                [ ("time", Number timestamp)
                , ("authorName", String $ mpName event)
                , ("distinct_id", String $ decodeUtf8 $ mpTrack event)
                , ("$insert_id", String $ T.replace " " "-" $ show $ mpGuid event)
                , ("simulation_guid", String $ show $ mpGuid event)
                , ("version", String $ mpVersion event)
                -- , ("verbose", Number 1)
                ]
        properties = case event of
            SimulationEvent{mpIsa, mpVariant} ->
                fromList
                    [ ("isa", String mpIsa)
                    , ("variant", maybe Null String mpVariant)
                    , ("asm_sha1", String $ mpAsmSha1 event)
                    , ("yaml_sha1", String $ mpYamlSha1 event)
                    , ("win_count", Number $ fromInteger $ toInteger $ mpWinCount event)
                    , ("fail_count", Number $ fromInteger $ toInteger $ mpFailCount event)
                    ]
            ReportViewEvent{} -> fromList []
        payload =
            V.fromList
                [ object
                    [ ("event", String eventName)
                    , ("properties", Object (baseProperties <> properties))
                    ]
                ]
        encodedData = encode payload
        auth = "Basic " <> (encodeUtf8 (encodeBase64 $ token <> ":") :: ByteString)

    request <- HTTP.parseRequest $ "POST https://api-eu.mixpanel.com/import?strict=1&project_id=" <> toString projectId
    let request' =
            HTTP.setRequestBody (HTTP.RequestBodyLBS encodedData)
                $ HTTP.setRequestHeader "Content-Type" ["application/json"]
                $ HTTP.setRequestHeader "Authorization" [auth] request
    catch
        (void $ HTTP.httpNoBody request')
        (\(e :: SomeException) -> putStrLn $ "Mixpanel tracking error: " ++ show e)
trackEvent Config{} _ = error "Mixpanel misconfiguration"

getTrack :: Maybe Text -> IO ByteString
getTrack cookie = case filter ((== "track_id") . fst) $ parseCookies $ maybe "" encodeUtf8 cookie of
    [c] -> return $ snd c
    _ -> show <$> liftIO nextRandom

trackCookie :: ByteString -> ByteString
trackCookie track = "track_id=" <> track <> "; path=/; Max-Age=10368000"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    cPort <- maybe 8080 Unsafe.read <$> lookupEnv "PORT"
    (cWrenchPath : cWrenchArgs) <- maybe ["stack", "exec", "wrench", "--"] (splitOn " ") <$> lookupEnv "WRENCH_EXEC"
    cStoragePath <- fromMaybe "uploads" <$> lookupEnv "STORAGE_PATH"
    cVariantsPath <- fromMaybe "variants" <$> lookupEnv "VARIANTS"
    cLogLimit <- maybe 10000 Unsafe.read <$> lookupEnv "LOG_LIMIT"
    cMixpanelToken <- fmap toText <$> lookupEnv "MIXPANEL_TOKEN"
    cMixpanelProjectId <- fmap toText <$> lookupEnv "MIXPANEL_PROJECT_ID"
    let conf = Config{cPort, cWrenchPath, cWrenchArgs, cStoragePath, cVariantsPath, cLogLimit, cMixpanelToken, cMixpanelProjectId}
    unless
        ( (isJust cMixpanelToken && isJust cMixpanelProjectId)
            || (isNothing cMixpanelToken && isNothing cMixpanelProjectId)
        )
        $ error "Mixpanel misconfiguration"
    print $ mask conf
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
        :<|> serveDirectoryWebApp "static/assets"
        :<|> redirectToForm

formPage :: Config -> Handler (Html ())
formPage Config{cVariantsPath} = do
    variants <- liftIO $ listVariants cVariantsPath
    let options = map (\v -> "<option value=\"" <> toText v <> "\">" <> toText v <> "</option>") variants
    template <- liftIO (decodeUtf8 <$> readFileBS "static/form.html")
    let renderTemplate = replace "{{variants}}" (mconcat options) . replace "{{version}}" wrenchVersion
    return $ toHtmlRaw $ renderTemplate template

listVariants :: FilePath -> IO [String]
listVariants path = do
    contents <- listDirectory path
    variants <- filterM (doesDirectoryExist . (path </>)) contents
    return $ sort variants

data SimulationRequest = SimulationRequest
    { name :: Text
    , asm :: Text
    , config :: Text
    , comment :: Text
    , variant :: Maybe Text
    , isa :: Text
    }
    deriving (FromForm, Generic, Show)

nameFn, commentFn, variantFn, isaFn, configFn, asmFn :: FilePath -> UUID -> FilePath
nameFn path guid = path <> "/" <> show guid <> "/name.txt"
commentFn path guid = path <> "/" <> show guid <> "/comment.txt"
variantFn path guid = path <> "/" <> show guid <> "/variant.txt"
isaFn path guid = path <> "/" <> show guid <> "/isa.txt"
configFn path guid = path <> "/" <> show guid <> "/config.yaml"
asmFn path guid = path <> "/" <> show guid <> "/source.s"

spitSimulationRequest :: FilePath -> UUID -> SimulationRequest -> IO ()
spitSimulationRequest cStoragePath guid SimulationRequest{name, asm, config, comment, variant, isa} = do
    let dir = cStoragePath <> "/" <> show guid
    createDirectoryIfMissing True dir
    mapM_
        (\(mkFn, content) -> writeFileText (mkFn cStoragePath guid) content)
        [ (asmFn, asm)
        , (configFn, config)
        , (nameFn, name)
        , (commentFn, comment)
        , (variantFn, fromMaybe "-" variant)
        , (isaFn, isa)
        ]

data SimulationTask = SimulationTask
    { stIsa :: Text
    , stAsmFn :: FilePath
    , stConfFn :: FilePath
    , stGuid :: UUID
    }
    deriving (FromForm, Generic, Show)

data SimulationResult = SimulationResult
    { srExitCode :: ExitCode
    , srOutput :: Text
    , srError :: Text
    , srCmd :: Text
    , srStatusLog :: Text
    , srTestCaseStatus :: Text
    , srTestCase :: Text
    }
    deriving (Generic, Show)

spitDump :: Config -> SimulationTask -> IO ()
spitDump Config{cStoragePath, cWrenchPath, cWrenchArgs} SimulationTask{stIsa, stAsmFn, stGuid} = do
    let args = cWrenchArgs <> ["--isa", toString stIsa, stAsmFn, "-S"]
        dumpFn = cStoragePath <> "/" <> show stGuid <> "/dump.log"
    (_exitCode, stdoutDump, _stderrDump) <- readProcessWithExitCode cWrenchPath args ""
    writeFile dumpFn stdoutDump

doSimulation :: Config -> SimulationTask -> IO SimulationResult
doSimulation Config{cWrenchPath, cWrenchArgs, cLogLimit} SimulationTask{stIsa, stAsmFn, stConfFn} = do
    let args = cWrenchArgs <> ["--isa", toString stIsa, stAsmFn, "-c", stConfFn]
        srCmd = T.intercalate " " $ map toText ([cWrenchPath] <> args)
    simConf <- decodeUtf8 <$> readFileBS stConfFn
    currentTime <- getCurrentTime
    (srExitCode, out, err) <- readProcessWithExitCode cWrenchPath args ""
    let srStatusLog =
            T.intercalate
                "\n"
                ["$ date", show currentTime, "$ wrench --version", wrenchVersion, srCmd, show srExitCode, toText srError]
        stdoutText = toText out
        srOutput =
            if T.length stdoutText > cLogLimit
                then "LOG TOO LONG, CROPPED\n\n" <> T.drop (T.length stdoutText - cLogLimit) stdoutText
                else toText stdoutText
        srError = toText err
        srTestCaseStatus = toText stConfFn <> ": " <> show srExitCode <> "\n" <> srError
        srTestCase =
            T.intercalate
                "\n\n"
                [ "# " <> toText stConfFn
                , simConf <> "==="
                , srOutput <> srError
                , "==="
                , srCmd
                ]
    return $ SimulationResult{srExitCode, srOutput, srError, srCmd, srStatusLog, srTestCase, srTestCaseStatus}

listTextCases :: FilePath -> IO [FilePath]
listTextCases path = do
    contents <- listDirectory path
    files <- filterM doesFileExist (map (path </>) contents)
    return $ sort $ filter (isSuffixOf ".yaml" . toText) $ map takeFileName files

submitForm ::
    Config
    -> Maybe Text
    -> SimulationRequest
    -> Handler (Headers '[Header "Location" String, Header "Set-Cookie" String] NoContent)
submitForm conf@Config{cStoragePath, cVariantsPath} cookie task@SimulationRequest{name, variant, isa, asm, config} = do
    guid <- liftIO nextRandom
    liftIO $ spitSimulationRequest cStoragePath guid task

    let dir = cStoragePath <> "/" <> show guid
        asmFile = dir <> "/source.s"
        configFile = dir <> "/config.yaml"

    let simulationTask = SimulationTask{stIsa = isa, stAsmFn = asmFile, stConfFn = configFile, stGuid = guid}

    liftIO $ spitDump conf simulationTask

    SimulationResult{srOutput, srStatusLog} <- liftIO $ doSimulation conf simulationTask

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

    track <- liftIO $ getTrack cookie
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
                }
    liftIO $ trackEvent conf event
    throwError $ err301{errHeaders = [("Location", "/result/" <> show guid), ("Set-Cookie", trackCookie track)]}

maybeReadFile :: FilePath -> IO (Maybe Text)
maybeReadFile path = do
    doesFileExist path >>= \case
        True -> Just . decodeUtf8 <$> readFileBS path
        False -> return Nothing

escapeHtml :: Text -> Text
escapeHtml = toText . renderText . toHtml

resultPage :: Config -> Maybe Text -> UUID -> Handler (Headers '[Header "Set-Cookie" String] (Html ()))
resultPage conf@Config{cStoragePath} cookie guid = do
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
    dump <- liftIO (fromMaybe "TOO OLD WRENCH" <$> maybeReadFile (dir <> "/dump.log"))

    template <- liftIO (decodeUtf8 <$> readFileBS "static/result.html")

    let renderTemplate =
            foldl'
                (\st (pat, new) -> replace pat (escapeHtml new) st)
                template
                [ ("{{name}}", nameContent)
                , ("{{variant}}", variantContent)
                , ("{{comment}}", commentContent)
                , ("{{assembler_code}}", asmContent)
                , ("{{yaml_content}}", configContent)
                , ("{{status}}", status)
                , ("{{result}}", logContent)
                , ("{{test_cases_status}}", testCaseStatus)
                , ("{{test_cases_result}}", testCaseResult)
                , ("{{dump}}", dump)
                ]

    track <- liftIO $ getTrack cookie
    let event = ReportViewEvent{mpGuid = guid, mpName = nameContent, mpVersion = wrenchVersion, mpTrack = track}
    liftIO $ trackEvent conf event
    return $ addHeader (decodeUtf8 (trackCookie track)) $ toHtmlRaw renderTemplate

redirectToForm :: Handler (Headers '[Header "Location" String] NoContent)
redirectToForm = throwError $ err301{errHeaders = [("Location", "/submit-form")]}

sha1 :: Text -> Text
sha1 text =
    let noSpaceText = T.replace " " "" $ T.replace "\n" "" $ T.replace "\t" " " text
        ctx0 = SHA1.init
        ctx = SHA1.update ctx0 $ encodeUtf8 noSpaceText
     in toText $ B.foldr showHex "" $ SHA1.hash $ SHA1.finalize ctx
