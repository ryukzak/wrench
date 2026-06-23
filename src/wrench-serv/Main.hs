{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Exception (try)
import Crypto.Hash.SHA1 qualified as SHA1
import Data.Aeson (FromJSON (..), defaultOptions, eitherDecodeStrict, genericParseJSON)
import Data.Aeson.Types (Options (..))
import Data.ByteString qualified as B
import Data.Char (toLower)
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
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, renameFile)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeBaseName, takeFileName, (</>))
import Wrench.Misc (wrenchVersion)
import WrenchServ.Config
import WrenchServ.Simulation
import WrenchServ.Statistics

formatCodeWithLineNumbers :: Text -> Text
formatCodeWithLineNumbers code =
    let codeLines = T.lines code
        lineCount = length codeLines
        lineNumbers = T.concat $ map (\i -> "<div class=\"line-number\">" <> show i <> "</div>") [1 .. lineCount]
        codeContent = T.concat $ map (\line -> "<div class=\"code-line\">" <> escapeHtml line <> "</div>") codeLines
        container =
            "<div class=\"code-container\"><div class=\"line-numbers\">"
                <> lineNumbers
                <> "</div><div class=\"code-content\">"
                <> codeContent
                <> "</div></div>"
     in container

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
        :<|> "report" :> GetReport
        :<|> "examples" :> GetExamples
        :<|> "assets" :> Raw
        :<|> Get '[JSON] (Headers '[Header "Location" Text] NoContent)

server :: Config -> Server API
server conf =
    getForm conf
        :<|> submitForm conf
        :<|> getReport conf
        :<|> getExamples conf
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
    guid <- liftIO nextRandom
    liftIO $ spitSimulationRequest cStoragePath guid task

    let dir = cStoragePath <> "/" <> show guid
        asmFile = dir <> "/source.s"
        configFile = dir <> "/config.yaml"
        pendingFile = dir <> "/pending"

    let simulationTask = SimulationTask{stIsa = isa, stAsmFn = asmFile, stConfFn = configFile, stGuid = guid}

    liftIO $ writeFileText pendingFile "Starting simulation...\n"

    liftIO $ void $ forkIO $ do
        jobResult <- try @SomeException $ do
            startAt <- now

            appendFileText pendingFile "Running dump...\n"
            spitDump conf simulationTask

            appendFileText pendingFile "Running main simulation...\n"
            SimulationResult{srOutput, srStatusLog, srSuccess = userSimSuccess} <- doSimulation conf simulationTask
            writeFileText (dir <> "/status.log") srStatusLog
            writeFileText (dir <> "/result.log") srOutput
            appendFileText pendingFile "Main simulation done.\n"

            varChecks <- case variant of
                Nothing -> return []
                Just variant' -> do
                    yamlFiles <- listTextCases (cVariantsPath </> toString variant')
                    let total = length yamlFiles
                    appendFileText pendingFile $ "Running " <> show total <> " variant test cases...\n"
                    forM (zip [1 ..] yamlFiles) $ \(i, yamlFile) -> do
                        appendFileText pendingFile $ "Test case " <> show (i :: Int) <> "/" <> show total <> "...\n"
                        doSimulation conf simulationTask{stConfFn = cVariantsPath </> toString variant' </> yamlFile}

            writeFileText (dir <> "/test_cases_status.log") ""
            forM_ varChecks $ \(SimulationResult{srTestCaseStatus}) ->
                appendFileText (dir <> "/test_cases_status.log") srTestCaseStatus

            writeFileText (dir <> "/test_cases_result.log") ""

            let wins = filter (\(SimulationResult{srExitCode}) -> srExitCode == ExitSuccess) varChecks
                fails = filter (\(SimulationResult{srExitCode}) -> srExitCode /= ExitSuccess) varChecks
            forM_ (take 1 fails) $ \(SimulationResult{srTestCase}) ->
                writeFileText (dir <> "/test_cases_result.log") srTestCase

            -- Execution stats for every variant case (the `Execution statistics`
            -- report block injected into each generated variant config). Headed by
            -- the case's status line so the report page can show stats per case.
            let statsLog =
                    T.intercalate "\n\n---\n\n"
                        $ map
                            ( \(SimulationResult{srTestCaseStatus, srStats}) ->
                                "# " <> T.takeWhile (/= '\n') srTestCaseStatus <> "\n" <> srStats
                            )
                            varChecks
            writeFileText (dir <> "/stats.log") statsLog

            endAt <- now
            track <- getTrack cookie
            posthogId <- getPosthogIdFromCookie cookie (track <> "_mp")
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
            trackEvent conf event
            appendFileText pendingFile $ "All done in " <> show (endAt - startAt) <> "s.\n"
            renameFile pendingFile (dir <> "/pending.done")

        case jobResult of
            Left e -> appendFileText pendingFile $ "ERROR: " <> show e <> "\n"
            Right _ -> return ()

    track <- liftIO $ getTrack cookie
    let locationHeader = ("Location", "/report/" <> show guid)
        cookieHeader = ("Set-Cookie", encodeUtf8 $ trackCookie track)
    throwError $ err301{errHeaders = [locationHeader, cookieHeader]}

type GetReport =
    Header "Cookie" Text
        :> Capture "guid" UUID
        :> Get '[HTML] (Headers '[Header "Set-Cookie" Text] (Html ()))

getReport :: Config -> Maybe Text -> UUID -> Handler (Headers '[Header "Set-Cookie" Text] (Html ()))
getReport conf@Config{cStoragePath, cExamplesPath} cookie guid = do
    dir <- liftIO $ do
        let storageDir = cStoragePath <> "/" <> show guid
        let examplesDir = cExamplesPath <> "/" <> show guid
        storageExists <- doesDirectoryExist storageDir
        return $ if storageExists then storageDir else examplesDir

    pending <- liftIO $ doesFileExist (dir <> "/pending")
    if pending
        then do
            logContent <- liftIO $ decodeUtf8 <$> readFileBS (dir <> "/pending")
            template <- liftIO $ decodeUtf8 <$> readFileBS "static/inprogress.html"
            track <- liftIO $ getTrack cookie
            let rendered =
                    foldl'
                        (\st (pat, new) -> replace pat new st)
                        template
                        [ ("{{log}}", escapeHtml logContent)
                        , ("{{tracker}}", postHogTracker)
                        ]
            return $ addHeader (trackCookie track) $ toHtmlRaw rendered
        else do
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
            dump <- liftIO (fromMaybe "DUMP NOT AVAILABLE" <$> maybeReadFile (dir <> "/dump.txt"))
            stats <- liftIO (fromMaybe "" <$> maybeReadFile (dir <> "/stats.log"))

            template <- liftIO (decodeUtf8 <$> readFileBS "static/result.html")

            let templateWithBasicContent =
                    foldl'
                        (\st (pat, new) -> replace pat new st)
                        (replace "{{tracker}}" postHogTracker template)
                        [ ("{{name}}", escapeHtml nameContent)
                        , ("{{variant}}", escapeHtml variantContent)
                        , ("{{comment}}", escapeHtml commentContent)
                        , ("{{status}}", escapeHtml status)
                        , ("{{test_cases_status}}", escapeHtml testCaseStatus)
                        , ("{{report_wrench_version}}", escapeHtml reportWrenchVersion)
                        , ("{{version_warning}}", versionWarning reportWrenchVersion)
                        ]

            let renderTemplate =
                    foldl'
                        (\st (pat, new) -> replace pat new st)
                        templateWithBasicContent
                        [ ("{{assembler_code}}", formatCodeWithLineNumbers asmContent)
                        , ("{{yaml_content}}", formatCodeWithLineNumbers configContent)
                        , ("{{result}}", formatCodeWithLineNumbers logContent)
                        , ("{{test_cases_result}}", formatCodeWithLineNumbers testCaseResult)
                        , ("{{execution_stats}}", formatCodeWithLineNumbers stats)
                        , ("{{dump}}", formatCodeWithLineNumbers dump)
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

versionWarning :: Text -> Text
versionWarning reportVer
    | reportVer == wrenchVersion = ""
    | otherwise =
        " <span class=\"text-[var(--c-orange)]\">[WARNING: current wrench version is " <> escapeHtml wrenchVersion <> "]</span>"

type GetExamples = Get '[HTML] (Html ())

data ExampleEntry = ExampleEntry
    { eeGuid :: Text
    , eeIsa :: Text
    , eeTitle :: Text
    , eeDescription :: Text
    , eeOk :: Bool
    }
    deriving (Generic)

instance FromJSON ExampleEntry where
    parseJSON =
        genericParseJSON
            defaultOptions
                { fieldLabelModifier = map toLower . drop 2
                }

getExamples :: Config -> Handler (Html ())
getExamples Config{cExamplesPath} = do
    template <- liftIO (decodeUtf8 <$> readFileBS "static/examples.html")
    entries <- liftIO $ readExamplesIndex cExamplesPath
    let rendered =
            foldl'
                (\st (pat, new) -> replace pat new st)
                template
                [ ("{{examples}}", renderExamplesList entries)
                , ("{{tracker}}", postHogTracker)
                , ("{{version}}", wrenchVersion)
                ]
    return $ toHtmlRaw rendered

readExamplesIndex :: FilePath -> IO [ExampleEntry]
readExamplesIndex examplesPath = do
    let indexFn = examplesPath </> "index.json"
    exists <- doesFileExist indexFn
    if not exists
        then return []
        else do
            raw <- readFileBS indexFn
            case eitherDecodeStrict raw of
                Right entries -> return entries
                Left err -> do
                    putStrLn $ "Failed to parse " <> indexFn <> ": " <> err
                    return []

renderExamplesList :: [ExampleEntry] -> Text
renderExamplesList [] =
    "<p class=\"text-[var(--c-grey)]\">No examples available.</p>"
renderExamplesList entries =
    T.concat $ map renderGroup $ groupByIsa $ sortWith (\e -> (eeIsa e, eeTitle e)) entries

groupByIsa :: [ExampleEntry] -> [(Text, [ExampleEntry])]
groupByIsa = foldr step []
    where
        step e [] = [(eeIsa e, [e])]
        step e (g@(isa, es) : rest)
            | eeIsa e == isa = (isa, e : es) : rest
            | otherwise = (eeIsa e, [e]) : g : rest

renderGroup :: (Text, [ExampleEntry]) -> Text
renderGroup (isa, items) =
    "<div class=\"mb-8\">"
        <> "<h2 class=\"mb-3 pb-1 border-b border-zinc-700 text-[var(--c-grey)] text-xl\">/* "
        <> escapeHtml isa
        <> " */</h2>"
        <> "<ul class=\"space-y-1\">"
        <> T.concat (map renderItem items)
        <> "</ul>"
        <> "</div>"

renderItem :: ExampleEntry -> Text
renderItem ExampleEntry{eeGuid, eeTitle, eeDescription, eeOk} =
    let (statusClass, statusLabel) =
            if eeOk
                then ("text-[var(--c-green)]", "ok")
                else ("text-[var(--c-orange)]", "fail")
        descHtml
            | T.null eeDescription = ""
            | otherwise =
                "<div class=\"w-full pl-[3ch] text-[var(--c-grey)] text-sm\">"
                    <> escapeHtml eeDescription
                    <> "</div>"
     in "<li class=\"flex flex-wrap items-baseline gap-x-2\">"
            <> "<span class=\""
            <> statusClass
            <> "\">["
            <> statusLabel
            <> "]</span>"
            <> "<a href=\"/report/"
            <> escapeHtml eeGuid
            <> "\" class=\"hover:bg-[var(--c-fuschia)] pt-[0.2ch] pb-[0.2ch] text-[var(--c-fuschia)] hover:text-[var(--c-black)] cursor-pointer\">["
            <> escapeHtml eeTitle
            <> "]</a>"
            <> descHtml
            <> "</li>"

redirectToForm :: Handler (Headers '[Header "Location" Text] NoContent)
redirectToForm = throwError $ err301{errHeaders = [("Location", "/submit-form")]}

sortFiles :: [FilePath] -> [FilePath]
sortFiles = sortBy compareFiles
    where
        compareFiles a b =
            let nameA = takeBaseName a
                nameB = takeBaseName b
             in case (reads nameA :: [(Int, String)], reads nameB :: [(Int, String)]) of
                    ([(nA, "")], [(nB, "")]) -> compare nA nB
                    ([(_, "")], _) -> LT
                    (_, [(_, "")]) -> GT
                    _ -> compare nameA nameB

listTextCases :: FilePath -> IO [FilePath]
listTextCases path = do
    contents <- listDirectory path
    files <- filterM doesFileExist (map (path </>) contents)
    return $ sortFiles $ filter (isSuffixOf ".yaml" . toText) $ map takeFileName files

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
