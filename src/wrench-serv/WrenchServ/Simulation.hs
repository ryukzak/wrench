module WrenchServ.Simulation (
    SimulationRequest (..),
    SimulationResult (..),
    SimulationTask (..),
    doSimulation,
    spitDump,
    spitSimulationRequest,
    dumpFn,
) where

import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Relude
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)
import Web.FormUrlEncoded (FromForm)
import Wrench.Misc (wrenchVersion)
import WrenchServ.Config

data SimulationRequest = SimulationRequest
    { name :: Text
    , asm :: Text
    , config :: Text
    , comment :: Text
    , variant :: Maybe Text
    , isa :: Text
    }
    deriving (FromForm, Generic, Show)

nameFn, commentFn, variantFn, isaFn, configFn, asmFn, wrenchVersionFn, dumpFn :: FilePath -> UUID -> FilePath
nameFn path guid = path <> "/" <> show guid <> "/name.txt"
commentFn path guid = path <> "/" <> show guid <> "/comment.txt"
variantFn path guid = path <> "/" <> show guid <> "/variant.txt"
isaFn path guid = path <> "/" <> show guid <> "/isa.txt"
configFn path guid = path <> "/" <> show guid <> "/config.yaml"
asmFn path guid = path <> "/" <> show guid <> "/source.s"
wrenchVersionFn path guid = path <> "/" <> show guid <> "/wrench-version.txt"
dumpFn path guid = path <> "/" <> show guid <> "/dump.txt"

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
        , (wrenchVersionFn, wrenchVersion)
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
    , srSuccess :: Bool
    }
    deriving (Generic, Show)

spitDump :: Config -> SimulationTask -> IO ()
spitDump Config{cStoragePath, cWrenchPath, cWrenchArgs} SimulationTask{stIsa, stAsmFn, stGuid} = do
    let args = cWrenchArgs <> ["--isa", toString stIsa, stAsmFn, "-S"]
    (_exitCode, stdoutDump, _stderrDump) <- readProcessWithExitCode cWrenchPath args ""
    writeFile (dumpFn cStoragePath stGuid) stdoutDump

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
    return
        $ SimulationResult
            { srExitCode
            , srSuccess = srExitCode == ExitSuccess
            , srOutput
            , srError
            , srCmd
            , srStatusLog
            , srTestCase
            , srTestCaseStatus
            }
