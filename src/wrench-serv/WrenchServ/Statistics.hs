module WrenchServ.Statistics (MixpanelEvent (..), trackEvent, getTrack, trackCookie) where

import Control.Exception (catch)
import Data.Aeson
import Data.Text qualified as T
import Data.Text.Encoding.Base64 (encodeBase64)
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as V
import Network.HTTP.Conduit qualified as HTTP
import Network.HTTP.Simple qualified as HTTP
import Relude
import Web.Cookie (parseCookies)
import WrenchServ.Config

data MixpanelEvent
    = SimulationEvent
        { mpGuid :: UUID
        , mpName :: Text
        , mpIsa :: Text
        , mpVariant :: Maybe Text
        , mpVersion :: Text
        , mpTrack :: ByteString
        , mpAsmSha1 :: Text
        , mpYamlSha1 :: Text
        , mpWinCount :: Int
        , mpFailCount :: Int
        }
    | ReportViewEvent
        { mpGuid :: UUID
        , mpName :: Text
        , mpVersion :: Text
        , mpTrack :: ByteString
        }
    deriving (Show)

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
