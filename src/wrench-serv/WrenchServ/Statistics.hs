module WrenchServ.Statistics (
    postHogTracker,
    MixpanelEvent (..),
    trackEvent,
    getTrack,
    trackCookie,
) where

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

posthogApiKey :: Text
posthogApiKey = "phc_4MVBHknwF8Qok57n2J5S9OVP3z6BpRJM4fiDtH7rGg7"

postHogTracker :: Text
postHogTracker =
    unlines
        [ "<script>"
        , "    !function(t,e){var o,n,p,r;e.__SV||(window.posthog=e,e._i=[],e.init=function(i,s,a){function g(t,e){var o=e.split(\".\");2==o.length&&(t=t[o[0]],e=o[1]),t[e]=function(){t.push([e].concat(Array.prototype.slice.call(arguments,0)))}}(p=t.createElement(\"script\")).type=\"text/javascript\",p.crossOrigin=\"anonymous\",p.async=!0,p.src=s.api_host.replace(\".i.posthog.com\",\"-assets.i.posthog.com\")+\"/static/array.js\",(r=t.getElementsByTagName(\"script\")[0]).parentNode.insertBefore(p,r);var u=e;for(void 0!==a?u=e[a]=[]:a=\"posthog\",u.people=u.people||[],u.toString=function(t){var e=\"posthog\";return\"posthog\"!==a&&(e+=\".\"+a),t||(e+=\" (stub)\"),e},u.people.toString=function(){return u.toString(1)+\".people (stub)\"},o=\"init capture register register_once register_for_session unregister unregister_for_session getFeatureFlag getFeatureFlagPayload isFeatureEnabled reloadFeatureFlags updateEarlyAccessFeatureEnrollment getEarlyAccessFeatures on onFeatureFlags onSurveysLoaded onSessionId getSurveys getActiveMatchingSurveys renderSurvey canRenderSurvey canRenderSurveyAsync identify setPersonProperties group resetGroups setPersonPropertiesForFlags resetPersonPropertiesForFlags setGroupPropertiesForFlags resetGroupPropertiesForFlags reset get_distinct_id getGroups get_session_id get_session_replay_url alias set_config startSessionRecording stopSessionRecording sessionRecordingStarted captureException loadToolbar get_property getSessionProperty createPersonProfile opt_in_capturing opt_out_capturing has_opted_in_capturing has_opted_out_capturing clear_opt_in_out_capturing debug getPageViewId captureTraceFeedback captureTraceMetric\".split(\" \"),n=0;n<o.length;n++)g(u,o[n]);e._i.push([i,s,a])},e.__SV=1)}(document,window.posthog||[]);"
        , "    posthog.init('" <> posthogApiKey <> "', {"
        , "        api_host: 'https://eu.i.posthog.com',"
        , "        person_profiles: 'identified_only', // or 'always' to create profiles for anonymous users as well"
        , "    })"
        , "</script>"
        ]
