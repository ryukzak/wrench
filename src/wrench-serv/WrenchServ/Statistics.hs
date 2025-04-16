module WrenchServ.Statistics (MixpanelEvent (..)) where

import Data.UUID (UUID)
import Relude

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
