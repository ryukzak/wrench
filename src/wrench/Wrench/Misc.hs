{-# LANGUAGE TemplateHaskell #-}

module Wrench.Misc (wrenchVersion) where

import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitHash)
import Language.Haskell.TH.Env (envQ)
import Paths_wrench (version)
import Relude

wrenchVersion :: Text
wrenchVersion = ver <> " (" <> toText (take 7 $(gitHash)) <> ")" <> " " <> $(gitCommitDate)
    where
        suffix :: String = fromMaybe "" $$(envQ "VERSION_SUFFIX")
        ver = toText $ showVersion version <> if suffix == "" then "" else "-" <> suffix
