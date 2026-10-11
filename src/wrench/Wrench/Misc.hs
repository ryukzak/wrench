{-# LANGUAGE TemplateHaskell #-}

module Wrench.Misc (wrenchVersion) where

import Data.Version (showVersion)
import Language.Haskell.TH.Env (envQ)
import Paths_wrench (version)
import Relude

-- | @<version>[-<suffix>] [(<commit>) <commit date>]@
wrenchVersion :: Text
wrenchVersion = ver <> provenance
    where
        suffix, hash, date :: String
        suffix = fromMaybe "" $$(envQ "VERSION_SUFFIX")
        hash = fromMaybe "" $$(envQ "GIT_HASH")
        date = fromMaybe "" $$(envQ "GIT_DATE")
        ver = toText $ showVersion version <> if suffix == "" then "" else "-" <> suffix
        provenance = toText $ case (hash, date) of
            ("", _) -> ""
            (h, "") -> " (" <> take 7 h <> ")"
            (h, d) -> " (" <> take 7 h <> ") " <> d
