{-# LANGUAGE TemplateHaskell #-}

module Misc (wrenchVersion) where

import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitHash)
import Language.Haskell.TH.Env (envQ)
import Paths_wrench (version)
import Relude

wrenchVersion :: Text
wrenchVersion = ver <> " (" <> toText (take 7 $(gitHash)) <> ")" <> " " <> $(gitCommitDate)
    where
        ver = toText $ showVersion version <> maybe "" ("-" <>) $$(envQ "VERSION_SUFFIX")
