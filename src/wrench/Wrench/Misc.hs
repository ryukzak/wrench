{-# LANGUAGE TemplateHaskell #-}

module Wrench.Misc (wrenchVersion) where

import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitHash)
import Language.Haskell.TH.Env (envQ)
import Paths_wrench (version)
import Relude

-- | @<version>[-<suffix>] (<commit>) <commit date>@
--
-- The commit and its date are taken from the @GIT_HASH@ and @GIT_DATE@ build
-- environment when set, and only otherwise from the repository itself: a build
-- context does not always carry a usable @.git@ (docker builds, git worktrees,
-- source tarballs), and 'gitHash' silently yields @UNKNOWN@ when it doesn't.
wrenchVersion :: Text
wrenchVersion = ver <> " (" <> commit <> ") " <> date
    where
        suffix :: String = fromMaybe "" $$(envQ "VERSION_SUFFIX")
        ver = toText $ showVersion version <> if suffix == "" then "" else "-" <> suffix
        hashEnv, dateEnv :: Maybe String
        hashEnv = $$(envQ "GIT_HASH")
        dateEnv = $$(envQ "GIT_DATE")
        commit = toText $ take 7 $ hashEnv `orElse` $(gitHash)
        date = toText $ dateEnv `orElse` $(gitCommitDate)
        orElse (Just s@(_ : _)) _ = s
        orElse _ fallback = fallback
