{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitHash)
import Language.Haskell.TH.Env (envQ)
import Options.Applicative (
    Parser,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    progDesc,
    short,
    showDefault,
    simpleVersioner,
    strArgument,
    strOption,
    switch,
    value,
 )
import Paths_wrench (version)
import Relude
import Wrench (Options (..), wrenchIO)

options :: Parser Options
options =
    Options
        <$> strArgument
            ( metavar "INPUT"
                <> help "Input assembler file (.s)"
            )
        <*> strOption
            ( long "isa"
                <> help "ISA"
                <> showDefault
                <> metavar "ISA"
                <> value "risc-iv-32"
                <> help "ISA (risc-iv-32, f32a, acc32)"
            )
        <*> optional
            ( strOption
                ( long "conf"
                    <> short 'c'
                    <> help "Configuration file (.yaml)"
                    <> showDefault
                    <> metavar "CONF"
                )
            )
        <*> switch
            ( short 'S'
                <> help "Only run preprocess and translation steps"
            )
        <*> switch
            ( long "verbose"
                <> short 'v'
                <> help "Verbose output"
            )

main :: IO ()
main = wrenchIO =<< execParser opts
    where
        edge = maybe "" (\(_ :: String) -> "-EDGE") $$(envQ "EDGE_BUILD")
        fullVersion = showVersion version <> edge <> " (" <> take 7 $(gitHash) <> ")" <> " " <> $(gitCommitDate)
        opts =
            info
                (options <**> helper <**> simpleVersioner fullVersion)
                (fullDesc <> progDesc "App for laboratory course of computer architecture.")
