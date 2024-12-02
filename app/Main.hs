{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Version
import Development.GitRev
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
import Wrench

options :: Parser Options
options =
    Options
        <$> strArgument
            ( metavar "INPUT"
                <> help "Input assembler file (.s)"
                <> value "test/factorial.s"
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
                    <> metavar "FILENAME"
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
        fullVersion = showVersion version <> " (" <> take 7 $(gitHash) <> ")" <> " " <> $(gitCommitDate)
        opts =
            info
                (options <**> helper <**> simpleVersioner fullVersion)
                (fullDesc <> progDesc "App for laboratory course of computer architecture.")
