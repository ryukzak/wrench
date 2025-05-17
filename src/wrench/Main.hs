module Main (main) where

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
import Relude
import Wrench.Misc (wrenchVersion)
import Wrench.Wrench (Options (..), runWrenchIO)

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
main = runWrenchIO =<< execParser opts
    where
        opts =
            info
                (options <**> helper <**> simpleVersioner (toString wrenchVersion))
                (fullDesc <> progDesc "App for laboratory course of computer architecture.")
