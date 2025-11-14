module Main (main) where

import Data.Default
import Options.Applicative (
    Parser,
    auto,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    option,
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
                <> help "ISA (risc-iv-32, f32a, acc32, m68k, vliw-iv)"
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
        <*> option
            auto
            ( long "instruction-limit"
                <> metavar "LIMIT"
                <> help "Maximum number of instructions to execute"
                <> value (maxInstructionLimit def)
                <> showDefault
            )
        <*> option
            auto
            ( long "memory-limit"
                <> metavar "SIZE"
                <> help "Maximum memory size in bytes"
                <> value (maxMemoryLimit def)
                <> showDefault
            )
        <*> option
            auto
            ( long "state-log-limit"
                <> metavar "LIMIT"
                <> help "Maximum number of state records to log"
                <> value (maxStateLogLimit def)
                <> showDefault
            )

main :: IO ()
main = runWrenchIO =<< execParser opts
    where
        opts =
            info
                (options <**> helper <**> simpleVersioner (toString wrenchVersion))
                (fullDesc <> progDesc "App for laboratory course of computer architecture.")
