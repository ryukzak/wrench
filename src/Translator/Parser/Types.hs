module Translator.Parser.Types (
    Parser,
    MnemonicParser (..),
) where

import Relude
import Text.Megaparsec (Parsec)

type Parser = Parsec Void String

class MnemonicParser m where
    mnemonic :: Parser m
