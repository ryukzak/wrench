module Wrench.Translator.Parser.Types (
    Parser,
    MnemonicParser (..),
    CommentStart (..),
) where

import Relude
import Text.Megaparsec (Parsec)

type Parser = Parsec Void String

class CommentStart m where
    commentStart :: String

class (CommentStart m) => MnemonicParser m where
    mnemonic :: Parser m
