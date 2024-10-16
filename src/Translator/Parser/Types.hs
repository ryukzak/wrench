module Translator.Parser.Types (
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

-- data Foo a b = Foo a b
-- data Bar a = Bar a

-- instance CommentStart Foo where
--     commentStart = ";"

-- instance CommentStart Bar where
--     commentStart = ";"
