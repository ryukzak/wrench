module Translator.Parser.CodeSection (
    codeSection,
) where

import Relude
import Text.Megaparsec (choice)
import Text.Megaparsec.Char (hspace1, string)
import Translator.Parser.Misc
import Translator.Parser.Types
import Translator.Types

codeSection ::
    (MnemonicParser isa) =>
    Parser [CodeToken isa String]
codeSection = do
    string ".text" >> eol'
    catMaybes
        <$> many
            ( choice
                [ nothing (hspace1 <|> eol')
                , Just . Mnemonic <$> mnemonic
                , Just . Label <$> label
                ]
            )
