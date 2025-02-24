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
    String
    -> Parser [CodeToken isa String]
codeSection cstart = do
    string ".text" >> eol' cstart
    catMaybes
        <$> many
            ( choice
                [ nothing (hspace1 <|> eol' cstart)
                , Just . Label <$> label
                , Just . Mnemonic <$> mnemonic
                ]
            )
