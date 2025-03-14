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
    -> Parser (Section isa w String)
codeSection cstart = do
    string ".text" >> eol' cstart
    items <-
        catMaybes
            <$> many
                ( choice
                    [ nothing (hspace1 <|> eol' cstart)
                    , Just . Org <$> orgDirective cstart
                    , Just . Item . Label <$> label
                    , Just . Item . Mnemonic <$> mnemonic
                    ]
                )
    return $ Code (sectionOrg items) $ sectionItems items
