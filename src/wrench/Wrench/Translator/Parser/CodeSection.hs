module Wrench.Translator.Parser.CodeSection (
    codeSection,
) where

import Relude
import Text.Megaparsec (choice)
import Text.Megaparsec.Char (hspace1, string)
import Wrench.Translator.Parser.Misc
import Wrench.Translator.Parser.Types
import Wrench.Translator.Types

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
