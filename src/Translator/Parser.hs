module Translator.Parser (asmParser) where

import Data.Functor
import Relude hiding (many)
import Text.Megaparsec (choice, eof, many)
import Text.Megaparsec.Char (space1)
import Translator.Parser.CodeSection
import Translator.Parser.DataSection
import Translator.Parser.Misc
import Translator.Parser.Types
import Translator.Types

asmParser ::
    forall isa w.
    (MachineWord w, MnemonicParser isa) =>
    Parser [Section isa w String]
asmParser =
    do
        let cstart = commentStart @isa
        secs <-
            catMaybes
                <$> many
                    ( choice
                        [ nothing (space1 <|> eol' cstart)
                        , dataSection cstart <&> Just
                        , codeSection cstart <&> Just
                        ]
                    )
        eof
        return secs
