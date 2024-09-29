{-# OPTIONS_GHC -Wno-orphans #-}

module Translator.Parser (asmParser) where

import Data.Functor
import Relude hiding (many)
import Text.Megaparsec (anySingle, choice, eof, many, manyTill, try)
import Text.Megaparsec.Char (eol, space1)
import Translator.Parser.CodeSection
import Translator.Parser.DataSection
import Translator.Parser.Misc
import Translator.Parser.Types
import Translator.Types

asmParser ::
    (MnemonicParser isa, MachineWord w) =>
    Parser [Section isa w String]
asmParser =
    do
        secs <-
            catMaybes
                <$> many
                    ( choice
                        [ nothing (space1 <|> eol')
                        , dataSection <&> Just . Data
                        , codeSection <&> Just . Code
                        ]
                    )
        eof
        return secs

instance MnemonicParser String where
    mnemonic = do
        manyTill
            anySingle
            (try (void eol <|> void comment))
