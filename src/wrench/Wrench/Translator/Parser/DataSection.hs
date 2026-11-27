{-# LANGUAGE ScopedTypeVariables #-}

module Wrench.Translator.Parser.DataSection (
    dataSection,
) where

import Data.List (singleton)
import Relude
import Text.Megaparsec (choice, getSourcePos, manyTill, sepBy, try)
import Text.Megaparsec.Char (char, hspace1, string)
import Text.Megaparsec.Char.Lexer (charLiteral)
import Wrench.Translator.Parser.Misc
import Wrench.Translator.Parser.Types
import Wrench.Translator.Types

dataSection :: (IsWord w) => String -> Parser (Section isa w Text)
dataSection cstart = do
    string ".data" >> eol' cstart
    items <-
        catMaybes
            <$> many
                ( choice
                    [ nothing (hspace1 <|> eol' cstart)
                    , Just . Item <$> dataSectionItemM cstart
                    , Just . Org <$> orgDirective cstart
                    ]
                )
    return $ Data (sectionOrg items) $ sectionItems items

dataSectionItemM :: (IsWord w) => String -> Parser (DataToken w Text)
dataSectionItemM cstart = do
    n <- label
    hspace1
    DataToken n <$> dataValue cstart

dataValue :: forall w. (IsWord w) => String -> Parser (DataValue w Text)
dataValue cstart =
    choice
        [ DByte <$> directive ".byte" (stringArray <|> (singleton <$> byteLiteral))
        , DWord <$> directive ".word" wordItem
        ]
    where
        wordItem = (map Literal <$> stringArray) <|> (singleton <$> operand)
        operand = (Literal <$> wordLiteral) <|> (LabelAddress <$> getSourcePos <*> labelRef)
        directive :: String -> Parser [a] -> Parser [a]
        directive keyword item = do
            void $ string keyword
            hspace1
            values <- concat <$> sepBy item (try comma)
            eol' cstart
            pure values

stringArray :: (Num a) => Parser [a]
stringArray = do
    _ <- quote
    strings <- manyTill charLiteral quote
    return $ map (fromIntegral . ord) strings
    where
        quote = char '\''
