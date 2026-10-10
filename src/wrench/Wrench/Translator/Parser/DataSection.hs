{-# LANGUAGE ScopedTypeVariables #-}

module Wrench.Translator.Parser.DataSection (
    dataSection,
) where

import Data.List (singleton)
import Relude
import Text.Megaparsec (choice, manyTill, sepBy, try)
import Text.Megaparsec.Char (char, hspace, hspace1, string)
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

dataValue :: forall w. (IsWord w) => String -> Parser (DataValue w)
dataValue cstart = choice [directive ".byte" byteLiteral DByte, directive ".word" wordLiteral DWord]
    where
        directive :: (Num a) => String -> Parser a -> ([a] -> DataValue w) -> Parser (DataValue w)
        directive keyword item wrap = do
            void $ string keyword
            hspace1
            values <- items item
            eol' cstart
            return $ wrap values
        items :: (Num a) => Parser a -> Parser [a]
        items item = concat <$> sepBy (choice [stringArray, singleton <$> item]) (try (hspace >> string "," >> hspace))

stringArray :: (Num a) => Parser [a]
stringArray = do
    _ <- quote
    strings <- manyTill charLiteral quote
    return $ map (fromIntegral . ord) strings
    where
        quote = char '\''
