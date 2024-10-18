module Translator.Parser.DataSection (
    dataSection,
) where

import Data.List (singleton)
import Relude
import Relude.Unsafe (read)
import Text.Megaparsec (choice, manyTill, sepBy)
import Text.Megaparsec.Char (char, hspace, hspace1, string)
import Text.Megaparsec.Char.Lexer (charLiteral)
import Translator.Parser.Misc
import Translator.Parser.Types
import Translator.Types

dataSection :: (Read w) => String -> Parser [DataToken w String]
dataSection cstart = do
    string ".data" >> eol' cstart
    catMaybes
        <$> many
            ( choice
                [ nothing (hspace1 <|> eol' cstart)
                , Just <$> dataSectionItemM cstart
                ]
            )

dataSectionItemM :: (Read w) => String -> Parser (DataToken w String)
dataSectionItemM cstart = do
    n <- label
    hspace1
    DataToken n <$> dataValue cstart

dataValue :: (Read w) => String -> Parser (DataValue w)
dataValue cstart = do
    wrapper :: (Read w) => [String] -> DataValue w <-
        choice
            [ string ".byte" >> return (DByte . map read)
            , string ".word" >> return (DWord . map read)
            ]
    hspace1
    values <- sepBy value (string "," >> hspace)
    eol' cstart
    return $ wrapper $ concat values
    where
        value =
            choice
                [ stringArray
                , singleton <$> hexNum
                , singleton <$> num
                ]

stringArray :: Parser [String]
stringArray = do
    _ <- quote
    strings <- manyTill charLiteral quote
    return $ map (show . ord) strings
    where
        quote = char '\''
