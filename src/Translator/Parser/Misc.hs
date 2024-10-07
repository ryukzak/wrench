{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Translator.Parser.Misc (
    num,
    hexNum,
    name,
    labelRef,
    comment,
    nothing,
    eol',
    label,
    reference,
) where

import Relude
import Relude.Unsafe (read)
import Text.Megaparsec (anySingle, choice, manyTill, single)
import Text.Megaparsec.Char (char, digitChar, eol, hspace, letterChar, string)
import Translator.Parser.Types
import Translator.Types

num :: Parser String
num =
    choice
        [ char '-' >> many digitChar <&> (:) '-'
        , many digitChar
        ]

hexNum :: Parser String
hexNum = do
    void $ string "0x"
    digits <- many digitChar
    return $ "0x" <> digits

eol' = hspace >> void (eol <|> comment)

name :: Parser String
name = do
    x <- letterChar <|> char '_'
    xs <- many (letterChar <|> digitChar <|> char '_')
    return $ x : xs

comment :: Parser String
comment = do
    void $ single ';'
    manyTill anySingle eol

nothing :: (Monad m) => m a -> m (Maybe b)
nothing p = p >> return Nothing

label :: Parser String
label = do
    n <- name
    void $ single ':'
    return n

labelRef = name

reference :: (Read w) => Parser (Ref w)
reference =
    choice
        [ labelRef <&> Ref
        , hexNum <&> ValueR . read
        , num <&> ValueR . read
        ]
