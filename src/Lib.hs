{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib (
    someFunc,
) where

import Data.Functor
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, choice, eof, many, manyTill, oneOf, parse, parseTest, single, try, (<?>), (<|>))
import Text.Megaparsec.Char (alphaNumChar, digitChar, eol, hspace, hspace1, letterChar, newline, space, string, tab)
import Text.Megaparsec.Debug (dbg')
import Text.Megaparsec.Error (errorBundlePretty)

data Section l m
    = Code l [Token l m]
    | Data l [DataToken l]
    deriving (Show)

------------------------------------------------------------
-- Data section

data DataToken l = Var
    { dataLabel :: !l
    , dataType :: !l
    , dataValue :: !l
    }
    deriving (Show)

dataSection :: String -> Parser (Section String m)
dataSection secLabel = do
    sectionHeader secLabel
    Data secLabel <$> body
  where
    body = catMaybes <$> many (try dataSectionItemM <|> insignificantM)

------------------------------------------------------------
-- Code section

data Token l m = Label l | Comment l | Empty | Mnemonic m
    deriving (Show)

------------------------------------------------------------
-- Utils

type Parser = Parsec Void String

codeSection :: forall m. (Mnemonic m) => String -> Parser (Section String m)
codeSection secLabel = do
    try $ sectionHeader secLabel
    Code secLabel <$> textSectionBody

insignificant :: Parser ()
insignificant = do
    hspace
    void $ comment <|> eol
  where
    comment = hspace >> single ';' >> manyTill anySingle eol

insignificantM :: Parser (Maybe a)
insignificantM = insignificant >> return Nothing

------

dataSectionItemM :: Parser (Maybe (DataToken String))
dataSectionItemM = do
    hspace
    n <- name
    hspace1
    type_ <- name
    hspace1
    values <- manyTill anySingle (hspace1 <|> void eol)
    hspace
    eol <|> comment
    return $ Just $ Var n type_ values

sectionHeader :: String -> Parser ()
sectionHeader secLabel =
    do
        void $ string ".section"
        hspace
        void $ string secLabel
        void $ insignificant

sectionBegin = void $ do
    hspace
    string ".section"

textSectionBody :: (Mnemonic m) => Parser [Token String m]
textSectionBody =
    endOfFile
        <|> try endOfSection
        <|> do
            hspace
            t <- emptyLine <|> (comment <&> Comment) <|> label <|> mnemonic
            space
            ts <- textSectionBody
            return $ t : ts
  where
    endOfFile = eof >> return []
    endOfSection = sectionBegin >> return []

emptyLine :: Parser (Token String m)
emptyLine = try $ do
    hspace
    void eol
    return Empty

comment = try $ do
    hspace
    void $ single ';'
    space
    text <- manyTill anySingle eol
    return text

label = try $ do
    n <- name
    void $ single ':'
    return $ Label n

name :: Parser String
name = do
    x <- letterChar
    xs <- many (letterChar <|> digitChar)
    return $ x : xs

class Mnemonic m where
    mnemonic :: Parser (Token String m)

instance Mnemonic String where
    mnemonic = do
        m <-
            manyTill
                anySingle
                (void eol <|> void comment)
        return $ Mnemonic m

pretty (Code _ ts) = "Code section:" : map (("    " ++) . show) ts
pretty (Data _ ts) = "Data section:" : map (("    " ++) . show) ts

someFunc fn src =
    let
        p :: Parser [Section String String]
        p = dbg' "COR" $ do
            secs <-
                catMaybes
                    <$> many
                        ( (try (dataSection ".data") <&> Just)
                            <|> try (codeSection @String ".text" <&> Just)
                            <|> insignificantM
                        )
            eof
            return secs
     in
        case parse p fn src of
            Right sections -> concatMap pretty sections
            Left err -> [errorBundlePretty err]
