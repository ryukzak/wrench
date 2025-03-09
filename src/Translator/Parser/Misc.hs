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
    referenceWithDirective,
    referenceWithFn,
    SectionItem (..),
    orgDirective,
    sectionItems,
    sectionOrg,
) where

import Data.Bits
import Data.Text qualified as T
import Relude
import Relude.Unsafe as Unsafe
import Text.Megaparsec (anySingle, anySingleBut, choice, manyTill, single, try)
import Text.Megaparsec.Char (
    char,
    digitChar,
    eol,
    hexDigitChar,
    hspace,
    hspace1,
    letterChar,
    string,
 )
import Translator.Parser.Types
import Translator.Types

data SectionItem i = Item i | Org Int

sectionItems :: [SectionItem i] -> [i]
sectionItems = mapMaybe $ \case
    Item i -> Just i
    Org _ -> Nothing

sectionOrg :: [SectionItem i] -> Maybe Int
sectionOrg items =
    let orgs =
            mapMaybe
                ( \case
                    Org i -> Just i
                    _ -> Nothing
                )
                items
     in listToMaybe orgs

orgDirective :: String -> Parser Int
orgDirective cstart = do
    void $ string ".org"
    hspace1
    value <- Unsafe.read <$> choice [hexNum, num]
    eol' cstart
    return value

removeUnderscores :: String -> String
removeUnderscores = toString . T.replace "_" "" . toText

num :: Parser String
num = do
    s <-
        choice
            [ char '-' >> many (digitChar <|> char '_') <&> (:) '-'
            , many (digitChar <|> char '_')
            ]
    return $ removeUnderscores s

hexNum :: Parser String
hexNum = do
    void $ string "0x"
    digits <- many (hexDigitChar <|> char '_')
    return $ "0x" <> removeUnderscores digits

eol' cstart = hspace >> void (eol <|> comment cstart)

name :: Parser String
name = do
    x <- letterChar <|> char '_'
    xs <- many (letterChar <|> digitChar <|> char '_')
    return $ x : xs

comment :: String -> Parser String
comment cstart = do
    void $ string cstart
    manyTill anySingle eol

nothing :: (Monad m) => m a -> m (Maybe b)
nothing p = p >> return Nothing

label :: Parser String
label = try $ do
    n <- name
    void $ single ':'
    return n

labelRef = name

referenceWithFn :: (Num w, Read w) => (w -> w) -> Parser (Ref w)
referenceWithFn f =
    choice
        [ do
            void quote
            c <- anySingleBut '\''
            void quote
            return $ ValueR f $ fromIntegral $ ord c
        , labelRef <&> Ref f
        , hexNum <&> ValueR f . read
        , num <&> ValueR f . read
        ]
    where
        quote = char '\''

referenceWithDirective :: (Bits w, Num w, Read w) => Parser (Ref w)
referenceWithDirective =
    choice
        [ do
            void $ string "%hi("
            ref <- referenceWithFn (\w -> (w `shiftR` 12) .&. 0xFFFFF)
            void $ string ")"
            return ref
        , do
            void $ string "%lo("
            ref <- referenceWithFn (.&. 0xFFF)
            void $ string ")"
            return ref
        , reference
        ]

reference :: (Num w, Read w) => Parser (Ref w)
reference = referenceWithFn id
