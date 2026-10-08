{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Wrench.Translator.Parser.Misc (
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
import Text.Megaparsec (anySingle, anySingleBut, choice, getSourcePos, manyTill, single, try)
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
import Wrench.Machine.Types (IsWord)
import Wrench.Machine.Word (fitSigned)
import Wrench.Translator.Parser.Types
import Wrench.Translator.Types

data SectionItem i = Item i | Org Int

sectionItems :: [SectionItem i] -> [i]
sectionItems = mapMaybe $ \case
    Item i -> Just i
    Org _ -> Nothing

sectionOrg :: [SectionItem i] -> Maybe Int
sectionOrg items =
    case [i | Org i <- items] of
        [] -> Nothing
        [x] -> Just x
        (_ : _ : _) -> error "error: multiple .org directives not allowed"

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

name :: Parser Text
name = do
    x <- letterChar <|> char '_'
    xs <- many (letterChar <|> digitChar <|> char '_')
    return $ toText (x : xs)

comment :: String -> Parser String
comment cstart = do
    void $ string cstart
    manyTill anySingle eol

nothing :: (Monad m) => m a -> m (Maybe b)
nothing p = p >> return Nothing

label :: Parser Text
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
        , do
            pos <- getSourcePos
            l <- labelRef
            return $ Ref f l pos
        , hexNum <&> ValueR f . read
        , num <&> ValueR f . read
        ]
    where
        quote = char '\''

-- | A reference that may be wrapped in a @%hi@ \/ @%lo@ relocation directive.
--
-- The two directives are designed to be used as a pair:
--
-- > lui  rd, %hi(x)
-- > addi rd, rd, %lo(x)
--
-- @%lo@ keeps the low 12 bits and sign-extends them from bit 11, because that is
-- what the instructions consuming the field do with it (see 'fitSigned'):
-- @%lo(0xFFFFFFFF)@ is @-1@, not @0xFFF@. To compensate for that borrow, @%hi@
-- rounds the value up by half a field (@+0x800@) before taking its upper 20 bits,
-- so the pair reconstructs @x@ exactly for every 32-bit @x@.
referenceWithDirective :: (IsWord w) => Parser (Ref w)
referenceWithDirective =
    choice
        [ do
            void $ string "%hi("
            ref <- referenceWithFn (\w -> ((w + 0x800) `shiftR` 12) .&. 0xFFFFF)
            void $ string ")"
            return ref
        , do
            void $ string "%lo("
            ref <- referenceWithFn (fitSigned 12)
            void $ string ")"
            return ref
        , reference
        ]

reference :: (Num w, Read w) => Parser (Ref w)
reference = referenceWithFn id
