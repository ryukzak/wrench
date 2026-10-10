{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Wrench.Translator.Parser.Misc (
    literal,
    byteLiteral,
    intLiteral,
    wordLiteral,
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
import Text.Megaparsec (anySingle, anySingleBut, choice, getOffset, getSourcePos, manyTill, setOffset, single, try)
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
    value <- intLiteral
    eol' cstart
    return value

removeUnderscores :: String -> String
removeUnderscores = toString . T.replace "_" "" . toText

-- | A literal has to start with a digit, which the @_@ separators may then be
-- mixed into. Without that requirement both parsers also match the empty string,
-- and @read@ dies on it as a bare @Prelude.read: no parse@ with no position.
num :: Parser String
num = do
    s <-
        choice
            [ try $ char '-' >> decDigits <&> (:) '-'
            , decDigits
            ]
    return $ removeUnderscores s
    where
        decDigits = (:) <$> digitChar <*> many (digitChar <|> char '_')

hexNum :: Parser String
hexNum = try $ do
    void $ string "0x"
    ds <- (:) <$> hexDigitChar <*> many (hexDigitChar <|> char '_')
    return $ "0x" <> removeUnderscores ds

literal :: Parser Integer
literal = Unsafe.read <$> choice [hexNum, num]

wordLiteral :: forall w. (IsWord w) => Parser w
wordLiteral = anyPattern $ "a " <> show (finiteBitSize (zeroBits :: w)) <> "-bit machine word"

byteLiteral :: Parser Word8
byteLiteral = anyPattern "a byte"

intLiteral :: Parser Int
intLiteral = narrowed "an Int" (toInteger (minBound :: Int)) (toInteger (maxBound :: Int))

-- | Any pattern the type's own bits can spell, read as signed or unsigned.
anyPattern :: forall a. (FiniteBits a, Integral a) => Text -> Parser a
anyPattern what = narrowed what (negate (bit (width - 1))) (bit width - 1)
    where
        width = finiteBitSize (zeroBits :: a)

narrowed :: (Integral a) => Text -> Integer -> Integer -> Parser a
narrowed what lo hi = do
    literalPos <- getOffset
    value <- literal
    if lo <= value && value <= hi
        then return $ fromInteger value
        else do
            setOffset literalPos
            fail $
                concat
                    [ "literal "
                    , show value
                    , " doesn't fit "
                    , toString what
                    , ", expected "
                    , show lo
                    , ".."
                    , show hi
                    ]

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

referenceWithFn :: (IsWord w) => (w -> w) -> Parser (Ref w)
referenceWithFn f = do
    pos <- getSourcePos
    choice
        [ do
            void quote
            c <- anySingleBut '\''
            void quote
            return $ ValueR f pos $ fromIntegral $ ord c
        , Ref f pos <$> labelRef
        , ValueR f pos <$> wordLiteral
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

reference :: (IsWord w) => Parser (Ref w)
reference = referenceWithFn id
