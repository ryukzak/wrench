{- | Line-by-line syntax highlighting for assembler sources. Concatenating the
token texts gives back the original line.
-}
module Wrench.Highlight (
    TokenKind (..),
    tokenizeLine,
) where

import Data.Char (isAlphaNum, isDigit, isHexDigit, isSpace)
import Data.Text qualified as T
import Relude
import Wrench.Isa (Isa (..))

data TokenKind
    = Plain
    | Comment
    | Label
    | Directive
    | Mnemonic
    | Register
    | Literal
    | Reloc
    | Ref
    | Separator
    deriving (Eq, Show)

data Lexeme
    = LSpace Text
    | LString Text
    | LLabel Text
    | LWord Text
    | LPunct Char

tokenizeLine :: Isa -> Text -> [(TokenKind, Text)]
tokenizeLine isa line =
    let (code, comment) = splitComment (commentChar isa) line
     in classify isa (lexCode isa (toString code)) <> [(Comment, comment) | not (T.null comment)]

commentChar :: Isa -> Char
commentChar F32a = '\\'
commentChar _ = ';'

splitComment :: Char -> Text -> (Text, Text)
splitComment c line = T.splitAt (go 0 Nothing (toString line)) line
    where
        go :: Int -> Maybe Char -> String -> Int
        go i _ [] = i
        go i Nothing (x : xs)
            | x == c = i
            | isQuote x = go (i + 1) (Just x) xs
            | otherwise = go (i + 1) Nothing xs
        go i (Just q) (x : xs)
            | x == '\\', _ : xs' <- xs = go (i + 2) (Just q) xs'
            | x == q = go (i + 1) Nothing xs
            | otherwise = go (i + 1) (Just q) xs

isQuote :: Char -> Bool
isQuote x = x == '\'' || x == '"'

lexCode :: Isa -> String -> [Lexeme]
lexCode _ [] = []
lexCode isa s@(x : xs)
    | isSpace x =
        let (sp, rest) = span isSpace s
         in LSpace (toText sp) : lexCode isa rest
    | isQuote x =
        let (str, rest) = quoted x xs
         in LString (toText (x : str)) : lexCode isa rest
    | isa == F32a =
        let (w, rest) = break isSpace s
         in wordOrLabel w : lexCode isa rest
    | isWordChar x || (x == '-' && maybe False isDigit (viaNonEmpty head xs)) =
        let (w, rest) = span isWordChar xs
         in case rest of
                ':' : rest' -> LLabel (toText (x : w <> ":")) : lexCode isa rest'
                _ -> LWord (toText (x : w)) : lexCode isa rest
    | otherwise = LPunct x : lexCode isa xs
    where
        wordOrLabel w
            | length w > 1, Just ':' <- viaNonEmpty last w = LLabel (toText w)
            | otherwise = LWord (toText w)

quoted :: Char -> String -> (String, String)
quoted _ [] = ([], [])
quoted q ('\\' : e : rest) = first (['\\', e] <>) (quoted q rest)
quoted q (c : rest)
    | c == q = ([c], rest)
    | otherwise = first (c :) (quoted q rest)

isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c `elem` ("_.%$" :: String)

classify :: Isa -> [Lexeme] -> [(TokenKind, Text)]
classify isa = go True
    where
        go _ [] = []
        go statementStart (l : ls) = case l of
            LSpace t -> (Plain, t) : go statementStart ls
            LString t -> (Literal, t) : go False ls
            LLabel t -> (Label, t) : go statementStart ls
            LPunct '/' | isa == VliwIv -> (Separator, "/") : go True ls
            LPunct c -> (Plain, one c) : go statementStart ls
            LWord w
                | isNumber w -> (Literal, w) : go False ls
                | "." `T.isPrefixOf` w -> (Directive, w) : go False ls
                | "%" `T.isPrefixOf` w -> (Reloc, w) : go False ls
                | isa == F32a || statementStart -> (Mnemonic, w) : go False ls
                | isRegister isa w -> (Register, w) : go False ls
                | otherwise -> (Ref, w) : go False ls

isNumber :: Text -> Bool
isNumber w =
    let unsigned = fromMaybe w (T.stripPrefix "-" w)
     in case T.stripPrefix "0x" unsigned of
            Just ds -> startsWith isHexDigit ds && T.all (\c -> isHexDigit c || c == '_') ds
            Nothing -> startsWith isDigit unsigned && T.all (\c -> isDigit c || c == '_') unsigned
    where
        startsWith p t = maybe False (p . fst) (T.uncons t)

isRegister :: Isa -> Text -> Bool
isRegister isa w = case isa of
    RiscIv -> w `elem` riscRegisters
    VliwIv -> w `elem` riscRegisters
    M68k -> case toString w of
        [r, n] -> r `elem` ("AD" :: String) && n `elem` ['0' .. '7']
        _ -> False
    _ -> False

riscRegisters :: [Text]
riscRegisters =
    ["zero", "ra", "sp", "gp", "tp", "fp"]
        <> numbered "t" 6
        <> numbered "s" 11
        <> numbered "a" 7
    where
        numbered p n = map ((p <>) . show) [0 .. n :: Int]
