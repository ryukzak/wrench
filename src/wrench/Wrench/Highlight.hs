{- | Line-by-line syntax highlighting for assembler sources and simulation
reports. Concatenating the token texts gives back the original line.
-}
module Wrench.Highlight (
    TokenKind (..),
    tokenizeLine,
    tokenizeReportLine,
) where

import Data.Char (isAlphaNum, isDigit, isHexDigit, isLower, isSpace, isUpper)
import Data.Text qualified as T
import Relude
import Wrench.Isa (Isa (..))
import Wrench.Isa.F32a (f32aMnemonics)
import Wrench.Isa.Wasm32 (wasm32Mnemonics)

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
    | Call
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
        tokens = classify isa (lexCode isa (toString code))
     in (if isa == F32a then f32aCalls tokens else tokens) <> [(Comment, comment) | not (T.null comment)]

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
                | isa == Wasm32, "$" `T.isPrefixOf` w -> (Register, w) : go False ls
                | Just known <- mnemonics isa -> (if w `elem` known then Mnemonic else Ref, w) : go False ls
                | statementStart -> (Mnemonic, w) : go False ls
                | isRegister isa w -> (Register, w) : go False ls
                | otherwise -> (Ref, w) : go False ls

mnemonics :: Isa -> Maybe [Text]
mnemonics F32a = Just f32aMnemonics
mnemonics Wasm32 = Just wasm32Mnemonics
mnemonics _ = Nothing

-- | A bare f32a word is a call unless it is the argument of a mnemonic.
f32aCalls :: [(TokenKind, Text)] -> [(TokenKind, Text)]
f32aCalls = go False
    where
        go _ [] = []
        go arg (tok@(kind, t) : ts) = case kind of
            Plain -> tok : go arg ts
            Ref | not arg -> (Call, t) : go False ts
            _ -> tok : go (kind == Mnemonic && t `elem` ["lit", "@p", "if", "-if", "next"]) ts

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

-- | Tokens for simulation log, memory dump and statistics lines.
tokenizeReportLine :: Isa -> Text -> [(TokenKind, Text)]
tokenizeReportLine isa line
    | "#" `T.isPrefixOf` line = [(Directive, line)]
    | T.strip line `elem` ["---", "```"] = [(Separator, line)]
    | Just (prefix, rest) <- linePrefix line =
        if isNumber (T.dropEnd 1 prefix) || "mem[" `T.isPrefixOf` prefix
            then (Separator, prefix) : addressed rest
            else (Ref, prefix) : reportBody isa (toString rest)
    | otherwise = reportBody isa (toString line)
    where
        addressed rest = case T.words rest of
            [name]
                | not (isNumber name)
                , maybe False ((\c -> isLower c || c == '_') . fst) (T.uncons name) ->
                    let (sp, w) = T.span isSpace rest in [(Plain, sp), (Label, w)]
            _ -> reportBody isa (toString rest)

linePrefix :: Text -> Maybe (Text, Text)
linePrefix line =
    let (before, after) = T.breakOn ": " (T.replace ":\t" ": " line)
        prefix = T.take (T.length before + 1) line
     in if T.null after || T.null before || (T.any isSpace before && not ("mem[" `T.isPrefixOf` before))
            then Nothing
            else Just (prefix, T.drop (T.length prefix) line)

reportBody :: Isa -> String -> [(TokenKind, Text)]
reportBody _ [] = []
reportBody isa s@(x : xs)
    | isSpace x = let (sp, rest) = span isSpace s in (Plain, toText sp) : reportBody isa rest
    | x == '"' = let (str, rest) = quoted x xs in (Literal, toText (x : str)) : reportBody isa rest
    | x == '@', '"' : ys <- xs = let (str, rest) = quoted '"' ys in (Label, toText ('@' : '"' : str)) : reportBody isa rest
    | x == '@', (w@(_ : _), rest) <- span isWordChar xs = (Label, toText (x : w)) : reportBody isa rest
    | isAlphaNum x || (x == '-' && maybe False isDigit (viaNonEmpty head xs)) =
        let (w, rest) = span (\c -> isAlphaNum c || c == '_') xs
         in (reportWord isa (toText (x : w)), toText (x : w)) : reportBody isa rest
    | otherwise = (Plain, one x) : reportBody isa xs

reportWord :: Isa -> Text -> TokenKind
reportWord isa w
    | isNumber w || (T.length w == 2 && T.all (\c -> isHexDigit c && not (isUpper c)) w) = Literal
    | w == "B" = Plain
    | maybe False (isUpper . fst) (T.uncons w) =
        if isRegister isa w || isRegister isa (T.toLower w) || (isa `elem` [RiscIv, VliwIv] && w == "S0Fp")
            then Register
            else Mnemonic
    | otherwise = Plain
