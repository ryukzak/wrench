{- |
@.frame@: names for locals, replaced by their indices before the program is
parsed.
-}
module Wrench.Isa.Wasm32.LocalNames (
    resolveLocalNames,
) where

import Data.Char (isAlphaNum, isSpace)
import Data.List qualified as L
import Relude
import Relude.Extra (lookup)
import Wrench.Translator.Types (Preprocessor)

-- | In @.text@, from each label to the next one or the section's end, blank
-- the @.frame ($a, $b) $c@ line and write each name it lists as its index,
-- padded to the name's width. A name the @.frame@ does not list stays in
-- the source for the parser to reject.
resolveLocalNames :: Preprocessor
resolveLocalNames src = intercalate "\n" <$> go 0 False mempty (splitOn '\n' src)
    where
        go _ _ _ [] = Right []
        go offset inText names (line : rest) = case L.words (code line) of
            ".text" : _ -> (line :) <$> next True mempty
            ".data" : _ -> (line :) <$> next False mempty
            w : _ | inText, ":" `L.isSuffixOf` w -> (line :) <$> next inText mempty
            _
                | not inText -> (line :) <$> next inText names
                | Just afterDirective <- frameDirective line -> case frameNames afterDirective of
                    Nothing ->
                        Left (offset + length (takeWhile isSpace line), "malformed .frame, expected distinct names as in .frame ($a, $b) $c")
                    Just declared -> (map (const ' ') line :) <$> next inText (fromList (zip declared [0 ..]))
                | otherwise -> (substitute names line :) <$> next inText names
            where
                next inText' names' = go (offset + length line + 1) inText' names' rest

-- | What follows @.frame@ on the line, if the line is one.
frameDirective :: String -> Maybe String
frameDirective line = case L.stripPrefix ".frame" (dropWhile isSpace (code line)) of
    Just s@(c : _) | isSpace c || c == '(' -> Just s
    Just [] -> Just []
    _ -> Nothing

-- | The names of @($a, $b) $c@: the parameters, then the reserved locals.
frameNames :: String -> Maybe [String]
frameNames s = do
    '(' : inside <- Just (dropWhile isSpace s)
    (params, _ : locals) <- Just (break (== ')') inside)
    declared <- (<>) <$> nameList params <*> nameList locals
    guard $ length (ordNub declared) == length declared
    return declared
    where
        nameList t
            | all isSpace t = Just []
            | otherwise = mapM name (splitOn ',' t)
        name t = case L.dropWhileEnd isSpace (dropWhile isSpace t) of
            n@('$' : c : cs) | all isNameChar (c : cs) -> Just n
            _ -> Nothing

substitute :: Map String Int -> String -> String
substitute names = \case
    '$' : rest
        | (name, rest') <- span isNameChar rest
        , Just idx <- lookup ('$' : name) names ->
            let index = show idx in index <> replicate (length name + 1 - length index) ' ' <> substitute names rest'
    c : rest -> c : substitute names rest
    [] -> []

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '_'

code :: String -> String
code = takeWhile (/= ';')

splitOn :: Char -> String -> [String]
splitOn sep s = case break (== sep) s of
    (chunk, []) -> [chunk]
    (chunk, _ : rest) -> chunk : splitOn sep rest
