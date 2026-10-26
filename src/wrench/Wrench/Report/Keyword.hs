{- | The keywords a report view may use inside @{...}@, as data: the
shared ones in "Wrench.Report", each ISA's own from 'viewKeywords'.
A key is checked against them before the simulation runs, and
normalized (default format filled in) before it is resolved.
-}
module Wrench.Report.Keyword (
    Keyword (..),
    Segment (..),
    keyword,
    valueKey,
    flagKey,
    valueFormat,
    keywordUsage,
    normalizeKey,
) where

import Data.Text qualified as T
import Relude

data Segment
    = Lit Text
    | -- | A required number (decimal or @0x@ hex), named for messages.
      Arg Text
    | -- | An optional trailing number.
      OptArg Text
    | -- | Allowed formats and the default; 'Nothing' makes it required.
      Fmt [Text] (Maybe Text)
    deriving (Eq, Ord, Show)

newtype Keyword = Keyword [Segment]
    deriving (Eq, Ord, Show)

-- | @keyword "mem:instr-ranges" rest@: literal segments, then @rest@.
keyword :: Text -> [Segment] -> Keyword
keyword name rest = Keyword (map Lit (T.splitOn ":" name) <> rest)

valueKey :: Text -> Keyword
valueKey name = keyword name [valueFormat]

flagKey :: Text -> Keyword
flagKey name = keyword name []

valueFormat :: Segment
valueFormat = Fmt ["dec", "hex"] (Just "dec")

keywordUsage :: Keyword -> Text
keywordUsage (Keyword segments) = "{" <> T.concat (zipWith render [0 :: Int ..] segments) <> "}"
    where
        sep n = if n == 0 then "" else ":"
        render n (Lit t) = sep n <> t
        render n (Arg a) = sep n <> "<" <> a <> ">"
        render n (OptArg a) = "[" <> sep n <> "<" <> a <> ">]"
        render n (Fmt fs Nothing) = sep n <> T.intercalate "|" fs
        render n (Fmt fs (Just _)) = "[" <> sep n <> T.intercalate "|" fs <> "]"

literals :: Keyword -> [Text]
literals (Keyword segments) = [t | Lit t <- takeWhile isLit segments]
    where
        isLit (Lit _) = True
        isLit _ = False

-- | Check a key and fill in its default format: @pc@ -> @pc:dec@. The
-- keyword with the longest literal prefix wins, so @memory:table@ is not
-- read as @memory:\<a\>:\<b\>@.
normalizeKey :: [Keyword] -> Text -> Either Text Text
normalizeKey keywords key =
    case groupByLength candidates of
        [] -> Left $ "unknown view {" <> key <> "}" <> suggestions keywords parts
        best : _ ->
            let results = [(k, T.intercalate ":" <$> match segments parts) | k@(Keyword segments) <- best]
             in case [normalized | (_, Right normalized) <- results] of
                    normalized : _ -> Right normalized
                    [] ->
                        Left $
                            T.intercalate
                                "\n"
                                ["{" <> key <> "}: " <> err <> ", usage: " <> keywordUsage k | (k, Left err) <- results]
    where
        parts = T.splitOn ":" key
        candidates =
            sortOn
                (Down . length . literals)
                [k | k <- keywords, let ls = literals k, not (null ls), ls `isPrefixOf` parts]
        groupByLength ks = case ks of
            [] -> []
            k : _ -> [filter ((== length (literals k)) . length . literals) ks]

match :: [Segment] -> [Text] -> Either Text [Text]
match [] [] = Right []
match [] extra = Left $ "unexpected '" <> T.intercalate ":" extra <> "'"
match (Lit t : ss) (p : ps) | t == p = (p :) <$> match ss ps
match (Lit t : _) _ = Left $ "expected '" <> t <> "'"
match (Arg a : _) [] = Left $ "missing <" <> a <> ">"
match (Arg a : ss) (p : ps) = number a p *> ((p :) <$> match ss ps)
match (OptArg _ : ss) [] = match ss []
match (OptArg a : ss) (p : ps) = number a p *> ((p :) <$> match ss ps)
match (Fmt _ (Just f) : ss) [] = (f :) <$> match ss []
match (Fmt fs Nothing : _) [] = Left $ "missing format, one of: " <> T.intercalate ", " fs
match (Fmt fs _ : ss) (p : ps)
    | p `elem` fs = (p :) <$> match ss ps
    | otherwise = Left $ "unknown format '" <> p <> "', one of: " <> T.intercalate ", " fs

number :: Text -> Text -> Either Text ()
number name p = case readMaybe @Int (toString p) of
    Just _ -> Right ()
    Nothing -> Left $ "<" <> name <> "> should be a number, got '" <> p <> "'"

-- | Keywords whose name is a small edit away from the key's leading
-- parts, compared case-insensitively.
suggestions :: [Keyword] -> [Text] -> Text
suggestions keywords parts =
    case take 3 $ map snd $ sortOn fst close of
        [] -> ""
        ks -> ", did you mean " <> T.intercalate " or " (map keywordUsage ks) <> "?"
    where
        close =
            [ (d, k)
            | k <- ordNub keywords
            , let name = T.intercalate ":" (literals k)
                  given = T.intercalate ":" (take (length (literals k)) parts)
                  d = editDistance (T.toLower name) (T.toLower given)
            , d <= max 1 (T.length name `div` 3)
            ]

editDistance :: Text -> Text -> Int
editDistance a b = fromMaybe 0 $ viaNonEmpty last $ foldl' step [0 .. T.length a] (toString b)
    where
        step prev@(p : ps) c =
            let row = scanl compute (p + 1) (zip3 (toString a) prev ps)
                compute left (ca, diag, up) = min (min (left + 1) (up + 1)) (diag + if ca == c then 0 else 1)
             in row
        step [] _ = []
