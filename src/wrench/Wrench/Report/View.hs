{- | Report views, the @{...}@ keys of a report template, as data: a
'Keyword' describing the key and the function that renders it. The shared
views live in "Wrench.Report", each ISA's own in 'Wrench.Machine.Types.views'.
Every key is matched, and its arguments checked, before the simulation runs.
-}
module Wrench.Report.View (
    Keyword (..),
    Segment (..),
    keyword,
    valueKey,
    flagKey,
    valueFormat,
    keywordUsage,
    keywordName,
    Match (..),
    isHex,
    CheckEnv (..),
    View (..),
    mkView,
    summary,
    mapEnv,
    matchView,
    checkView,
    viewsTable,
) where

import Data.List.NonEmpty qualified as NE
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
    deriving (Eq, Show)

newtype Keyword = Keyword [Segment]
    deriving (Eq, Show)

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

-- | The literal part of a keyword: @memory:table@, @memory@, @pc@.
keywordName :: Keyword -> Text
keywordName = T.intercalate ":" . literals

literals :: Keyword -> [Text]
literals (Keyword segments) = [t | Lit t <- takeWhile isLit segments]
    where
        isLit (Lit _) = True
        isLit _ = False

-- | A matched key: its arguments in order, and its format with the
-- default filled in (empty for a key without one).
data Match = Match
    { mArgs :: [Int]
    , mFormat :: Text
    }
    deriving (Eq, Show)

isHex :: Match -> Bool
isHex Match{mFormat} = mFormat == "hex"

-- | What a key's arguments are checked against before the run.
data CheckEnv = CheckEnv
    { ceMemorySize :: Int
    , ceIoPorts :: [Int]
    }

data View env = View
    { vKeyword :: Keyword
    , vDoc :: Text
    , vCheck :: CheckEnv -> Match -> Either Text ()
    , vRender :: env -> Match -> Text
    , vSummary :: Bool
    -- ^ Listed in @{isa-specific}@ as @name: value@.
    }

mkView :: Keyword -> Text -> (env -> Match -> Text) -> View env
mkView k doc render = View{vKeyword = k, vDoc = doc, vCheck = \_ _ -> Right (), vRender = render, vSummary = False}

-- | A run-wide stat of an ISA, also listed in @{isa-specific}@.
summary :: Text -> Text -> (env -> Text) -> View env
summary name doc render = (mkView (flagKey name) doc (const . render)){vSummary = True}

mapEnv :: (b -> a) -> View a -> View b
mapEnv f v@View{vRender} = v{vRender = vRender . f}

-- | Find the view a key names and read its arguments. The keyword with
-- the longest literal prefix wins, so @memory:table@ is not read as
-- @memory:\<from\>:\<to\>@.
matchView :: [View env] -> Text -> Either Text (View env, Match)
matchView views key =
    case groupByLength candidates of
        [] -> Left $ "unknown view {" <> key <> "}" <> suggestions (map vKeyword views) parts
        best : _ ->
            let results = [(v, match segments parts) | v@View{vKeyword = Keyword segments} <- best]
             in case [(v, m) | (v, Right m) <- results] of
                    found : _ -> Right found
                    [] ->
                        Left $
                            T.intercalate
                                "\n"
                                ["{" <> key <> "}: " <> err <> ", usage: " <> keywordUsage vKeyword | (View{vKeyword}, Left err) <- results]
    where
        parts = T.splitOn ":" key
        prefixLength = length . literals . vKeyword
        candidates =
            sortOn
                (Down . prefixLength)
                [v | v <- views, let ls = literals (vKeyword v), not (null ls), ls `isPrefixOf` parts]
        groupByLength vs = case vs of
            [] -> []
            v : _ -> [filter ((== prefixLength v) . prefixLength) vs]

-- | A Markdown table of views. Neighbours with the same description, such
-- as a register under its two names, share a row.
viewsTable :: [View env] -> Text
viewsTable vs =
    T.unlines $
        ["| View | Description |", "| --- | --- |"]
            <> map row (NE.groupBy ((==) `on` vDoc) vs)
    where
        row same@(View{vDoc, vSummary} :| _) =
            "| "
                <> T.intercalate ", " [code (keywordUsage vKeyword) | View{vKeyword} <- toList same]
                <> " | "
                <> vDoc
                <> (if vSummary then " Listed in `{isa-specific}`." else "")
                <> " |"
        code usage = "`" <> T.replace "|" "\\|" usage <> "`"

-- | Match a key and check its arguments.
checkView :: CheckEnv -> [View env] -> Text -> Either Text ()
checkView env views key = do
    (View{vKeyword, vCheck}, m) <- matchView views key
    first (\err -> "{" <> key <> "}: " <> err <> ", usage: " <> keywordUsage vKeyword) (vCheck env m)

match :: [Segment] -> [Text] -> Either Text Match
match segments parts = go segments parts (Match [] "")
    where
        go [] [] m@Match{mArgs} = Right m{mArgs = reverse mArgs}
        go [] extra _ = Left $ "unexpected '" <> T.intercalate ":" extra <> "'"
        go (Lit t : ss) (p : ps) m | t == p = go ss ps m
        go (Lit t : _) _ _ = Left $ "expected '" <> t <> "'"
        go (Arg a : _) [] _ = Left $ "missing <" <> a <> ">"
        go (Arg a : ss) (p : ps) m = number a p >>= \n -> go ss ps m{mArgs = n : mArgs m}
        go (OptArg _ : ss) [] m = go ss [] m
        go (OptArg a : ss) (p : ps) m = number a p >>= \n -> go ss ps m{mArgs = n : mArgs m}
        go (Fmt _ (Just f) : ss) [] m = go ss [] m{mFormat = f}
        go (Fmt fs Nothing : _) [] _ = Left $ "missing format, one of: " <> T.intercalate ", " fs
        go (Fmt fs _ : ss) (p : ps) m
            | p `elem` fs = go ss ps m{mFormat = p}
            | otherwise = Left $ "unknown format '" <> p <> "', one of: " <> T.intercalate ", " fs

number :: Text -> Text -> Either Text Int
number name p = case readMaybe (toString p) of
    Just n -> Right n
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
            | k <- keywords
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
