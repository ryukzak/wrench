{-# LANGUAGE GADTs #-}

{- | Report views, the @{...}@ keys of a report template, as data. A view's
'Spec' says how its key is spelled and what it reads (numbers, a format);
parsing a key against it yields that value, typed, and the renderer takes
it. Usage text, the docs table and key matching all read the same 'Spec'.

The shared views live in "Wrench.Report", each ISA's own in
'Wrench.Machine.Types.views'. A report template is compiled against them
once, before the simulation runs, so rendering cannot meet a bad key.
-}
module Wrench.Report.View (
    -- * Key specs
    Spec,
    Format (..),
    lit,
    arg,
    optArg,
    format,
    formatRequired,
    usage,
    specName,

    -- * Views
    CheckEnv (..),
    View (..),
    mkView,
    checkedView,
    withRenames,
    contramapView,
    viewUsage,
    viewName,
    viewsTable,
    renamesTable,
    Stat (..),
    statView,

    -- * Templates
    Template,
    compileTemplate,
    templateViews,
    renderTemplate,
    examples,
) where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Relude

data Format = Dec | Hex | Sym | Bin
    deriving (Bounded, Enum, Eq, Show)

formatName :: Format -> Text
formatName = T.toLower . show

-- | One colon-separated part of a key.
data Piece a where
    PLit :: Text -> Piece ()
    -- | A number, decimal or @0x@ hex, named for messages.
    PArg :: Text -> Piece Int
    POptArg :: Text -> Piece (Maybe Int)
    -- | Allowed formats and the default; 'Nothing' makes it required.
    PFormat :: NonEmpty Format -> Maybe Format -> Piece Format

-- | How a key is spelled, and what it reads: a free applicative over its
-- parts, so it can be both parsed and inspected (for usage and docs).
data Spec a where
    Done :: a -> Spec a
    Step :: Piece b -> Spec (b -> a) -> Spec a

instance Functor Spec where
    fmap f (Done a) = Done (f a)
    fmap f (Step p rest) = Step p (fmap (f .) rest)

instance Applicative Spec where
    pure = Done
    Done f <*> x = fmap f x
    Step p rest <*> x = Step p (flip <$> rest <*> x)

-- | Literal parts: @lit "mem:instr-ranges"@ is two of them.
lit :: Text -> Spec ()
lit name = traverse_ (\t -> Step (PLit t) (Done id)) (T.splitOn ":" name)

arg :: Text -> Spec Int
arg name = Step (PArg name) (Done id)

optArg :: Text -> Spec (Maybe Int)
optArg name = Step (POptArg name) (Done id)

format :: [Format] -> Format -> Spec Format
format fs def = Step (PFormat (nonEmptyFormats fs) (Just def)) (Done id)

formatRequired :: [Format] -> Spec Format
formatRequired fs = Step (PFormat (nonEmptyFormats fs) Nothing) (Done id)

nonEmptyFormats :: [Format] -> NonEmpty Format
nonEmptyFormats = fromMaybe (error "a format piece needs at least one format") . nonEmpty

data SomePiece = forall a. SomePiece (Piece a)

pieces :: Spec a -> [SomePiece]
pieces (Done _) = []
pieces (Step p rest) = SomePiece p : pieces rest

-- | The literal part of a key: @mem:table@, @mem:dump@, @pc@.
specName :: Spec a -> Text
specName = T.intercalate ":" . literals

literals :: Spec a -> [Text]
literals spec = [t | SomePiece (PLit t) <- takeWhile isLit (pieces spec)]
    where
        isLit (SomePiece (PLit _)) = True
        isLit _ = False

usage :: Spec a -> Text
usage spec = "{" <> T.concat (zipWith render [0 :: Int ..] (pieces spec)) <> "}"
    where
        sep n = if n == 0 then "" else ":"
        render :: Int -> SomePiece -> Text
        render n (SomePiece p) = case p of
            PLit t -> sep n <> t
            PArg a -> sep n <> "<" <> a <> ">"
            POptArg a -> "[" <> sep n <> "<" <> a <> ">]"
            PFormat fs Nothing -> sep n <> formats fs
            PFormat fs (Just _) -> "[" <> sep n <> formats fs <> "]"
        formats :: NonEmpty Format -> Text
        formats = T.intercalate "|" . map formatName . toList

-- | Read a key's parts, left to right.
parseSpec :: Spec a -> [Text] -> Either Text a
parseSpec (Done a) [] = Right a
parseSpec (Done _) extra = Left $ "unexpected '" <> T.intercalate ":" extra <> "'"
parseSpec (Step p rest) parts = case parsePiece p parts of
    Right (b, parts') -> ($ b) <$> parseSpec rest parts'
    -- An optional format that does not match may be left out: the part is
    -- then the next piece's (@stack:layout:3@). If that fails too, the format
    -- error is the one to report.
    Left err
        | PFormat _ (Just def) <- p
        , not (null (pieces rest)) ->
            first (const err) (($ def) <$> parseSpec rest parts)
        | otherwise -> Left err

parsePiece :: Piece a -> [Text] -> Either Text (a, [Text])
parsePiece p parts = case (p, parts) of
    (PLit t, x : xs) | t == x -> Right ((), xs)
    (PLit t, _) -> Left $ "expected '" <> t <> "'"
    (PArg a, []) -> Left $ "missing <" <> a <> ">"
    (PArg a, x : xs) -> (,xs) <$> number a x
    (POptArg _, []) -> Right (Nothing, [])
    (POptArg a, x : xs) -> (,xs) . Just <$> number a x
    (PFormat _ (Just f), []) -> Right (f, [])
    (PFormat fs Nothing, []) -> Left $ "missing format, one of: " <> T.intercalate ", " (map formatName (toList fs))
    (PFormat fs _, x : xs) -> case find ((== x) . formatName) fs of
        Just f -> Right (f, xs)
        Nothing -> Left $ "unknown format '" <> x <> "', one of: " <> T.intercalate ", " (map formatName (toList fs))
    where
        number name x = maybeToRight ("<" <> name <> "> should be a number, got '" <> x <> "'") (readMaybe (toString x))

-- | What a key's arguments are checked against: the configuration.
data CheckEnv = CheckEnv
    { ceMemorySize :: Int
    , ceIoPorts :: [Int]
    }

data View env = forall a. View
    { vSpec :: Spec a
    , vDoc :: Text
    , vCheck :: CheckEnv -> a -> Either Text ()
    , vRender :: env -> a -> Text
    , vRenamedTo :: Maybe [Text]
    }

mkView :: Spec a -> Text -> (env -> a -> Text) -> View env
mkView spec doc = checkedView spec doc (\_ _ -> Right ())

-- | A view whose arguments must also fit the configuration.
checkedView :: Spec a -> Text -> (CheckEnv -> a -> Either Text ()) -> (env -> a -> Text) -> View env
checkedView spec doc check render = View spec doc check render Nothing

withRenames :: [(Text, Text)] -> [View env] -> [View env]
withRenames renames vs = vs <> map alias renames
    where
        alias (old, new) = case find ((== new) . viewName) vs of
            Just (View spec doc check render _) ->
                View (lit old *> dropLiterals spec) doc check render (Just (literals spec))
            Nothing -> error $ "renamed view not found: " <> new

dropLiterals :: Spec a -> Spec a
dropLiterals (Step (PLit _) rest) = dropLiterals (fmap ($ ()) rest)
dropLiterals s = s

-- | The same view over a larger environment.
contramapView :: (b -> a) -> View a -> View b
contramapView f (View spec doc check render renamedTo) = View spec doc check (render . f) renamedTo

viewUsage :: View env -> Text
viewUsage View{vSpec} = usage vSpec

viewName :: View env -> Text
viewName View{vSpec} = specName vSpec

-- | Find the view a key names, read and check its arguments, and close
-- over them. The view with the longest literal prefix wins, so
-- @stack:layout@ is not read as @stack@ with an unknown format.
matchView :: CheckEnv -> [View env] -> Text -> Either Text (Maybe Text, Text, env -> Text)
matchView checkEnv views key =
    case groupByLength candidates of
        [] -> Left $ "unknown view {" <> key <> "}" <> suggestions views parts
        best : _ ->
            let results = map try best
             in case rights results of
                    hole : _ -> Right hole
                    [] -> Left $ T.intercalate "\n" (lefts results)
    where
        parts = T.splitOn ":" key
        prefixLength View{vSpec} = length (literals vSpec)
        candidates =
            sortOn
                (Down . prefixLength)
                [v | v@View{vSpec} <- views, let ls = literals vSpec, not (null ls), ls `isPrefixOf` parts]
        groupByLength vs = case vs of
            [] -> []
            v : _ -> [filter ((== prefixLength v) . prefixLength) vs]
        try View{vSpec, vCheck, vRender, vRenamedTo} =
            first (\err -> "{" <> key <> "}: " <> err <> ", usage: " <> usage vSpec) $ do
                a <- parseSpec vSpec parts
                vCheck checkEnv a
                return
                    ( renamedWarning (length (literals vSpec)) <$> vRenamedTo
                    , T.intercalate ":" (fromMaybe (literals vSpec) vRenamedTo)
                    , (`vRender` a)
                    )
        renamedWarning n new =
            "{" <> key <> "} is renamed, use {" <> T.intercalate ":" (new <> drop n parts) <> "}"

-- | Views whose name is a small edit away from the key's leading parts,
-- compared case-insensitively.
suggestions :: [View env] -> [Text] -> Text
suggestions views parts =
    case take 3 $ map snd $ sortOn fst close of
        [] -> ""
        us -> ", did you mean " <> T.intercalate " or " us <> "?"
    where
        close =
            [ (d, usage vSpec)
            | View{vSpec, vRenamedTo = Nothing} <- views
            , let ls = literals vSpec
                  name = T.intercalate ":" ls
                  given = T.intercalate ":" (take (length ls) parts)
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

-- | Keys that name this view, one per spelling: every format, with and
-- without optional parts, @0@ for each number.
examples :: View env -> [Text]
examples View{vSpec} = map (T.intercalate ":") (go (pieces vSpec))
    where
        go :: [SomePiece] -> [[Text]]
        go [] = [[]]
        go (SomePiece p : ps) = [x <> rest | x <- spellings p, rest <- go ps]
        spellings :: Piece a -> [[Text]]
        spellings p = case p of
            PLit t -> [[t]]
            PArg _ -> [["0"]]
            POptArg _ -> [[], ["0"]]
            PFormat fs d -> [[] | isJust d] <> [[formatName f] | f <- toList fs]

-- | A run-wide stat of an ISA: a view of its own and a line of @{isa-specific}@.
data Stat env = Stat
    { statName :: Text
    , statDoc :: Text
    , statRender :: env -> Text
    }

statView :: Stat env -> View env
statView Stat{statName, statDoc, statRender} =
    mkView (lit statName) (statDoc <> " Listed in `{isa-specific}`.") (\env () -> statRender env)

-- | A Markdown table of the current views. Neighbours with the same
-- description, such as a register under its two names, share a row.
viewsTable :: [View env] -> Text
viewsTable vs =
    markdownTable
        ("View", "Description")
        [ (T.intercalate ", " [code (viewUsage v) | v <- toList same], vDoc)
        | same@(View{vDoc} :| _) <- NE.groupBy ((==) `on` vDoc) [v | v@View{vRenamedTo = Nothing} <- vs]
        ]

renamesTable :: [View env] -> Text
renamesTable vs =
    case [ (usage vSpec, usage (lit (T.intercalate ":" new) *> dropLiterals vSpec))
         | View{vSpec, vRenamedTo = Just new} <- vs
         ] of
        [] -> ""
        rows -> markdownTable ("Old", "New") (map (bimap code code) rows)

markdownTable :: (Text, Text) -> [(Text, Text)] -> Text
markdownTable header rows =
    T.unlines $ line header : line (T.replicate wA "-", T.replicate wB "-") : map line rows
    where
        wA = foldr (max . T.length . fst) 0 (header : rows)
        wB = foldr (max . T.length . snd) 0 (header : rows)
        line (a, b) = "| " <> T.justifyLeft wA ' ' a <> " | " <> T.justifyLeft wB ' ' b <> " |"

code :: Text -> Text
code u = "`" <> T.replace "|" "\\|" u <> "`"

-- | A report template with every key already matched to its view.
newtype Template env = Template [Chunk env]

-- | A hole carries the current name of its view.
data Chunk env = Literal Text | Hole Text (env -> Text)

-- | Split a template into text and @{key}@ holes, matching every key.
compileTemplate :: CheckEnv -> [View env] -> Text -> Either [Text] ([Text], Template env)
compileTemplate checkEnv views input =
    case partitionEithers (map compile (chunks input)) of
        ([], cs) -> Right (mapMaybe fst cs, Template (map snd cs))
        (errs, _) -> Left errs
    where
        compile (Left t) = Right (Nothing, Literal t)
        compile (Right key) = (\(warning, name, render) -> (warning, Hole name render)) <$> matchView checkEnv views key
        -- A '{' without a later '}' is text, as is everything outside braces.
        chunks t
            | T.null t = []
            | otherwise =
                let (before, rest) = T.breakOn "{" t
                    (key, rest') = T.breakOn "}" (T.drop 1 rest)
                 in if T.null rest || T.null rest'
                        then [Left t]
                        else [Left before | not (T.null before)] <> (Right key : chunks (T.drop 1 rest'))

renderTemplate :: Template env -> env -> Text
renderTemplate (Template cs) env = T.concat (map render cs)
    where
        render (Literal t) = t
        render (Hole _ f) = f env

-- | The views a template uses, by their current names.
templateViews :: Template env -> [Text]
templateViews (Template cs) = [name | Hole name _ <- cs]
