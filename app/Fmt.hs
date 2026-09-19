{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Default
import Data.Text qualified as T
import Options.Applicative
import Relude
import Relude.Unsafe qualified as Unsafe
import Wrench.Wrench (Isa (..))

data Options = Options
    { fileNames :: [String]
    , isa :: String
    , inplace :: Bool
    , verbose :: Bool
    , check :: Bool
    }

options :: Parser Options
options =
    Options
        <$> some
            ( strArgument
                ( metavar "FILENAME"
                    <> help "Assembly file name"
                )
            )
        <*> strOption
            ( long "isa"
                <> metavar "ISA"
                <> help "Instruction set architecture (acc32, f32a, risc-iv-32, vliw-iv, m68k, wasm32)"
            )
        <*> switch
            ( long "inplace"
                <> help "Modify the file in place"
            )
        <*> switch
            ( long "verbose"
                <> short 'v'
                <> help "Verbose output"
            )
        <*> switch
            ( long "check"
                <> help "Check the formatting without modifying the file"
            )

main :: IO ()
main = do
    opts@Options{verbose, fileNames} <- execParser optsParser
    results <- mapM (process opts) fileNames
    when verbose $ mapM_ (putTextLn . either id id) results
    case lefts results of
        [] -> exitSuccess
        _ -> exitFailure
    where
        optsParser =
            info
                (options <**> helper)
                ( fullDesc
                    <> progDesc "Format assembly files"
                    <> header "asm-formatter - a simple assembly file formatter"
                )

data ArchStyle
    = StandardArch
    | VliwArch {vliwSlotWidths :: [Int]}
    deriving (Eq, Show)

-- | Which bare-keyword tokens shift a `TextLine`'s indentation, tracked as a
-- running depth across a `.text` section, independent of `archStyle` (which
-- only governs how one already-indented line's tokens get laid out). Three
-- disjoint roles, since a keyword closing a scope isn't always the same as
-- one that permanently reduces depth going forward:
--
--   - 'biOpensBlock': starts a new nested scope -- indent everything until
--     the matching close one level deeper (@block@\/@loop@\/@if@).
--   - 'biClosesBlock': ends the innermost scope for good, both printed
--     one level shallower and reducing depth for every following line
--     (@end@).
--   - 'biRedentsLine': printed one level shallower than the current scope,
--     like a close, but the scope stays open afterward -- depth for
--     following lines is unaffected (@else@, which starts an alternate
--     body at the *same* depth the @if@'s own body had).
data BlockIndent = BlockIndent
    { biOpensBlock :: [Text]
    , biClosesBlock :: [Text]
    , biRedentsLine :: [Text]
    }

data FmtConfig = FmtConfig
    { dataLabelWidth :: Int
    , dataTypeWidth :: Int
    , dataValueWidth :: Int
    , textCommandIndent :: Int
    , textCommandTokenWidths :: [Int]
    , textCommandWidth :: Int
    , commentStart :: Text
    , archStyle :: ArchStyle
    , blockIndent :: Maybe BlockIndent
    }

instance Default FmtConfig where
    def =
        FmtConfig
            { dataLabelWidth = 16
            , dataTypeWidth = 6
            , dataValueWidth = 18
            , textCommandIndent = 4
            , textCommandTokenWidths = [8, 0, 0, 0, 0, 0, 0]
            , textCommandWidth = 40
            , commentStart = ";"
            , archStyle = StandardArch
            , blockIndent = Nothing
            }

f32aFmt :: FmtConfig
f32aFmt =
    def
        { textCommandTokenWidths = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        , textCommandWidth = 24
        , commentStart = "\\"
        }

acc32Fmt :: FmtConfig
acc32Fmt = def{textCommandTokenWidths = [12, 8, 8, 8, 8, 8, 8]}

vliwIvFmt :: FmtConfig
vliwIvFmt =
    def
        { commentStart = ";"
        , archStyle = VliwArch [34, 34, 12, 12] -- ALU1 | ALU2 | Memory | Control
        }

wasm32Fmt :: FmtConfig
wasm32Fmt =
    def
        { blockIndent =
            Just
                BlockIndent
                    { biOpensBlock = ["block", "loop", "if"]
                    , biClosesBlock = ["end"]
                    , biRedentsLine = ["else", "end"]
                    }
        }

process :: Options -> String -> IO (Either Text Text)
process Options{isa, inplace, check} fileName = do
    content <- decodeUtf8 <$> readFileBS fileName
    let formattedContent = case readMaybe isa of
            Just RiscIv -> formatFile def content
            Just F32a -> formatFile f32aFmt content
            Just Acc32 -> formatFile acc32Fmt content
            Just M68k -> formatFile def content
            Just VliwIv -> formatFile vliwIvFmt content
            Just Wasm32 -> formatFile wasm32Fmt content
            _ -> error $ "Invalid ISA: " <> show isa
        msgFormatted = toText fileName <> " already formatted"
        msgReformatted = toText fileName <> " reformatted"
    case (check, inplace, content == formattedContent) of
        (True, _, True) -> return $ Right msgFormatted
        (True, _, False) -> return $ Left $ toText fileName <> " needs formatting"
        (_, False, True) -> do
            putTextLn formattedContent
            return $ Right msgFormatted
        (_, False, False) -> do
            putTextLn formattedContent
            return $ Right msgReformatted
        (_, True, True) -> return $ Right msgFormatted
        (_, True, False) -> do
            writeFileText fileName formattedContent
            return $ Right msgReformatted

data Statement
    = OutOfSection [Text]
    | DataLine [Text]
    | TextLine [Text]
    deriving (Show)

formatFile :: FmtConfig -> Text -> Text
formatFile fmt content =
    let statements = formatLines fmt $ map (tokenize fmt) $ lines content
     in unlines statements

formatLines :: FmtConfig -> [[Text]] -> [Text]
formatLines fmt tokenss =
    let (source, comments) = unzip $ map (splitComment fmt) tokenss
        statements = formatLines' OutOfSection source
        -- Calculate VLIW slot widths if needed
        archStyle' = case archStyle fmt of
            VliwArch _ -> VliwArch (calculateVliwSlotWidths statements)
            StandardArch -> StandardArch
        fmt' = fmt{archStyle = archStyle'}
        -- One indent-in-spaces per statement, shared by both the code
        -- rendering below and comment-only lines just after: a
        -- comment-only line inside a `block`/`loop`/`if` body should nest
        -- along with the code around it, not sit flat at the top level
        -- (see 'statementIndents').
        indents = statementIndents fmt' statements
        source' = zipWith (\ind st -> pprint fmt'{textCommandIndent = ind} st) indents statements
        comments' =
            map
                ( \(s, ind, c) ->
                    if T.null c
                        then c
                        else case s of
                            OutOfSection [] -> c
                            DataLine [] -> T.replicate ind " " <> c
                            TextLine [] -> T.replicate ind " " <> c
                            _ -> c
                )
                (zip3 statements indents comments)
     in zipWith (\s c -> T.stripEnd (if T.null s then c else s <> " " <> c)) source' comments'

-- | The indent (in spaces) each statement's own line should get. Without
-- 'blockIndent' tracking, that's just 'textCommandIndent' for every line,
-- matching every non-block-structured ISA's flat indentation. With it,
-- each `TextLine` bumps or drops the running depth per 'blockLineDepth'\/
-- 'blockNextDepth' -- e.g. a `block`\/`loop`\/`if` body sits one level
-- deeper than the line that opened it.
statementIndents :: FmtConfig -> [Statement] -> [Int]
statementIndents FmtConfig{textCommandIndent, blockIndent = Just bi} statements = go 0 statements
    where
        go _ [] = []
        go depth (statement : rest) =
            let lineDepth = blockLineDepth bi depth statement
                nextDepth = blockNextDepth bi depth statement
             in (textCommandIndent + lineDepth * 4) : go nextDepth rest
statementIndents FmtConfig{textCommandIndent} statements = textCommandIndent <$ statements

blockLineDepth :: BlockIndent -> Int -> Statement -> Int
blockLineDepth BlockIndent{biRedentsLine} depth (TextLine (token : _))
    | token `elem` biRedentsLine = max 0 (depth - 1)
    | otherwise = depth
blockLineDepth _ depth _ = depth

blockNextDepth :: BlockIndent -> Int -> Statement -> Int
blockNextDepth BlockIndent{biOpensBlock, biClosesBlock} depth (TextLine (token : _))
    | token `elem` biOpensBlock = depth + 1
    | token `elem` biClosesBlock = max 0 (depth - 1)
    | otherwise = depth
blockNextDepth _ depth _ = depth

calculateVliwSlotWidths :: [Statement] -> [Int]
calculateVliwSlotWidths statements =
    let textLines =
            [ tokens
            | TextLine tokens <- statements
            , not (null tokens)
            , (t : _) <- [tokens]
            , not (T.isSuffixOf ":" t)
            ]
        slotsList = map splitByPipe textLines
        numSlots = if null slotsList then 0 else foldl' max 0 (map length slotsList)
        maxWidths =
            [ foldl'
                max
                0
                ( 0
                    : [ T.length (unwords slot)
                      | slots <- slotsList
                      , idx < length slots
                      , let slot = slots Unsafe.!! idx
                      ]
                )
            | idx <- [0 .. numSlots - 1]
            ]
     in maxWidths
    where
        splitByPipe :: [Text] -> [[Text]]
        splitByPipe [] = []
        splitByPipe tokens =
            let (slot, rest) = break (== "/") tokens
             in slot : case rest of
                    [] -> []
                    (_ : rest') -> splitByPipe rest'

splitComment :: FmtConfig -> [Text] -> ([Text], Text)
splitComment FmtConfig{} [] = ([], "")
splitComment FmtConfig{commentStart} tokens =
    if T.isPrefixOf commentStart (Unsafe.last tokens)
        then (take (length tokens - 1) tokens, Unsafe.last tokens)
        else (tokens, "")

formatLines' :: ([Text] -> Statement) -> [[Text]] -> [Statement]
formatLines' _ [] = []
formatLines' _ (tokens@(".data" : _) : rest) = OutOfSection tokens : formatLines' DataLine rest
formatLines' _ (tokens@(".text" : _) : rest) = OutOfSection tokens : formatLines' TextLine rest
formatLines' wrapper (tokens : test) = wrapper tokens : formatLines' wrapper test

width :: Int -> Text -> Text
width n t =
    let len = T.length t
     in t <> T.replicate (n - len) " "

pprint :: FmtConfig -> Statement -> Text
pprint
    FmtConfig
        { dataLabelWidth
        , dataTypeWidth
        , dataValueWidth
        , textCommandIndent
        , textCommandTokenWidths
        , textCommandWidth
        , archStyle
        } = inner
        where
            inner (OutOfSection tokens) = "    " <> unwords tokens
            inner (DataLine []) = ""
            inner (DataLine (label : type_ : rest)) =
                unwords
                    [ width dataLabelWidth label
                    , width dataTypeWidth type_
                    , width dataValueWidth (unwords rest)
                    ]
            inner (TextLine []) = ""
            inner (TextLine (l : rest))
                | T.isSuffixOf ":" l = l <> "\n" <> inner (TextLine rest)
            inner (TextLine tokens) = case archStyle of
                VliwArch widths -> T.replicate textCommandIndent " " <> formatVliwLine widths tokens
                StandardArch ->
                    let cmdTokens =
                            zipWith width textCommandTokenWidths tokens
                                <> drop (length textCommandTokenWidths) tokens
                        cmd = width textCommandWidth $ unwords cmdTokens
                     in T.replicate textCommandIndent " " <> cmd
            inner st = error $ "Invalid statement: " <> show st

            formatVliwLine :: [Int] -> [Text] -> Text
            formatVliwLine widths tokens =
                let slots = splitByPipe tokens
                    formattedSlots = zipWith formatSlot widths slots
                 in T.intercalate " / " formattedSlots

            splitByPipe :: [Text] -> [[Text]]
            splitByPipe [] = []
            splitByPipe tokens =
                let (slot, rest) = break (== "/") tokens
                 in slot : case rest of
                        [] -> []
                        (_ : rest') -> splitByPipe rest'

            formatSlot :: Int -> [Text] -> Text
            formatSlot w [] = T.replicate w " "
            formatSlot w ts = width w (unwords ts)

tokenize :: FmtConfig -> Text -> [Text]
tokenize FmtConfig{commentStart, archStyle} content = inner $ T.strip content
    where
        inner "" = []
        inner txt
            | T.isPrefixOf commentStart txt = [txt]
            | T.isPrefixOf "'" txt =
                let (string, rest) = T.breakOn "'" (T.drop 1 txt)
                 in ("'" <> string <> "'") : inner (T.strip $ T.drop 1 rest)
            | isVliwArch && T.isPrefixOf "/" txt = "/" : inner (T.strip $ T.drop 1 txt)
            | (token, rest) <-
                T.break
                    ( \c ->
                        c == ' ' || c == '\t' || c == '\'' || c == T.head commentStart || (isVliwArch && c == '/')
                    )
                    txt =
                token : inner (T.strip rest)
        isVliwArch = case archStyle of
            VliwArch _ -> True
            StandardArch -> False
