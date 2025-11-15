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
                <> help "Instruction set architecture (acc32, f32a, risc-iv-32, vliw-iv, m68k)"
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

data FmtConfig = FmtConfig
    { dataLabelWidth :: Int
    , dataTypeWidth :: Int
    , dataValueWidth :: Int
    , textCommandIndent :: Int
    , textCommandTokenWidths :: [Int]
    , textCommandWidth :: Int
    , commentStart :: Text
    , isVliw :: Bool
    , vliwSlotWidths :: [Int]
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
            , isVliw = False
            , vliwSlotWidths = []
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
        , isVliw = True
        , vliwSlotWidths = [15, 34, 34, 0]  -- Memory | ALU1 | ALU2 | Control (last slot no padding)
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
        slotWidths = if isVliw fmt then calculateVliwSlotWidths statements else vliwSlotWidths fmt
        fmt' = fmt{vliwSlotWidths = slotWidths}
        source' = map (pprint fmt') statements
        comments' =
            zipWith
                ( \s c ->
                    if T.null c
                        then c
                        else case s of
                            OutOfSection [] -> c
                            DataLine [] -> T.replicate 4 " " <> c
                            TextLine [] -> T.replicate 4 " " <> c
                            _ -> c
                )
                statements
                comments
     in zipWith (\s c -> T.stripEnd (if T.null s then c else s <> " " <> c)) source' comments'

calculateVliwSlotWidths :: [Statement] -> [Int]
calculateVliwSlotWidths statements =
    let textLines = [tokens | TextLine tokens <- statements, not (null tokens), case tokens of (t:_) -> not (T.isSuffixOf ":" t); _ -> True]
        slotsList = map splitByPipe textLines
        numSlots = if null slotsList then 0 else foldl' max 0 (map length slotsList)
        maxWidths = [foldl' max 0 (0 : [T.length (unwords slot) | slots <- slotsList, idx < length slots, let slot = slots Unsafe.!! idx]) | idx <- [0 .. numSlots - 1]]
     in maxWidths
    where
        splitByPipe :: [Text] -> [[Text]]
        splitByPipe [] = []
        splitByPipe tokens =
            let (slot, rest) = break (== "|") tokens
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
        , isVliw
        , vliwSlotWidths
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
            inner (TextLine tokens)
                | isVliw = T.replicate textCommandIndent " " <> formatVliwLine vliwSlotWidths tokens
            inner (TextLine tokens) =
                let cmdTokens = zipWith width textCommandTokenWidths tokens
                    cmd = width textCommandWidth $ unwords cmdTokens
                 in T.replicate textCommandIndent " " <> cmd
            inner st = error $ "Invalid statement: " <> show st
            
            formatVliwLine :: [Int] -> [Text] -> Text
            formatVliwLine widths tokens =
                let slots = splitByPipe tokens
                    formattedSlots = zipWith formatSlot widths slots
                 in T.intercalate " | " formattedSlots
            
            splitByPipe :: [Text] -> [[Text]]
            splitByPipe [] = []
            splitByPipe tokens =
                let (slot, rest) = break (== "|") tokens
                 in slot : case rest of
                        [] -> []
                        (_ : rest') -> splitByPipe rest'
            
            formatSlot :: Int -> [Text] -> Text
            formatSlot w [] = T.replicate w " "
            formatSlot w ts = width w (unwords ts)

tokenize :: FmtConfig -> Text -> [Text]
tokenize FmtConfig{commentStart, isVliw} content = inner $ T.strip content
    where
        inner "" = []
        inner txt
            | T.isPrefixOf commentStart txt = [txt]
            | T.isPrefixOf "'" txt =
                let (string, rest) = T.breakOn "'" (T.drop 1 txt)
                 in ("'" <> string <> "'") : inner (T.strip $ T.drop 1 rest)
            | isVliw && T.isPrefixOf "|" txt = "|" : inner (T.strip $ T.drop 1 txt)
            | (token, rest) <-
                T.break
                    ( \c ->
                        c == ' ' || c == '\t' || c == '\'' || c == T.head commentStart || (isVliw && c == '|')
                    )
                    txt =
                token : inner (T.strip rest)
