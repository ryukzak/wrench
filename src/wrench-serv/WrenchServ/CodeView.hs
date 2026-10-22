module WrenchServ.CodeView (
    escapeHtml,
    formatCodeWithLineNumbers,
    formatAsm,
) where

import Data.Text qualified as T
import Lucid (renderText, toHtml)
import Relude
import Wrench.Highlight (TokenKind (..), tokenizeLine)
import Wrench.Isa (Isa)

formatCodeWithLineNumbers :: Text -> Text
formatCodeWithLineNumbers = formatCodeLines (\line -> "<div class=\"code-line\">" <> escapeHtml line <> "</div>")

formatAsm :: Maybe Isa -> Text -> Text
formatAsm Nothing = formatCodeWithLineNumbers
formatAsm (Just isa) = formatCodeLines (highlightLine isa)

formatCodeLines :: (Text -> Text) -> Text -> Text
formatCodeLines renderLine code =
    let codeLines = T.lines code
        lineCount = length codeLines
        lineNumbers = T.concat $ map (\i -> "<div class=\"line-number\">" <> show i <> "</div>") [1 .. lineCount]
        codeContent = T.concat $ map renderLine codeLines
        container =
            "<div class=\"code-container\"><div class=\"line-numbers\">"
                <> lineNumbers
                <> "</div><div class=\"code-content\">"
                <> codeContent
                <> "</div></div>"
     in container

highlightLine :: Isa -> Text -> Text
highlightLine isa line =
    let tokens = tokenizeLine isa line
        commentOnly = any ((== Comment) . fst) tokens && all (\(k, t) -> k == Comment || T.null (T.strip t)) tokens
        cls = if commentOnly then "code-line comment-only" else "code-line"
     in "<div class=\"" <> cls <> "\">" <> T.concat (map renderToken tokens) <> "</div>"
    where
        renderToken (Plain, t) = escapeHtml t
        renderToken (kind, t) = "<span class=\"tok-" <> T.toLower (show kind) <> "\">" <> escapeHtml t <> "</span>"

escapeHtml :: Text -> Text
escapeHtml = toText . renderText . toHtml
