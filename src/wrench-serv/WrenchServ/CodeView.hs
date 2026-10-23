module WrenchServ.CodeView (
    escapeHtml,
    formatCodeWithLineNumbers,
    formatAsm,
    formatReport,
) where

import Data.Text qualified as T
import Lucid (renderText, toHtml)
import Relude
import Wrench.Highlight (TokenKind (..), tokenizeLine, tokenizeReportLine)
import Wrench.Isa (Isa)

formatCodeWithLineNumbers :: Text -> Text
formatCodeWithLineNumbers = formatCodeLines (\line -> "<div class=\"code-line\">" <> escapeHtml line <> "</div>")

formatAsm :: Maybe Isa -> Text -> Text
formatAsm Nothing = formatCodeWithLineNumbers
formatAsm (Just isa) = formatCodeLines (highlightLine . tokenizeLine isa)

formatReport :: Maybe Isa -> Text -> Text
formatReport Nothing = formatCodeWithLineNumbers
formatReport (Just isa) = formatCodeLines (highlightLine . tokenizeReportLine isa)

formatCodeLines :: (Text -> Text) -> Text -> Text
formatCodeLines renderLine code =
    let codeLines = T.lines (T.filter (/= '\r') code)
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

highlightLine :: [(TokenKind, Text)] -> Text
highlightLine tokens =
    let commentOnly = any ((== Comment) . fst) tokens && all (\(k, t) -> k == Comment || T.null (T.strip t)) tokens
        cls = if commentOnly then "code-line comment-only" else "code-line"
     in "<div class=\"" <> cls <> "\">" <> T.concat (map renderToken tokens) <> "</div>"
    where
        renderToken (Plain, t) = escapeHtml t
        renderToken (kind, t) = "<span class=\"tok-" <> T.toLower (show kind) <> "\">" <> escapeHtml t <> "</span>"

escapeHtml :: Text -> Text
escapeHtml = toText . renderText . toHtml
