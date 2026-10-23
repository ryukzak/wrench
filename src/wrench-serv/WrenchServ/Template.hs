module WrenchServ.Template (fillTemplate) where

import Data.List (lookup)
import Data.Text qualified as T
import Relude

-- | Substitute @{{key}}@ placeholders in one pass: inserted values are never
-- scanned again, and unknown placeholders are kept as is.
fillTemplate :: [(Text, Text)] -> Text -> Text
fillTemplate vars = T.concat . go
    where
        go t = case T.breakOn "{{" t of
            (before, "") -> [before]
            (before, rest) ->
                let (key, after) = T.breakOn "}}" (T.drop 2 rest)
                 in case lookup key vars of
                        Just value | not (T.null after) -> before : value : go (T.drop 2 after)
                        _ -> before : "{{" : go (T.drop 2 rest)
