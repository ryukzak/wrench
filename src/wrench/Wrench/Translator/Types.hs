{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Wrench.Translator.Types (
    Section (..),
    CodeToken (..),
    DataToken (..),
    DataValue (..),
    WordOperand,
    ByteSize (..),
    IsWord,
    markupOffsets,
    markupSectionOffsets,
    DerefMnemonic (..),
    resolveRef,
    Ref (..),
    derefSection,
    Preprocessor,
    preprocess,
) where

import Prelude qualified
import Relude
import Text.Megaparsec.Pos (SourcePos, sourcePosPretty)
import Wrench.Machine.Types

-- | Rewrites the source before it is parsed. It keeps every character at its
-- offset, so that errors are shown against the original source, and fails
-- with the offset of what it rejects.
type Preprocessor = String -> Either (Int, Text) String

preprocess :: [Preprocessor] -> String -> Either (Int, Text) String
preprocess preprocessors src = foldlM (&) src preprocessors

class DerefMnemonic m w where
    derefMnemonic :: (Text -> Maybe w) -> w -> m (Ref w) -> m w

data Section isa w l a
    = Code
        { org :: Maybe Int
        , codeTokens :: ![CodeToken isa l]
        }
    | Data
        { org :: Maybe Int
        , dataTokens :: ![DataToken w l a]
        }
    deriving (Show)

instance (ByteSize isa, ByteSizeT w) => ByteSize (Section isa w l a) where
    byteSize Code{codeTokens} = sum $ map byteSize codeTokens
    byteSize Data{dataTokens} = sum $ map byteSize dataTokens

derefSection ::
    forall isa w.
    (ByteSize (isa (Ref w)), DerefMnemonic isa w, IsWord w, Show (isa w)) =>
    (Text -> Maybe w)
    -> w
    -> Section (isa (Ref w)) w Text (WordOperand w)
    -> Either Text (Section (isa w) w w w)
derefSection f offset code@Code{codeTokens} =
    let mnemonics = [m | Mnemonic m <- codeTokens]
        marked :: [(w, isa (Ref w))]
        marked = markupOffsets offset mnemonics
     in Right $
            code
                { codeTokens =
                    map
                        ( \(offset', m) ->
                            let m' = derefMnemonic f offset' m
                                -- Force every Ref-derived field of m' to WHNF so that
                                -- an unresolved label aborts translation here, not
                                -- lazily at execution when something happens to read
                                -- the value (see issue #143). Walking @show@ visits
                                -- every constructor field, which is enough since
                                -- @w@ is a machine word and forcing it to WHNF is
                                -- already full evaluation.
                                !_ = length (show m' :: String)
                             in Mnemonic m'
                        )
                        marked
                }
derefSection f _offset Data{org, dataTokens} = Data org <$> traverse resolveToken dataTokens
    where
        resolveToken DataToken{dtLabel, dtValue} = do
            addr <- maybeToRight ("unknown label: " <> dtLabel) (f dtLabel)
            value <- case dtValue of
                DByte bs -> Right (DByte bs)
                DWord ws -> DWord <$> traverse (either resolveAddress Right) ws
            pure (DataToken addr value)
        resolveAddress (pos, label) =
            maybeToRight (toText (sourcePosPretty pos) <> ": can't resolve label: " <> show label) (f label)

markupOffsets :: (ByteSize t, IsWord w) => w -> [t] -> [(w, t)]
markupOffsets _offset [] = []
markupOffsets offset (m : ms) = (offset, m) : markupOffsets (offset + toEnum (byteSize m)) ms

markupSectionOffsets :: (ByteSize isa, IsWord w) => w -> [Section isa w l a] -> [(w, Section isa w l a)]
markupSectionOffsets _offset [] = []
markupSectionOffsets offset (s : ss) =
    let offset' = Prelude.maybe offset toEnum (org s)
     in (offset', s) : markupSectionOffsets (offset' + toEnum (byteSize s)) ss

data CodeToken isa l
    = Label l
    | Mnemonic isa
    deriving (Show)

instance (ByteSize isa) => ByteSize (CodeToken isa l) where
    byteSize (Mnemonic m) = byteSize m
    byteSize _ = 0

data Ref w
    = Ref
        { refPrepare :: w -> w
        , refPos :: SourcePos
        , refLabel :: Text
        }
    | ValueR
        { refPrepare :: w -> w
        , refPos :: SourcePos
        , refValue :: w
        }

instance (Eq w) => Eq (Ref w) where
    Ref{refLabel = l} == Ref{refLabel = l'} = l == l'
    ValueR{refValue = x} == ValueR{refValue = x'} = x == x'
    _ == _ = False

instance (Show w) => Show (Ref w) where
    show Ref{refLabel} = toString refLabel
    show ValueR{refPrepare, refValue} = show $ refPrepare refValue

-- | Resolve a 'Ref' against a label table and check the result against the
-- instruction field it is going to be encoded into. Strict, so that an
-- unresolved label aborts translation here rather than becoming a thunk that
-- only blows up later if something happens to read it. Either failure is
-- reported with the position the reference was written at.
resolveRef :: (w -> Either Text w) -> (Text -> Maybe w) -> Ref w -> w
resolveRef valueGuard labelResolver ref =
    let pos = toText $ sourcePosPretty $ refPos ref
        !value = case ref of
            Ref{refPrepare, refLabel}
                | Just x <- labelResolver refLabel -> refPrepare x
                | otherwise -> error (pos <> ": can't resolve label: " <> show refLabel)
            ValueR{refPrepare, refValue} -> refPrepare refValue
     in case valueGuard value of
            Right x -> x
            Left err -> error (pos <> ": " <> err)

type WordOperand w = Either (SourcePos, Text) w

data DataToken w l a = DataToken
    { dtLabel :: !l
    , dtValue :: DataValue w a
    }
    deriving (Show)

instance (ByteSizeT w) => ByteSize (DataToken w l a) where
    byteSize DataToken{dtValue} = byteSize dtValue

data DataValue w a
    = DByte [Word8]
    | DWord [a]
    deriving (Show)

instance (ByteSizeT w) => ByteSize (DataValue w a) where
    byteSize (DByte xs) = length xs
    byteSize (DWord xs) = byteSizeT @w * length xs
