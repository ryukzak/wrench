{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Wrench.Translator.Types (
    Section (..),
    CodeToken (..),
    DataToken (..),
    DataValue (..),
    ByteSize (..),
    IsWord,
    markupOffsets,
    markupSectionOffsets,
    DerefMnemonic (..),
    deref',
    resolveRef,
    Ref (..),
    derefSection,
) where

import Prelude qualified
import Relude
import Text.Megaparsec.Pos (SourcePos, sourcePosPretty)
import Wrench.Machine.Types

class DerefMnemonic m w where
    derefMnemonic :: (Text -> Maybe w) -> w -> m (Ref w) -> m w

data Section isa w l
    = Code
        { org :: Maybe Int
        , codeTokens :: ![CodeToken isa l]
        }
    | Data
        { org :: Maybe Int
        , dataTokens :: ![DataToken w l]
        }
    deriving (Show)

instance (ByteSize isa, ByteSizeT w) => ByteSize (Section isa w l) where
    byteSize Code{codeTokens} = sum $ map byteSize codeTokens
    byteSize Data{dataTokens} = sum $ map byteSize dataTokens

derefSection ::
    forall isa w.
    (ByteSize (isa (Ref w)), DerefMnemonic isa w, IsWord w, Show (isa w)) =>
    (Text -> Maybe w)
    -> w
    -> Section (isa (Ref w)) w Text
    -> Section (isa w) w w
derefSection f offset code@Code{codeTokens} =
    let mnemonics = [m | Mnemonic m <- codeTokens]
        marked :: [(w, isa (Ref w))]
        marked = markupOffsets offset mnemonics
     in code
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
derefSection f _offset dt@Data{dataTokens} =
    dt
        { dataTokens =
            map
                ( \DataToken{dtLabel, dtValue} ->
                    DataToken
                        { dtLabel = fromMaybe (error $ "unknown label: " <> show dtLabel) $ f dtLabel
                        , dtValue = dtValue
                        }
                )
                dataTokens
        }

markupOffsets :: (ByteSize t, IsWord w) => w -> [t] -> [(w, t)]
markupOffsets _offset [] = []
markupOffsets offset (m : ms) = (offset, m) : markupOffsets (offset + toEnum (byteSize m)) ms

markupSectionOffsets :: (ByteSize isa, IsWord w) => w -> [Section isa w l] -> [(w, Section isa w l)]
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

-- | 'resolveRef' for a field that has nothing to check.
deref' :: (Text -> Maybe w) -> Ref w -> w
deref' = resolveRef Right

data DataToken w l = DataToken
    { dtLabel :: !l
    , dtValue :: DataValue w
    }
    deriving (Show)

instance (ByteSizeT w) => ByteSize (DataToken w l) where
    byteSize DataToken{dtValue} = byteSize dtValue

data DataValue w
    = DByte [Word8]
    | DWord [w]
    deriving (Show)

instance (ByteSizeT w) => ByteSize (DataValue w) where
    byteSize (DByte xs) = length xs
    byteSize (DWord xs) = byteSizeT @w * length xs
