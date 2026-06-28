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
    MachineWord,
    markupOffsets,
    markupSectionOffsets,
    DerefMnemonic (..),
    deref',
    Ref (..),
    derefSection,
) where

import Relude
import Wrench.Machine.Types
import Prelude qualified

class DerefMnemonic m w where
    derefMnemonic :: (String -> Maybe w) -> w -> m (Ref w) -> m w

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
    (ByteSize (isa (Ref w)), DerefMnemonic isa w, MachineWord w, Show (isa w)) =>
    (String -> Maybe w)
    -> w
    -> Section (isa (Ref w)) w String
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

markupOffsets :: (ByteSize t, MachineWord w) => w -> [t] -> [(w, t)]
markupOffsets _offset [] = []
markupOffsets offset (m : ms) = (offset, m) : markupOffsets (offset + toEnum (byteSize m)) ms

markupSectionOffsets :: (ByteSize isa, MachineWord w) => w -> [Section isa w l] -> [(w, Section isa w l)]
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
    = Ref (w -> w) String
    | ValueR (w -> w) w

instance (Eq w) => Eq (Ref w) where
    (Ref _ l) == (Ref _ l') = l == l'
    (ValueR _ x) == (ValueR _ x') = x == x'
    _ == _ = False

instance (Show w) => Show (Ref w) where
    show (Ref _ l) = l
    show (ValueR f x) = show $ f x

-- | Resolve a 'Ref' against a label table. Strict: forces the lookup and the
--   resulting value to WHNF before returning. Call sites should use @$!@ so
--   that an unresolved label aborts translation rather than producing a thunk
--   that only blows up later if something happens to read it.
deref' :: (String -> Maybe w) -> Ref w -> w
deref' f (Ref prepare l) = case f l of
    Just w -> let !v = prepare w in v
    Nothing -> error ("Can't resolve label: " <> show l)
deref' _f (ValueR prepare x) = let !v = prepare x in v

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
