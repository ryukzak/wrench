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
    (ByteSize (isa (Ref w)), DerefMnemonic isa w, MachineWord w) =>
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

instance (Show w) => Show (Ref w) where
    show (Ref _ l) = l
    show (ValueR f x) = show $ f x

deref' :: (String -> Maybe w) -> Ref w -> w
deref' f (Ref prepare l) = prepare <$> fromMaybe (error ("Can't resolve label: " <> show l)) $ f l
deref' _f (ValueR prepare x) = prepare x

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
