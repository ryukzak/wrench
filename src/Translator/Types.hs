{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Translator.Types (
    Section (..),
    CodeToken (..),
    DataToken (..),
    DataValue (..),
    ByteLength (..),
    MachineWord,
    markupOffsets,
    markupSectionOffsets,
    DerefMnemonic (..),
    deref',
    Ref (..),
    derefSection,
) where

import Data.Default (Default, def)
import Machine.Types
import Relude
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

instance (ByteLength isa, ByteLength w, Default w) => ByteLength (Section isa w l) where
    byteLength Code{codeTokens} = sum $ map byteLength codeTokens
    byteLength Data{dataTokens} = sum $ map byteLength dataTokens

derefSection ::
    forall isa w.
    (ByteLength (isa (Ref w)), DerefMnemonic isa w, MachineWord w) =>
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

markupOffsets :: (ByteLength t, MachineWord w) => w -> [t] -> [(w, t)]
markupOffsets _offset [] = []
markupOffsets offset (m : ms) = (offset, m) : markupOffsets (offset + toEnum (byteLength m)) ms

markupSectionOffsets :: (ByteLength isa, MachineWord w) => w -> [Section isa w l] -> [(w, Section isa w l)]
markupSectionOffsets _offset [] = []
markupSectionOffsets offset (s : ss) =
    let offset' = Prelude.maybe offset toEnum (org s)
     in (offset', s) : markupSectionOffsets (offset' + toEnum (byteLength s)) ss

data CodeToken isa l
    = Label l
    | Mnemonic isa
    deriving (Show)

instance (ByteLength isa) => ByteLength (CodeToken isa l) where
    byteLength (Mnemonic m) = byteLength m
    byteLength _ = 0

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

instance (ByteLength w, Default w) => ByteLength (DataToken w l) where
    byteLength DataToken{dtValue} = byteLength dtValue

data DataValue w
    = DByte [Word8]
    | DWord [w]
    deriving (Show)

instance (ByteLength w, Default w) => ByteLength (DataValue w) where
    byteLength (DByte xs) = length xs
    byteLength (DWord xs) = byteLength (def :: w) * length xs
