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
    DerefMnemonic (..),
    deref',
    Ref (..),
    derefSection,
) where

import Data.Default (Default, def)
import Machine.Types
import Relude

class DerefMnemonic m w where
    derefMnemonic :: (String -> Maybe w) -> w -> m (Ref w) -> m w

data Section isa w l
    = Code
        { codeTokens :: ![CodeToken isa l]
        }
    | Data
        { dataTokens :: ![DataToken w l]
        }
    deriving (Show)

instance (ByteLength isa, ByteLength w, Default w) => ByteLength (Section isa w l) where
    byteLength Code{codeTokens} = sum $ map byteLength codeTokens
    byteLength Data{dataTokens} = sum $ map byteLength dataTokens

derefSection ::
    forall isa w.
    (MachineWord w, ByteLength (isa (Ref w)), DerefMnemonic isa w) =>
    (String -> Maybe w)
    -> w
    -> Section (isa (Ref w)) w String
    -> Section (isa w) w w
derefSection f offset Code{codeTokens} =
    let mnemonics = [m | Mnemonic m <- codeTokens]
        marked :: [(w, isa (Ref w))]
        marked = markupOffsets offset mnemonics
     in Code
            { codeTokens =
                map
                    ( \(offset', m) ->
                        let m' = derefMnemonic f offset' m
                         in Mnemonic m'
                    )
                    marked
            }
derefSection f _offset Data{dataTokens} =
    Data
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

markupOffsets :: (MachineWord w, ByteLength t) => w -> [t] -> [(w, t)]
markupOffsets _offset [] = []
markupOffsets offset (m : ms) = (offset, m) : markupOffsets (offset + toEnum (byteLength m)) ms

data CodeToken isa l
    = Comment l
    | Label l
    | Mnemonic isa
    deriving (Show)

instance (ByteLength isa) => ByteLength (CodeToken isa l) where
    byteLength (Mnemonic m) = byteLength m
    byteLength _ = 0

data Ref w
    = Ref String
    | ValueR w
    deriving (Show)

deref' :: (String -> Maybe w) -> Ref w -> w
deref' f (Ref l) = fromMaybe (error ("Can't resolve label: " <> show l)) $ f l
deref' _f (ValueR x) = x

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
