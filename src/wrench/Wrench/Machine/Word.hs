{- | Pure machine-word helpers: fixed-width truncation/sign handling and
arithmetic that also reports overflow and carry. These operate on any
'IsWord' and are shared across the ISA implementations.
-}
module Wrench.Machine.Word (
    signBitAnd,
    fitSigned,
    lShiftR,
    lShiftL,
    Ext (..),
    addExt,
    subExt,
    mulExt,
    u5,
    u20,
    i12,
    i13,
    i21,
) where

import Data.Bits
import Relude
import Wrench.Machine.Types (FromSign (..), IsWord)

-- | Truncate @x@ to the low bits of @mask@ while preserving its original sign:
-- a negative value keeps its high bits set, a non-negative one is masked. This
-- is /not/ a signed-field truncation (see 'fitSigned'): the sign comes from the
-- full word rather than from the top bit of the field, so a value that only
-- /looks/ positive once truncated stays positive. Used by the accumulator ISA.
signBitAnd :: (IsWord w) => w -> w -> w
signBitAnd x mask
    | x < 0 = x .|. complement mask
    | otherwise = x .&. mask

-- | Reduce a value to a fixed-width signed instruction field: keep the low @n@
-- bits and sign-extend from bit @n-1@ to the full machine word. Bits at or above
-- bit @n@ are silently discarded, modelling an immediate/offset that does not
-- fit its encoding field.
fitSigned :: (IsWord w) => Int -> w -> w
fitSigned n x =
    let mask = bit n - 1
        low = x .&. mask
     in if testBit low (n - 1) then low .|. complement mask else low

lShiftR :: (IsWord w) => w -> w -> w
lShiftR x n = toSign (fromSign x `shiftR` fromEnum n)

lShiftL :: (IsWord w) => w -> w -> w
lShiftL x n = toSign (fromSign x `shiftL` fromEnum n)

data Ext a = Ext {value :: a, overflow :: Bool, carry :: Bool}
    deriving (Eq, Show)

addExt :: (IsWord w) => w -> w -> Ext w
addExt x y =
    let result = x + y
        overflow = ((x > 0 && y > 0 && result < 0) || (x < 0 && y < 0 && result > 0))
        carry = testBit (toInteger (fromSign x) + toInteger (fromSign y)) (finiteBitSize x)
     in Ext{value = result, overflow, carry}

subExt :: (IsWord w) => w -> w -> Ext w
subExt x y =
    let result = x - y
        overflow = ((x > 0 && y < 0 && result < 0) || (x < 0 && y > 0 && result > 0))
        carry = fromSign x < fromSign y
     in Ext{value = result, overflow, carry}

mulExt :: (IsWord w) => w -> w -> Ext w
mulExt x y =
    let result = x * y
        overflow = (x /= 0 && y /= 0 && result `div` x /= y)
        carry = (fromIntegral x * fromIntegral y) > (maxBound :: Word)
     in Ext{value = result, overflow, carry}

-- Field guards: a value that does not fit the field it is encoded into stops
-- translation with the message the caller builds, instead of reaching the
-- simulator and being truncated there.
u5 :: (IsWord w) => (Text -> Text) -> w -> Either Text w
u5 errMsgF x
    | 0 <= x && x <= 31 = Right x
    | otherwise = Left $ errMsgF $ show x

u20 :: (IsWord w) => (Text -> Text) -> w -> Either Text w
u20 errMsgF x
    | 0 <= x && x <= 1048575 = Right x
    | otherwise = Left $ errMsgF $ show x

i12 :: (IsWord w) => (Text -> Text) -> w -> Either Text w
i12 errMsgF x
    | -2048 <= x && x <= 2047 = Right x
    | otherwise = Left $ errMsgF $ show x

i13 :: (IsWord w) => (Text -> Text) -> w -> Either Text w
i13 errMsgF x
    | -4096 <= x && x <= 4095 = Right x
    | otherwise = Left $ errMsgF $ show x

i21 :: (IsWord w) => (Text -> Text) -> w -> Either Text w
i21 errMsgF x
    | -1048576 <= x && x <= 1048575 = Right x
    | otherwise = Left $ errMsgF $ show x
