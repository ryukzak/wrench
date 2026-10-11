{- | Pure machine-word helpers: fixed-width truncation/sign handling and
arithmetic that also reports overflow and carry. These operate on any
'IsWord' and are shared across the ISA implementations.
-}
module Wrench.Machine.Word (
    fitSigned,
    lShiftR,
    lShiftL,
    Ext (..),
    addExt,
    subExt,
    mulExt,
    b8,
    u5,
    u16,
    u20,
    i10,
    i12,
    i13,
    i15,
    i16,
    i17,
    i20,
    i21,
) where

import Data.Bits
import Relude
import Wrench.Machine.Types (FromSign (..), IsWord)

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
-- simulator and being truncated there. One per field width the ISAs encode, so
-- a call site names the field rather than computing it.

-- | Any pattern a byte can spell, read as signed or unsigned -- the range the
-- @.byte@ directive accepts, so a byte-wide immediate and a byte of data agree
-- on what fits.
b8 :: (IsWord w) => (Text -> Text) -> w -> Either Text w
b8 errMsgF x
    | -128 <= x && x <= 255 = Right x
    | otherwise = Left $ errMsgF $ show x

u5 :: (IsWord w) => (Text -> Text) -> w -> Either Text w
u5 errMsgF x
    | 0 <= x && x <= 31 = Right x
    | otherwise = Left $ errMsgF $ show x

u16 :: (IsWord w) => (Text -> Text) -> w -> Either Text w
u16 errMsgF x
    | 0 <= x && x <= 65535 = Right x
    | otherwise = Left $ errMsgF $ show x

u20 :: (IsWord w) => (Text -> Text) -> w -> Either Text w
u20 errMsgF x
    | 0 <= x && x <= 1048575 = Right x
    | otherwise = Left $ errMsgF $ show x

i10 :: (IsWord w) => (Text -> Text) -> w -> Either Text w
i10 errMsgF x
    | -512 <= x && x <= 511 = Right x
    | otherwise = Left $ errMsgF $ show x

i12 :: (IsWord w) => (Text -> Text) -> w -> Either Text w
i12 errMsgF x
    | -2048 <= x && x <= 2047 = Right x
    | otherwise = Left $ errMsgF $ show x

i13 :: (IsWord w) => (Text -> Text) -> w -> Either Text w
i13 errMsgF x
    | -4096 <= x && x <= 4095 = Right x
    | otherwise = Left $ errMsgF $ show x

i15 :: (IsWord w) => (Text -> Text) -> w -> Either Text w
i15 errMsgF x
    | -16384 <= x && x <= 16383 = Right x
    | otherwise = Left $ errMsgF $ show x

i16 :: (IsWord w) => (Text -> Text) -> w -> Either Text w
i16 errMsgF x
    | -32768 <= x && x <= 32767 = Right x
    | otherwise = Left $ errMsgF $ show x

i17 :: (IsWord w) => (Text -> Text) -> w -> Either Text w
i17 errMsgF x
    | -65536 <= x && x <= 65535 = Right x
    | otherwise = Left $ errMsgF $ show x

i20 :: (IsWord w) => (Text -> Text) -> w -> Either Text w
i20 errMsgF x
    | -524288 <= x && x <= 524287 = Right x
    | otherwise = Left $ errMsgF $ show x

i21 :: (IsWord w) => (Text -> Text) -> w -> Either Text w
i21 errMsgF x
    | -1048576 <= x && x <= 1048575 = Right x
    | otherwise = Left $ errMsgF $ show x
