{- | Pure machine-word helpers: fixed-width truncation/sign handling and
arithmetic that also reports overflow and carry. These operate on any
'MachineWord' and are shared across the ISA implementations.
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
) where

import Data.Bits
import Relude
import Wrench.Machine.Types (FromSign (..), MachineWord)

-- | Truncate @x@ to the low bits of @mask@ while preserving its original sign:
-- a negative value keeps its high bits set, a non-negative one is masked. This
-- is /not/ a signed-field truncation (see 'fitSigned') — it is the semantics the
-- accumulator and RISC-V ISAs rely on for unsigned low-bit immediates such as
-- @%lo@.
signBitAnd :: (MachineWord w) => w -> w -> w
signBitAnd x mask
    | x < 0 = x .|. complement mask
    | otherwise = x .&. mask

-- | Reduce a value to a fixed-width signed instruction field: keep the low @n@
-- bits and sign-extend from bit @n-1@ to the full machine word. Bits at or above
-- bit @n@ are silently discarded, modelling an immediate/offset that does not
-- fit its encoding field.
fitSigned :: (MachineWord w) => Int -> w -> w
fitSigned n x =
    let mask = bit n - 1
        low = x .&. mask
     in if testBit low (n - 1) then low .|. complement mask else low

lShiftR :: (MachineWord w) => w -> w -> w
lShiftR x n = toSign (fromSign x `shiftR` fromEnum n)

lShiftL :: (MachineWord w) => w -> w -> w
lShiftL x n = toSign (fromSign x `shiftL` fromEnum n)

data Ext a = Ext {value :: a, overflow :: Bool, carry :: Bool}
    deriving (Eq, Show)

addExt :: (MachineWord w) => w -> w -> Ext w
addExt x y =
    let result = x + y
        overflow = ((x > 0 && y > 0 && result < 0) || (x < 0 && y < 0 && result > 0))
        carry = testBit (toInteger (fromSign x) + toInteger (fromSign y)) (finiteBitSize x)
     in Ext{value = result, overflow, carry}

subExt :: (MachineWord w) => w -> w -> Ext w
subExt x y =
    let result = x - y
        overflow = ((x > 0 && y < 0 && result < 0) || (x < 0 && y > 0 && result > 0))
        carry = fromSign x < fromSign y
     in Ext{value = result, overflow, carry}

mulExt :: (MachineWord w) => w -> w -> Ext w
mulExt x y =
    let result = x * y
        overflow = (x /= 0 && y /= 0 && result `div` x /= y)
        carry = (fromIntegral x * fromIntegral y) > (maxBound :: Word)
     in Ext{value = result, overflow, carry}
