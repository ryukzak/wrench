module Machine.Types (
    Trace (..),
    Machine (..),
    Mem,
    IoMem (..),
    Cell (..),
    InitState (..),
    StateInterspector (..),
    MachineWord,
    FromSign (..),
    ViewState (..),
    RegisterId,
    ByteLength (..),
    WordParts (..),
    signBitAnd,
    Ext (..),
    addExt,
    subExt,
    mulExt,
) where

import Data.Bits
import Data.Default (Default)
import Relude

-- * State

type MachineWord w =
    ( Bits w
    , FiniteBits w
    , ByteLength w
    , Default w
    , Enum w
    , FromSign w
    , Num (Unsign w)
    , Hashable w
    , Num w
    , Ord (Unsign w)
    , Ord w
    , Read w
    , Show w
    , WordParts w
    , Integral w
    , FromSign w
    )

type RegisterId r = (Hashable r, Show r, Read r)

class (Bits (Unsign w), Integral (Unsign w), Show (Unsign w)) => FromSign w where
    type Unsign w :: Type
    fromSign :: w -> Unsign w

instance FromSign Int32 where
    type Unsign Int32 = Word32
    fromSign = fromIntegral

class WordParts w where
    wordSplit :: w -> [Word8]
    wordCombine :: [Word8] -> w

instance WordParts Int32 where
    wordSplit w = [byte3, byte2, byte1, byte0]
        where
            byte0 = fromIntegral $ (w `shiftR` 24) .&. 0xFF -- Extract the highest byte
            byte1 = fromIntegral $ (w `shiftR` 16) .&. 0xFF -- Extract the second byte
            byte2 = fromIntegral $ (w `shiftR` 8) .&. 0xFF -- Extract the third byte
            byte3 = fromIntegral $ w .&. 0xFF

    wordCombine [byte3, byte2, byte1, byte0] =
        (fromIntegral byte0 `shiftL` 24)
            .|. (fromIntegral byte1 `shiftL` 16)
            .|. (fromIntegral byte2 `shiftL` 8)
            .|. fromIntegral byte3
    wordCombine _ = error "not applicable"

signBitAnd :: (MachineWord w) => w -> w -> w
signBitAnd x mask
    | x < 0 = x .|. complement mask
    | otherwise = x .&. mask

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
        carry = False
     in Ext{value = result, overflow, carry}

mulExt :: (MachineWord w) => w -> w -> Ext w
mulExt x y =
    let result = x * y
        overflow = (x /= 0 && y /= 0 && result `div` x /= y)
        carry = (fromIntegral x * fromIntegral y) > (maxBound :: Word)
     in Ext{value = result, overflow, carry}

class ByteLength t where
    byteLength :: t -> Int

instance ByteLength Word32 where
    byteLength _ = 4

instance ByteLength Int32 where
    byteLength _ = 4

class InitState mem st | st -> mem where
    initState :: Int -> mem -> st

class StateInterspector st isa w r | st -> isa w r where
    registers :: st -> HashMap r w
    programCounter :: st -> Int
    memoryDump :: st -> Mem isa w
    ioStreams :: st -> IntMap ([w], [w])
    reprState :: HashMap String w -> st -> Text -> Text
    reprState _labels _st var = "unknown variable: " <> var

class ViewState st where
    viewState :: st -> Text -> Text

class Machine st isa w | st -> isa w where
    instructionFetch :: State st (Maybe (Int, isa))
    instructionStep :: State st ()

newtype Trace st isa
    = TState st
    deriving (Show)

type Mem isa w = IntMap (Cell isa w)

data IoMem isa w = IoMem
    { mIoStreams :: IntMap ([w], [w])
    , mIoCells :: Mem isa w
    }
    deriving (Show)

data Cell isa w
    = Instruction isa
    | InstructionPart
    | Value Word8
    deriving (Show)
