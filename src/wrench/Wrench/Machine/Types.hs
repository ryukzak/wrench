module Wrench.Machine.Types (
    Trace (..),
    Machine (..),
    Mem (..),
    IoMem (..),
    mkIoMem,
    Cell (..),
    InitState (..),
    StateInterspector (..),
    MachineWord,
    FromSign (..),
    RegisterId,
    ByteSize (..),
    ByteSizeT (..),
    WordParts (..),
    signBitAnd,
    Ext (..),
    addExt,
    subExt,
    mulExt,
    halted,
    lShiftL,
    lShiftR,
) where

import Data.Bits
import Data.Default (Default, def)
import Relude
import Relude.Extra (keys)

-- * State

type MachineWord w =
    ( Bits w
    , FiniteBits w
    , ByteSize w
    , ByteSizeT w
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
    , Bounded w
    , Bounded (Unsign w)
    )

type RegisterId r = (Hashable r, Show r, Read r)

class (Bits (Unsign w), Bounded (Unsign w), Integral (Unsign w), Show (Unsign w)) => FromSign w where
    type Unsign w :: Type
    fromSign :: w -> Unsign w
    toSign :: Unsign w -> w

instance FromSign Int8 where
    type Unsign Int8 = Word8
    fromSign = fromIntegral
    toSign = fromIntegral

instance FromSign Int32 where
    type Unsign Int32 = Word32
    fromSign = fromIntegral
    toSign = fromIntegral

class WordParts w where
    wordSplit :: w -> [Word8]
    wordCombine :: [Word8] -> w
    byteToWord :: Word8 -> w

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

    byteToWord = fromIntegral

instance WordParts Int8 where
    wordSplit b = [fromInteger $ toInteger b]
    wordCombine [b] = fromInteger $ toInteger b
    wordCombine _ = error "not applicable"
    byteToWord = fromIntegral

signBitAnd :: (MachineWord w) => w -> w -> w
signBitAnd x mask
    | x < 0 = x .|. complement mask
    | otherwise = x .&. mask

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
        carry = False
     in Ext{value = result, overflow, carry}

mulExt :: (MachineWord w) => w -> w -> Ext w
mulExt x y =
    let result = x * y
        overflow = (x /= 0 && y /= 0 && result `div` x /= y)
        carry = (fromIntegral x * fromIntegral y) > (maxBound :: Word)
     in Ext{value = result, overflow, carry}

class ByteSize t where
    byteSize :: t -> Int

instance ByteSize Word32 where
    byteSize _ = 4

instance ByteSize Int8 where
    byteSize _ = 1

instance ByteSize Int32 where
    byteSize _ = 4

class ByteSizeT t where
    byteSizeT :: Int

instance (ByteSize t, Default t) => ByteSizeT t where
    byteSizeT = byteSize (def :: t)

class InitState mem st | st -> mem where
    initState :: Int -> mem -> st

class StateInterspector st m isa w | st -> m isa w where
    programCounter :: st -> Int
    memoryDump :: st -> m
    ioStreams :: st -> IntMap ([w], [w])
    reprState :: HashMap String w -> st -> Text -> Text
    reprState _labels _st var = "unknown variable: " <> var

class Machine st isa w | st -> isa w where
    instructionFetch :: State st (Either Text (Int, isa))
    instructionStep :: State st ()
    instructionStep = do
        (pc, instruction) <- either (error . ("internal error: " <>)) id <$> instructionFetch
        instructionExecute pc instruction
    instructionExecute :: Int -> isa -> State st ()

halted :: Text
halted = "halted"

data Trace st isa
    = TState st
    | TError Text
    | TWarn Text
    deriving (Show)

data Mem isa w = Mem
    { memorySize :: Int
    , memoryData :: IntMap (Cell isa w)
    }
    deriving (Eq, Show)

data IoMem isa w = IoMem
    { mIoStreams :: IntMap ([w], [w])
    , mIoCells :: Mem isa w
    , mIoKeys :: [Int]
    , mIoByteToWord :: IntMap Int
    , mSeed :: Int
    }
    deriving (Eq, Show)

mkIoMem :: forall w isa. (ByteSizeT w) => IntMap ([w], [w]) -> Mem isa w -> Maybe Int -> IoMem isa w
mkIoMem streams cells (Just v) =
    IoMem
        { mIoStreams = streams
        , mIoCells = cells
        , mIoKeys = keys streams
        , mIoByteToWord =
            fromList $ concatMap (\i -> map (,i) [i .. i + byteSizeT @w - 1]) (keys streams)
        , mSeed = v
        }
mkIoMem streams cells _ = mkIoMem streams cells (Just 0)

data Cell isa w
    = Instruction isa
    | InstructionPart
    | Value Word8
    deriving (Eq, Show)
