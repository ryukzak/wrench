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
    arithmAnd,
) where

import Data.Bits
import Data.Default (Default)
import Relude

-- * State

type MachineWord w =
    ( Bits w
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
    )

type RegisterId r = (Hashable r, Show r, Read r)

class FromSign w where
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

arithmAnd :: (MachineWord w) => w -> w -> w
arithmAnd x mask
    | x < 0 = x .|. complement mask
    | otherwise = x .&. mask

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
