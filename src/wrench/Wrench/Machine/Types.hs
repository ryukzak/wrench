module Wrench.Machine.Types (
    Trace (..),
    Machine (..),
    Mem (..),
    IoMem (..),
    mkIoMem,
    Cell (..),
    InitState (..),
    StateInterspector (..),
    Intervals (..),
    emptyIntervals,
    recordRange,
    renderIntervals,
    renderIntervalsHex,
    inIntervals,
    intervalsSize,
    intervalsToList,
    intervalsRange,
    intervalsIntersect,
    intervalsUnion,
    intervalsDifference,
    AccessLog (..),
    emptyAccessLog,
    MachineWord,
    FromSign (..),
    RegisterId,
    ByteSize (..),
    ByteSizeT (..),
    WordParts (..),
    halted,
) where

import Data.Bits
import Data.Default (Default, def)
import Data.Interval qualified as I
import Data.IntervalSet (IntervalSet)
import Data.IntervalSet qualified as IS
import Data.Text qualified as T
import Numeric (showHex)
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
    initState :: Int -> mem -> [Int] -> st

class StateInterspector st m isa w | st -> m isa w where
    programCounter :: st -> Int
    memoryDump :: st -> m
    ioStreams :: st -> IntMap ([w], [w])
    reprState :: HashMap Text w -> st -> Text -> Text
    reprState _labels _st var = "unknown variable: " <> var

    -- | Per-run summary views, resolved from the simulator's *final* state
    --   (not the per-state record in the trace log). Use this for stats that
    --   only make sense at end-of-run -- e.g. accumulators that grow each
    --   step, where the per-state value would be off by one. Returns
    --   'Nothing' when the variable isn't a summary view, in which case the
    --   resolver falls through to the per-state 'reprState'.
    summaryView :: HashMap Text w -> st -> Text -> Maybe Text
    summaryView _labels _st _var = Nothing

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
    = -- | A captured machine state, tagged with the 1-indexed instruction step
      --   number it sits before (i.e. the @sim:instruction-count@ value at this
      --   point in the trace).
      TState
        { tInstructionCount :: !Int
        , tState :: !st
        }
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
    , mAccessLog :: !AccessLog
    -- ^ Tracks the address ranges touched at runtime, surfaced via @mem:*@.
    }
    deriving (Eq, Show)

mkIoMem :: forall w isa. (ByteSizeT w) => IntMap ([w], [w]) -> Mem isa w -> IoMem isa w
mkIoMem streams cells =
    IoMem
        { mIoStreams = streams
        , mIoCells = cells
        , mIoKeys = keys streams
        , mAccessLog = emptyAccessLog
        , mIoByteToWord =
            fromList $ concatMap (\i -> map (,i) [i .. i + byteSizeT @w - 1]) (keys streams)
        }

data Cell isa w
    = Instruction isa
    | InstructionPart
    | Value Word8
    deriving (Eq, Show)

-----------------------------------------------------------
-- Address-range accounting (mem:* stats)

-- | Sorted, non-overlapping integer address ranges with adjacency merging.
--   Backed by 'IntervalSet' 'Integer' from the @data-interval@ package.
--
--   We store each access as the half-open interval @[lo, hi+1)@ so that two
--   integer-adjacent accesses (one ending at N, the next starting at N+1)
--   share a boundary and get merged by 'IS.insert'. On render we convert
--   back to the inclusive @"lo..hi"@ form by subtracting 1 from the upper.
newtype Intervals = Intervals {unIntervals :: IntervalSet Integer}
    deriving (Eq, Show)

emptyIntervals :: Intervals
emptyIntervals = Intervals IS.empty

-- | Record an access spanning @[addr .. addr+len-1]@. Length must be ≥ 1.
recordRange :: Int -> Int -> Intervals -> Intervals
recordRange addr len (Intervals s) =
    let lo = I.Finite (toInteger addr)
        hi = I.Finite (toInteger (addr + len))
     in Intervals (IS.insert (lo I.<=..< hi) s)

-- | Render intervals as @"lo1..hi1, lo2..hi2"@ (or @"-"@ when empty),
--   using the given per-address formatter for both bounds.
renderIntervalsWith :: (Integer -> Text) -> Intervals -> Text
renderIntervalsWith fmt (Intervals s) =
    case IS.toAscList s of
        [] -> "-"
        is -> T.intercalate ", " (map renderInterval is)
    where
        renderInterval i =
            let lo = case I.lowerBound i of I.Finite n -> n; _ -> error "Intervals: unexpected infinite lower bound"
                hi = case I.upperBound i of I.Finite n -> n - 1; _ -> error "Intervals: unexpected infinite upper bound"
             in fmt lo <> ".." <> fmt hi

-- | Decimal-formatted ranges.
renderIntervals :: Intervals -> Text
renderIntervals = renderIntervalsWith show

-- | Hex-formatted ranges (@0xNN@ lowercase, no padding).
renderIntervalsHex :: Intervals -> Text
renderIntervalsHex = renderIntervalsWith (\n -> "0x" <> toText (showHex n ""))

-- | Membership test: is @addr@ inside any interval?
inIntervals :: Int -> Intervals -> Bool
inIntervals addr (Intervals s) = IS.member (toInteger addr) s

-- | Total number of bytes covered by all intervals.
intervalsSize :: Intervals -> Int
intervalsSize (Intervals s) =
    fromInteger
        $ sum
            [ hi - lo
            | i <- IS.toAscList s
            , I.Finite lo <- [I.lowerBound i]
            , I.Finite hi <- [I.upperBound i]
            ]

-- | Convert intervals back to a list of inclusive @(lo, hi)@ pairs in
--   ascending order.
intervalsToList :: Intervals -> [(Int, Int)]
intervalsToList (Intervals s) =
    [ (fromInteger lo, fromInteger (hi - 1))
    | i <- IS.toAscList s
    , I.Finite lo <- [I.lowerBound i]
    , I.Finite hi <- [I.upperBound i]
    ]

-- | Build an 'Intervals' covering the inclusive range @[lo, hi]@.
intervalsRange :: Int -> Int -> Intervals
intervalsRange lo hi
    | hi < lo = emptyIntervals
    | otherwise = recordRange lo (hi - lo + 1) emptyIntervals

intervalsIntersect :: Intervals -> Intervals -> Intervals
intervalsIntersect (Intervals a) (Intervals b) = Intervals (IS.intersection a b)

intervalsUnion :: Intervals -> Intervals -> Intervals
intervalsUnion (Intervals a) (Intervals b) = Intervals (IS.union a b)

intervalsDifference :: Intervals -> Intervals -> Intervals
intervalsDifference (Intervals a) (Intervals b) = Intervals (IS.difference a b)

-- | Runtime access ranges accumulated by 'IoMem' while the program runs.
data AccessLog = AccessLog
    { alInstr :: !Intervals
    -- ^ Instruction-fetch addresses.
    , alData :: !Intervals
    -- ^ Data read/write addresses (merged — we don't distinguish direction).
    , alIo :: !Intervals
    -- ^ Memory-mapped IO addresses touched.
    }
    deriving (Eq, Show)

emptyAccessLog :: AccessLog
emptyAccessLog = AccessLog{alInstr = emptyIntervals, alData = emptyIntervals, alIo = emptyIntervals}
