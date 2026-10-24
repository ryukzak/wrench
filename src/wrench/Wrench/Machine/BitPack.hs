{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

{- | Packing values into bits of a stream of machine words.

A layout is a sequence of fields, each a number of bits, read and written
most significant bit first; a field may cross a word boundary, and 'word'
groups the fields that make up exactly one word. 'BitGet' asks for the next
word only when a field reaches it, so the layout itself decides how many words
are read. A failure names where it happened through 'label' \/ '<?>'.
-}
module Wrench.Machine.BitPack (
    -- * Packing
    BitPack (..),
    packWords,
    unpackWords,

    -- * Monads
    BitGet,
    BitPut,

    -- * Fields
    getField,
    putField,
    getEnum,
    putEnum,
    getPad,
    putPad,

    -- * Structure
    Layout (..),
    (<?>),

    -- * Running
    runBitGetWith,
    runBitGet,
    runBitPut,
) where

import Control.Monad (ap, liftM)
import Control.Monad.Except (throwError)
import Data.Bits (finiteBitSize, shiftL, shiftR, (.&.), (.|.))
import Data.String.Interpolate (i)
import Relude
import Relude.Extra (safeToEnum)
import Wrench.Machine.Types

-- * Packing

-- | A type with a bit layout of its own, so layouts compose by type:
-- @tag <- unpack@ inside a larger layout.
class BitPack a where
    unpack :: (IsWord w) => BitGet w a
    pack :: (IsWord w) => a -> BitPut w ()

-- | Pack a value into whole words.
packWords :: forall w a. (BitPack a, IsWord w) => a -> Either Text [w]
packWords = runBitPut . pack

-- | Unpack a value from exactly these words; fails if words are left over.
unpackWords :: forall a w. (BitPack a, IsWord w) => [w] -> Either Text a
unpackWords words_ = do
    (a, rest) <- runBitGetRest unpack words_
    unless (null rest) $ Left [i|words left over after decoding: #{length rest}|]
    return a

-- * Monads

data GetStep w a
    = Done a
    | NeedWord (Maybe w -> GetStep w a)
    | Fail Text

data GetState w = GetState
    { gsWord :: w
    , gsUsed :: Int
    }

newtype BitGet w a = BitGet (GetState w -> GetStep w (a, GetState w))

instance Functor (BitGet w) where
    fmap = liftM

instance Applicative (BitGet w) where
    pure a = BitGet $ \st -> Done (a, st)
    (<*>) = ap

instance Monad (BitGet w) where
    BitGet g >>= f = BitGet $ \st -> continue (g st)
        where
            continue (Done (a, st')) = let BitGet h = f a in h st'
            continue (NeedWord k) = NeedWord (continue . k)
            continue (Fail err) = Fail err

data PutState w = PutState
    { psWritten :: [w]
    , psWord :: w
    , psUsed :: Int
    }

newtype BitPut w a = BitPut (StateT (PutState w) (Either Text) a)
    deriving (Applicative, Functor, Monad)

-- * Fields

-- | The next @n@ bits, unsigned.
getField :: forall w. (IsWord w) => Int -> BitGet w Int
getField n
    | n < 0 || n > maxFieldBits = BitGet $ const $ Fail [i|field width #{n} is outside 0..#{maxFieldBits}|]
    | otherwise = BitGet $ go n 0
    where
        go 0 acc st = Done (acc, st)
        go k acc GetState{gsWord, gsUsed}
            | gsUsed == wordBits @w = NeedWord $ resume k acc
            | otherwise =
                let taken = min k (wordBits @w - gsUsed)
                    bits = (unsigned gsWord `shiftR` (wordBits @w - gsUsed - taken)) .&. mask taken
                 in go (k - taken) ((acc `shiftL` taken) .|. bits) GetState{gsWord, gsUsed = gsUsed + taken}
        resume _ _ Nothing = Fail "out of words"
        resume k acc (Just w) = go k acc GetState{gsWord = w, gsUsed = 0}

-- | Write @value@ into the next @n@ bits.
putField :: forall w. (IsWord w) => Int -> Int -> BitPut w ()
putField n value
    | n < 0 || n > maxFieldBits = BitPut $ throwError [i|field width #{n} is outside 0..#{maxFieldBits}|]
    | value < 0 || value > mask n = BitPut $ throwError [i|#{value} doesn't fit in #{n} bits|]
    | otherwise = BitPut $ go n
    where
        go 0 = return ()
        go k = do
            PutState{psWritten, psWord, psUsed} <- get
            let taken = min k (wordBits @w - psUsed)
                bits = (value `shiftR` (k - taken)) .&. mask taken
                word' = psWord .|. (fromIntegral bits `shiftL` (wordBits @w - psUsed - taken))
                used' = psUsed + taken
            put $
                if used' == wordBits @w
                    then PutState{psWritten = word' : psWritten, psWord = 0, psUsed = 0}
                    else PutState{psWritten, psWord = word', psUsed = used'}
            go (k - taken)

-- | The next @n@ bits as a constructor of @e@; fails on a value no
-- constructor has.
getEnum :: forall e w. (Bounded e, Enum e, IsWord w) => Int -> BitGet w e
getEnum n = do
    value <- getField n
    maybe (BitGet $ const $ Fail [i|unknown tag #{value}|]) return (safeToEnum value)

-- | Write a constructor of @e@ into the next @n@ bits.
putEnum :: (Enum e, IsWord w) => Int -> e -> BitPut w ()
putEnum n = putField n . fromEnum

-- | Skip @n@ bits, any number of them.
getPad :: (IsWord w) => Int -> BitGet w ()
getPad = mapM_ getField . padChunks

-- | @n@ zero bits, any number of them.
putPad :: (IsWord w) => Int -> BitPut w ()
putPad = mapM_ (`putField` 0) . padChunks

-- * Structure

-- | Combinators that work in both directions.
class Layout f where
    -- | Run a layout that must cover exactly one word, starting on a word
    -- boundary.
    word :: f a -> f a

    -- | Prefix a failure with where it happened; nested labels read
    -- outermost first.
    label :: Text -> f a -> f a

instance (IsWord w) => Layout (BitGet w) where
    word (BitGet inner) = BitGet $ \st ->
        if gsUsed st /= wordBits @w
            then Fail "word layout doesn't start on a word boundary"
            else go (0 :: Int) (inner st)
        where
            go n (NeedWord k) = NeedWord (go (n + 1) . k)
            go n (Done (a, st'))
                | n == 1 && gsUsed st' == wordBits @w = Done (a, st')
                | otherwise = Fail [i|word layout doesn't fill exactly one #{wordBits @w}-bit word|]
            go _ (Fail err) = Fail err
    label name (BitGet g) = BitGet $ relabel . g
        where
            relabel (Fail err) = Fail $ name <> ": " <> err
            relabel (NeedWord k) = NeedWord (relabel . k)
            relabel done = done

instance (IsWord w) => Layout (BitPut w) where
    word (BitPut inner) = BitPut $ do
        before <- get
        unless (psUsed before == 0) $
            throwError "word layout doesn't start on a word boundary"
        a <- inner
        after <- get
        unless (psUsed after == 0 && length (psWritten after) == length (psWritten before) + 1) $
            throwError [i|word layout doesn't fill exactly one #{wordBits @w}-bit word|]
        return a
    label name (BitPut body) = BitPut $ StateT $ first ((name <> ": ") <>) . runStateT body

infixl 1 <?>

-- | @layout <?> name@ is 'label' with the name last, after the field it names.
(<?>) :: (Layout f) => f a -> Text -> f a
(<?>) = flip label

-- * Running

-- | Decode, fetching word @k@ of the stream with @fetch k@ when the layout
-- reaches it.
runBitGetWith :: forall w m a. (IsWord w, Monad m) => (Int -> m w) -> BitGet w a -> m (Either Text a)
runBitGetWith fetch (BitGet body) = go 0 $ body emptyGetState
    where
        go _ (Done (a, _)) = return $ Right a
        go _ (Fail err) = return $ Left err
        go k (NeedWord next) = fetch k >>= go (k + 1) . next . Just

-- | Decode a list of words; fails if the layout runs past them.
runBitGet :: forall w a. (IsWord w) => BitGet w a -> [w] -> Either Text a
runBitGet decoder = fmap fst . runBitGetRest decoder

runBitGetRest :: forall w a. (IsWord w) => BitGet w a -> [w] -> Either Text (a, [w])
runBitGetRest (BitGet body) words_ = go words_ $ body emptyGetState
    where
        go rest (Done (a, _)) = Right (a, rest)
        go _ (Fail err) = Left err
        go rest (NeedWord next) = case rest of
            [] -> go [] $ next Nothing
            w : rest' -> go rest' $ next $ Just w

emptyGetState :: forall w. (IsWord w) => GetState w
emptyGetState = GetState{gsWord = 0, gsUsed = wordBits @w}

emptyPutState :: (IsWord w) => PutState w
emptyPutState = PutState{psWritten = [], psWord = 0, psUsed = 0}

-- | Pack the fields into words; fails if a value doesn't fit its field or the
-- fields don't fill whole words.
runBitPut :: forall w. (IsWord w) => BitPut w () -> Either Text [w]
runBitPut (BitPut body) = do
    PutState{psWritten, psUsed} <- executingStateT emptyPutState body
    unless (psUsed == 0) $
        Left [i|#{length psWritten * wordBits @w + psUsed} bits don't fill whole #{wordBits @w}-bit words|]
    return $ reverse psWritten

-- * Helpers

wordBits :: forall w. (IsWord w) => Int
wordBits = finiteBitSize (0 :: w)

unsigned :: (IsWord w) => w -> Int
unsigned = fromIntegral . fromSign

-- | The widest field whose value fits a non-negative 'Int'.
maxFieldBits :: Int
maxFieldBits = finiteBitSize (0 :: Int) - 1

padChunks :: Int -> [Int]
padChunks n = replicate (n `div` maxFieldBits) maxFieldBits <> [n `mod` maxFieldBits | n `mod` maxFieldBits > 0]

mask :: Int -> Int
mask n = (1 `shiftL` n) - 1
