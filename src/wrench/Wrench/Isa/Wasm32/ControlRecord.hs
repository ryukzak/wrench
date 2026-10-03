{- |
The control-record binary format: the typed payload ('Scope') and pure
(de)serialization to\/from the words one record occupies on the control
stack. Stateful operations -- push, read, close -- live in
"Wrench.Isa.Wasm32" itself, as does the description of where the control
stack sits in memory.

Each open @block@, @loop@ or call is one record, written at the moment the
scope is entered. Every kind is the same width whatever it needs to store:

* a @block@ records where its own matching @end@ is (@csEnd@), so a branch
  to it knows where to jump and its @end@ knows it is the one that closes
  it;
* a @loop@ records both its re-entry point (@csStart@, just after the
  @loop@ instruction) and its @csEnd@;
* a call records the caller's @frameBase@ and local count, to restore on
  return, plus its own return address and declared result count.

A @block@ leaves its second word unused. Paying that word is what makes the
control stack a fixed-stride array: the record @n@ scopes out starts exactly
@n + 1@ strides below the frontier. That is why a branch finds its target by
arithmetic -- no chain of stored links to walk, and no memory read just to
discover how wide the next record is.
-}
module Wrench.Isa.Wasm32.ControlRecord (
    -- * Layout
    -- $layout
    Scope (..),
    scopeRecordWords,
    scopeRecordBytes,
    scopeWordAddrs,
    encodeScope,
    decodeScope,
    nullAddr,
    describeScope,
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Relude
import Relude.Extra (safeToEnum)
import Wrench.Machine.Types

-- | One control record's payload -- a typed shape per kind, so a
-- mismatch between kind and payload can't be constructed.
--
-- Every record is the same 'scopeRecordWords' words whatever its kind,
-- making the control stack a fixed-stride array: the record @n@ scopes
-- out starts exactly @n+1@ strides below the frontier. That is what lets
-- @br@ reach its target by arithmetic rather than a chain of stored
-- links, and why no record describes its own predecessor.
--
-- 'LoopScope' and 'BlockScope' both carry @csEnd@: a plain @end@
-- compares its own address against it to tell "closes this scope" from
-- "closes an @if@" (only the former pops).
data Scope
    = -- | Reaching this (via @br@\/@br_if@) leaves it open: jump to
      -- @csStart@ (just after @loop@) to run the body again.
      LoopScope {csStart :: Int, csEnd :: Int}
    | -- | Reaching this closes it: jump just past @csEnd@, popping it
      -- off the stack -- unlike a loop, nothing left to continue.
      BlockScope {csEnd :: Int}
    | -- | Pushed by @call@, closed by @return@ only. @csCallerFrameBase@\/
      -- @csCallerLocalCount@ are the caller's own values, restored on
      -- return; @csReturnPc@\/@csResultCount@ drive where to jump back
      -- to and how many values to carry across.
      CallScope {csCallerFrameBase :: Int, csCallerLocalCount :: Int, csReturnPc :: Int, csResultCount :: Int}
    deriving (Eq, Show)

-- | How many machine words one record occupies, whatever its kind. A
-- @block@ leaves its second word unused; paying that word buys a
-- fixed-stride control stack (see 'Scope').
scopeRecordWords :: Int
scopeRecordWords = 2

-- | How many bytes one record occupies.
scopeRecordBytes :: forall w. (IsWord w) => Int
scopeRecordBytes = scopeRecordWords * byteSizeT @w

-- | The word addresses one record at @addr@ occupies, in order -- what
-- both the interpreter and the report views read to decode it.
scopeWordAddrs :: forall w. (IsWord w) => Int -> [Int]
scopeWordAddrs addr = [addr + i * byteSizeT @w | i <- [0 .. scopeRecordWords - 1]]

-- | The persisted tag, kept separate from 'Scope' so the typed shape
-- never has to double as the stored representation. Three kinds in two
-- bits leaves one bit pattern unused, which 'decodeScope' rejects.
data ControlTag = LoopTag | BlockTag | CallTag
    deriving (Bounded, Enum, Eq, Show)

tagOfScope :: Scope -> ControlTag
tagOfScope LoopScope{} = LoopTag
tagOfScope BlockScope{} = BlockTag
tagOfScope CallScope{} = CallTag

-- | See the @Layout@ section at the foot of this module for the field
-- layout these widths produce.
--
-- Every address field is the same 'addrBitWidth' in the same bits, and
-- every count the same 'countBitWidth' in theirs: no part of the address
-- space deserves more trust than another, and a uniform layout is one
-- decoder in hardware rather than three.
tagBitWidth, addrBitWidth, countBitWidth :: Int
tagBitWidth = 2
addrBitWidth = 22
countBitWidth = 8

tagShift, countShift :: Int
countShift = addrBitWidth
tagShift = addrBitWidth + countBitWidth

bitMask :: Int -> Int
bitMask n = (1 `shiftL` n) - 1

-- | Pack one field of @width@ bits at bit offset @shiftAmt@, failing
-- rather than truncating a value that doesn't fit. Masking off a high
-- bit doesn't make a smaller number, it makes a different one: a
-- truncated @csReturnPc@ returns to the wrong address and a truncated
-- @csResultCount@ pops the wrong number of values, both silently.
packField :: Text -> Int -> Int -> Int -> Either Text Int
packField name width shiftAmt value
    | value < 0 || value > bitMask width =
        Left $ name <> " " <> show value <> " doesn't fit in " <> show width <> " bits"
    | otherwise = Right (value `shiftL` shiftAmt)

-- | The inverse of 'packField'. Total: anything a field's own bits can
-- hold is a value that field can mean.
unpackField :: Int -> Int -> Int -> Int
unpackField width shiftAmt word = (word `shiftR` shiftAmt) .&. bitMask width

packAddr, packCount :: Text -> Int -> Either Text Int
packAddr name = packField name addrBitWidth 0
packCount name = packField name countBitWidth countShift

unpackAddr, unpackCount :: Int -> Int
unpackAddr = unpackField addrBitWidth 0
unpackCount = unpackField countBitWidth countShift

packTag :: ControlTag -> Int
packTag = (`shiftL` tagShift) . fromEnum

-- | A record's header packs its tag into the top bits, so the raw bit
-- pattern often doesn't fit @w@'s *signed* range even though it's a
-- valid word -- plain 'toEnum'\/'fromEnum' would reject or corrupt it.
-- These two treat it as the unsigned bit pattern it actually is.
wordFromBits :: forall w. (IsWord w) => Int -> w
wordFromBits = toSign . fromIntegral

bitsFromWord :: forall w. (IsWord w) => w -> Int
bitsFromWord = fromIntegral . fromSign

-- | 'Left' when a field's value doesn't fit the bits the format gives
-- it -- see 'packField'.
encodeScope :: forall w. (IsWord w) => Scope -> Either Text [w]
encodeScope scope =
    map wordFromBits <$> case scope of
        BlockScope{csEnd} -> do
            end <- packAddr "csEnd" csEnd
            return [tag .|. end, 0]
        LoopScope{csStart, csEnd} -> do
            start <- packAddr "csStart" csStart
            end <- packAddr "csEnd" csEnd
            return [tag .|. start, end]
        CallScope{csCallerFrameBase, csCallerLocalCount, csReturnPc, csResultCount} -> do
            frameBase <- packAddr "csCallerFrameBase" csCallerFrameBase
            localCount <- packCount "csCallerLocalCount" csCallerLocalCount
            returnPc <- packAddr "csReturnPc" csReturnPc
            resultCount <- packCount "csResultCount" csResultCount
            return [tag .|. localCount .|. frameBase, resultCount .|. returnPc]
    where
        tag = packTag (tagOfScope scope)

-- | The inverse of 'encodeScope'. 'Left' for a word pair that doesn't
-- decode as any real record -- the one unused tag pattern, or the wrong
-- number of words -- rather than guessing at a shape, since the only way
-- to see one is a control stack something has corrupted.
decodeScope :: forall w. (IsWord w) => [w] -> Either Text Scope
decodeScope words_
    | [word0, word1] <- words_
    , let bits0 = bitsFromWord word0
    , let bits1 = bitsFromWord word1
    , let tag = unpackField tagBitWidth tagShift bits0 =
        -- Matched over the tag's own constructors rather than against
        -- 'fromEnum' of each: a fourth record kind is then a
        -- missing-pattern warning here, not a bit pattern this silently
        -- reports as corruption.
        case safeToEnum tag of
            Just BlockTag -> Right BlockScope{csEnd = unpackAddr bits0}
            Just LoopTag -> Right LoopScope{csStart = unpackAddr bits0, csEnd = unpackAddr bits1}
            Just CallTag ->
                Right
                    CallScope
                        { csCallerFrameBase = unpackAddr bits0
                        , csCallerLocalCount = unpackCount bits0
                        , csReturnPc = unpackAddr bits1
                        , csResultCount = unpackCount bits1
                        }
            Nothing -> Left $ "no record kind has tag " <> show tag
    | otherwise =
        Left $ "expected " <> show scopeRecordWords <> " words, got " <> show (length words_)

-- | Sentinel for "no enclosing control record" and "no active function
-- frame".
nullAddr :: Int
nullAddr = -1

-- | A Haskell-record-literal rendering of a scope, close to how 'Scope'
-- itself reads in source -- addresses follow @showAddr@'s format, plain
-- counts stay decimal.
describeScope :: (Int -> Text) -> Scope -> Text
describeScope showAddr scope = case scope of
    LoopScope{csStart, csEnd} ->
        "LoopScope { csStart = " <> showAddr csStart <> ", csEnd = " <> showAddr csEnd <> " }"
    BlockScope{csEnd} ->
        "BlockScope { csEnd = " <> showAddr csEnd <> " }"
    CallScope{csCallerFrameBase, csCallerLocalCount, csReturnPc, csResultCount} ->
        "CallScope { csCallerFrameBase = "
            <> showAddr csCallerFrameBase
            <> ", csCallerLocalCount = "
            <> show csCallerLocalCount
            <> ", csReturnPc = "
            <> showAddr csReturnPc
            <> ", csResultCount = "
            <> show csResultCount
            <> " }"

-- $layout
--
-- The exact byte layout, in Erlang bit syntax -- illustrative notation
-- only, the project is Haskell, but a precise and standard way to say
-- exactly which bits are which. Most-significant field first:
--
-- > %% block -- 8 bytes
-- > <<Tag:2, 0:8, CsEnd:22>>, <<0:32>>
-- >
-- > %% loop -- 8 bytes
-- > <<Tag:2, 0:8, CsStart:22>>, <<0:10, CsEnd:22>>
-- >
-- > %% call -- 8 bytes
-- > <<Tag:2, CsCallerLocalCount:8, CsCallerFrameBase:22>>, <<0:2, CsResultCount:8, CsReturnPc:22>>
--
-- Every address-shaped field -- @CsStart@, @CsEnd@ and @CsReturnPc@ (code
-- addresses) and @CsCallerFrameBase@ (a stack address) -- gets the same
-- uniform 'addrBitWidth' of 22 bits, and every count the same
-- 'countBitWidth' of 8, in the same bit positions. There is no ISA-level
-- reason to trust one part of the address space more than another, and a
-- uniform layout means one decoder rather than three. 22 bits covers 4 MiB,
-- far past the 64 KiB memory limit; 8 bits matches the one-byte counts
-- @call@ itself carries, so what a call site can declare and what its
-- record can hold agree exactly.
--
-- Nothing is silently truncated. A field value that does not fit stops the
-- program naming the field ('packField'), and an instruction immediate out
-- of range for its own encoding is rejected at translate time.
--
-- @Tag@ needs only 2 bits for three kinds. The one unused pattern is not a
-- valid record, and reading it reports a corrupted control stack rather
-- than guessing at a shape -- see 'decodeScope'.
