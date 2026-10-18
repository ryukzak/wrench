{-# LANGUAGE QuasiQuotes #-}

{- |
The machine's state and every primitive that touches it: where code, data and
the two stacks sit in memory, the counted word\/byte accessors, and the
operand and control stacks -- the latter including the binary format of a
control record, since the format and the LIFO discipline that gives it meaning
are one thing.
-}
module Wrench.Isa.Wasm32.State (
    -- * The state
    Wasm32St (..),

    -- * Memory map
    defaultStackRoot,

    -- * Memory access
    getWord,
    setWord,
    fetchInstructionAt,
    getByte,
    setByte,
    setPc,
    nextPc,
    raiseInternalError,

    -- * Operand stack
    pushValue,
    popValue,

    -- * Locals
    localAddr,
    reserveLocals,

    -- * Scan cache
    scanCacheSize,
    cachedScan,

    -- * Branch cache
    BranchEntry (..),
    branchCacheSize,
    cachedBranch,
    rememberBranch,

    -- * Control stack
    Scope (..),
    pushScope,
    retireScopes,
    scopeAtDepth,
    scopeRecordAtDepth,
    innermostCall,
    ctrlCorruptedErr,

    -- * Control-record encoding
    scopeAddrs,
    scopeRecordBytes,
    scopeWordAddrs,
    decodeScope,
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Default (def)
import Data.String.Interpolate (i)
import Relude
import Relude.Extra (safeToEnum)
import Wrench.Isa.Wasm32.Instruction
import Wrench.Machine.Memory
import Wrench.Machine.Types

-- | The machine state.
data Wasm32St w = Wasm32St
    { pc :: Int
    -- ^ Address of the next instruction.
    , sp :: Int
    -- ^ Address of the topmost operand.
    , frameBase :: Int
    -- ^ Address of local 0 of the current frame.
    , localCount :: Int
    -- ^ How many locals the current frame has: its parameters plus
    -- whatever @locals.reserve@ added.
    , ctrlSp :: Int
    -- ^ Top of the control stack.
    , stackRoot :: Int
    -- ^ Where both stacks start. Set by @sp.init@; by default three
    -- quarters of the way into memory.
    , scanCache :: [(Int, Int)]
    -- ^ The last 'scanCacheSize' scans for a matching @else@ or @end@, most
    -- recently used first: where each scan started and where it ended.
    , branchCache :: [BranchEntry w]
    -- ^ The last 'branchCacheSize' branches taken, most recently used first.
    , mem :: IoMem (Wasm32Isa w w) w
    -- ^ Memory, including the memory-mapped I\/O ports.
    , stopped :: Bool
    -- ^ Set by @halt@.
    , internalError :: Maybe Text
    -- ^ The error that stopped the machine, if any.
    }
    deriving (Show)

type instance MemOf (Wasm32St w) = IoMem (Wasm32Isa w w) w

defaultStackRoot :: IoMem (Wasm32Isa w w) w -> Int
defaultStackRoot IoMem{mIoCells = Mem{memorySize}} = (memorySize `div` 4) * 3

setPc :: Int -> State (Wasm32St w) ()
setPc addr = modify $ \st -> st{pc = addr}

nextPc :: Wasm32Isa w w -> State (Wasm32St w) ()
nextPc instruction = do
    Wasm32St{pc} <- get
    setPc (pc + byteSize instruction)

fetchInstructionAt :: (IsWord w) => Int -> State (Wasm32St w) (Either Text (Wasm32Isa w w))
fetchInstructionAt addr = do
    st@Wasm32St{mem} <- get
    case readInstruction mem addr of
        Right (mem', instruction) -> do
            put st{mem = mem'}
            return $ Right instruction
        Left err -> return $ Left err

raiseInternalError :: Text -> State (Wasm32St w) ()
raiseInternalError msg = modify $ \st -> st{internalError = Just msg}

getWord :: (IsWord w) => Int -> State (Wasm32St w) w
getWord addr = do
    st@Wasm32St{mem} <- get
    case readWord mem addr of
        Right (mem', w) -> put st{mem = mem'} >> return w
        Left err -> do
            raiseInternalError [i|memory access error: #{err}|]
            return def

setWord :: forall w. (IsWord w) => Int -> w -> State (Wasm32St w) ()
setWord addr w = do
    st@Wasm32St{mem} <- get
    case writeWord mem addr w of
        Right mem' -> put st{mem = mem'} >> forgetChangedBranches addr (byteSizeT @w) (Just w)
        Left err -> raiseInternalError [i|memory access error: #{err}|]

getByte :: (IsWord w) => Int -> State (Wasm32St w) Word8
getByte addr = do
    st@Wasm32St{mem} <- get
    case readByte mem addr of
        Right (mem', b) -> put st{mem = mem'} >> return b
        Left err -> do
            raiseInternalError [i|memory access error: #{err}|]
            return 0

setByte :: (IsWord w) => Int -> Word8 -> State (Wasm32St w) ()
setByte addr b = do
    st@Wasm32St{mem} <- get
    case writeByte mem addr b of
        Right mem' -> put st{mem = mem'} >> forgetChangedBranches addr 1 Nothing
        Left err -> raiseInternalError [i|memory access error: #{err}|]

-- * Data stack

pushValue :: forall w. (IsWord w) => w -> State (Wasm32St w) ()
pushValue value = do
    Wasm32St{sp} <- get
    let sp' = sp - byteSizeT @w
    modify $ \st -> st{sp = sp'}
    setWord sp' value

popValue :: forall w. (IsWord w) => State (Wasm32St w) w
popValue = do
    Wasm32St{sp} <- get
    value <- getWord sp
    modify $ \st -> st{sp = sp + byteSizeT @w}
    return value

reserveLocals :: forall w. (IsWord w) => Int -> State (Wasm32St w) ()
reserveLocals n = do
    Wasm32St{sp} <- get
    let sp' = sp - n * byteSizeT @w
    modify $ \st ->
        st
            { sp = sp'
            , localCount = localCount st + n
            }

localAddr :: forall w. (IsWord w) => Int -> State (Wasm32St w) Int
localAddr idx = do
    Wasm32St{frameBase} <- get
    return $ frameBase - idx * byteSizeT @w

-- * Scan cache

scanCacheSize :: Int
scanCacheSize = 4

cachedScan :: Int -> State (Wasm32St w) (Either Text Int) -> State (Wasm32St w) (Either Text Int)
cachedScan start scan = do
    Wasm32St{scanCache} <- get
    result <- maybe scan (return . Right) (listToMaybe [end | (s, end) <- scanCache, s == start])
    whenRight_ result $ \end ->
        modify $ \st -> st{scanCache = take scanCacheSize ((start, end) : filter ((/= start) . fst) scanCache)}
    return result

-- * Branch cache

-- | A taken @br@ or @br_if@: where it was, the control record it read and
-- that record's words, and where it went.
data BranchEntry w = BranchEntry
    { beFrom :: Int
    , beRecord :: Int
    , beWords :: [w]
    , beTo :: Int
    , beCtrlSp :: Int
    }
    deriving (Show)

branchCacheSize :: Int
branchCacheSize = 4

cachedBranch :: (IsWord w) => Int -> Int -> State (Wasm32St w) (Maybe (BranchEntry w))
cachedBranch from record = do
    Wasm32St{branchCache} <- get
    let hit = find (\BranchEntry{beFrom, beRecord} -> beFrom == from && beRecord == record) branchCache
    whenJust hit rememberBranch
    return hit

rememberBranch :: forall w. (IsWord w) => BranchEntry w -> State (Wasm32St w) ()
rememberBranch entry@BranchEntry{beFrom, beRecord} = do
    Wasm32St{mem} <- get
    unless (any (`elem` ioPorts mem) (scopeWordAddrs @w beRecord)) $
        modify $
            \st -> st{branchCache = take branchCacheSize (entry : filter (not . same) (branchCache st))}
    where
        same BranchEntry{beFrom = f, beRecord = r} = f == beFrom && r == beRecord

forgetChangedBranches :: forall w. (IsWord w) => Int -> Int -> Maybe w -> State (Wasm32St w) ()
forgetChangedBranches addr size value =
    modify $ \st -> st{branchCache = filter unchanged (branchCache st)}
    where
        unchanged BranchEntry{beRecord, beWords} = and $ zipWith keeps (scopeWordAddrs @w beRecord) beWords
        keeps wordAddr old
            | wordAddr + byteSizeT @w <= addr || addr + size <= wordAddr = True
            | otherwise = wordAddr == addr && value == Just old

-- * Control stack

data Scope
    = LoopScope
        { csStart :: Int
        , csEnd :: Int
        }
    | BlockScope {csEnd :: Int}
    | CallScope
        { csCallerFrameBase :: Int
        , csCallerLocalCount :: Int
        , csReturnPc :: Int
        , csResultCount :: Int
        }
    deriving (Eq, Show)

data ControlTag = LoopTag | BlockTag | CallTag
    deriving (Bounded, Enum, Eq, Show)

scopeAddrs :: forall w. (IsWord w) => Wasm32St w -> [Int]
scopeAddrs Wasm32St{ctrlSp, stackRoot} =
    takeWhile (>= stackRoot) [ctrlSp - stride, ctrlSp - 2 * stride ..]
    where
        stride = scopeRecordBytes @w

ctrlCorruptedErr :: Int -> Text -> Text
ctrlCorruptedErr addr err = [i|control stack corrupted at #{hexAddr 2 addr}: #{err}|]

pushScope :: forall w. (IsWord w) => Scope -> State (Wasm32St w) ()
pushScope scope = do
    Wasm32St{ctrlSp} <- get
    let step = byteSizeT @w
        ctrlSp' = ctrlSp + scopeRecordBytes @w
    case encodeScope scope of
        Left err -> raiseInternalError [i|control record doesn't fit: #{err}|]
        Right words_ -> do
            forM_ (zip [0 ..] words_) $ \(idx, word) -> setWord (ctrlSp + idx * step) word
            modify $ \st -> st{ctrlSp = ctrlSp'}

retireScopes :: forall w. (IsWord w) => Int -> State (Wasm32St w) ()
retireScopes n = modify $ \st ->
    st{ctrlSp = ctrlSp st - n * scopeRecordBytes @w}

scopeAtDepth :: forall w. (IsWord w) => Int -> State (Wasm32St w) (Maybe (Either Text Scope))
scopeAtDepth depth = fmap decodeAt <$> scopeRecordAtDepth depth
    where
        decodeAt (addr, words_) = first (ctrlCorruptedErr addr) (decodeScope words_)

scopeRecordAtDepth :: forall w. (IsWord w) => Int -> State (Wasm32St w) (Maybe (Int, [w]))
scopeRecordAtDepth depth = do
    st <- get
    case drop depth (scopeAddrs st) of
        [] -> return Nothing
        addr : _ -> Just . (addr,) <$> mapM getWord (scopeWordAddrs @w addr)

innermostCall :: forall w. (IsWord w) => State (Wasm32St w) (Either Text (Int, Scope))
innermostCall = get >>= go . scopeAddrs
    where
        go [] = return $ Left "return without active function frame"
        go (addr : outer) = do
            word0 <- getWord addr
            case decodeScopeTag word0 of
                Left err -> return $ Left $ ctrlCorruptedErr addr err
                Right CallTag -> do
                    rest <- mapM getWord (drop 1 (scopeWordAddrs @w addr))
                    return $ case decodeScope (word0 : rest) of
                        Left err -> Left $ ctrlCorruptedErr addr err
                        Right scope -> Right (addr, scope)
                Right _ -> go outer

scopeRecordWords :: Int
scopeRecordWords = 2

scopeRecordBytes :: forall w. (IsWord w) => Int
scopeRecordBytes = scopeRecordWords * byteSizeT @w

scopeWordAddrs :: forall w. (IsWord w) => Int -> [Int]
scopeWordAddrs addr = [addr + idx * byteSizeT @w | idx <- [0 .. scopeRecordWords - 1]]

decodeScopeTag :: (IsWord w) => w -> Either Text ControlTag
decodeScopeTag word0 =
    maybe (Left [i|no record kind has tag #{tag}|]) Right (safeToEnum tag)
    where
        tag = unpackField tagBitWidth tagShift (bitsFromWord word0)

tagBitWidth, addrBitWidth, countBitWidth :: Int
tagBitWidth = 2
addrBitWidth = 22
countBitWidth = 8

tagShift, countShift :: Int
countShift = addrBitWidth
tagShift = addrBitWidth + countBitWidth

bitMask :: Int -> Int
bitMask n = (1 `shiftL` n) - 1

packField :: Text -> Int -> Int -> Int -> Either Text Int
packField name width shiftAmt value
    | value < 0 || value > bitMask width =
        Left [i|#{name} #{value} doesn't fit in #{width} bits|]
    | otherwise = Right (value `shiftL` shiftAmt)

unpackField :: Int -> Int -> Int -> Int
unpackField width shiftAmt word = (word `shiftR` shiftAmt) .&. bitMask width

packAddr, packCount :: Text -> Int -> Either Text Int
packAddr name = packField name addrBitWidth 0
packCount name = packField name countBitWidth countShift

unpackAddr, unpackCount :: Int -> Int
unpackAddr = unpackField addrBitWidth 0
unpackCount = unpackField countBitWidth countShift

wordFromBits :: (IsWord w) => Int -> w
wordFromBits = toSign . fromIntegral

bitsFromWord :: (IsWord w) => w -> Int
bitsFromWord = fromIntegral . fromSign

encodeScope :: (IsWord w) => Scope -> Either Text [w]
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
        tag =
            (`shiftL` tagShift) $
                fromEnum $ case scope of
                    LoopScope{} -> LoopTag
                    BlockScope{} -> BlockTag
                    CallScope{} -> CallTag

decodeScope :: (IsWord w) => [w] -> Either Text Scope
decodeScope words_
    | [word0, word1] <- words_
    , let bits0 = bitsFromWord word0
    , let bits1 = bitsFromWord word1
    , let tag = unpackField tagBitWidth tagShift bits0 =
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
            Nothing -> Left [i|no record kind has tag #{tag}|]
    | otherwise =
        Left [i|expected #{scopeRecordWords} words, got #{length words_}|]
