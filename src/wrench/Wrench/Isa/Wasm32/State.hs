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
    cachedScan,

    -- * Branch cache
    BranchEntry (..),
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
) where

import Data.Default (def)
import Data.List (lookup)
import Data.String.Interpolate (i)
import Relude
import Wrench.Isa.Wasm32.Instruction
import Wrench.Machine.BitPack
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
    , scanCacheSize :: Int
    -- ^ How many scans 'scanCache' keeps.
    , branchCache :: [BranchEntry w]
    -- ^ The last 'branchCacheSize' branches taken, most recently used first.
    , branchCacheSize :: Int
    -- ^ How many branches 'branchCache' keeps.
    , tosRegs :: [(Int, w)]
    -- ^ The top-of-stack registers: copies of the last 'tosRegCount'
    -- values pushed and not yet popped, the last pushed first, each with
    -- the address of its slot.
    , tosRegCount :: Int
    -- ^ How many values 'tosRegs' holds.
    , mem :: IoMem (Wasm32Isa w w) w
    -- ^ Memory, including the memory-mapped I\/O ports.
    , ir :: [(Int, Wasm32Isa w w)]
    -- ^ Instruction register: the instructions of the last fetched words, by address.
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
    st@Wasm32St{mem, ir} <- get
    case lookup addr ir of
        Just instruction -> return $ Right instruction
        Nothing -> case readInstructions mem addr of
            Right (mem', fetched@((_, instruction) :| _)) -> do
                put st{mem = mem', ir = toList fetched}
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
        Right mem' -> put st{mem = mem'} >> forgetChangedBranches addr (byteSizeT @w) (Just w) >> forgetTos addr (byteSizeT @w)
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
        Right mem' -> put st{mem = mem'} >> forgetChangedBranches addr 1 Nothing >> forgetTos addr 1
        Left err -> raiseInternalError [i|memory access error: #{err}|]

-- * Data stack

pushValue :: forall w. (IsWord w) => w -> State (Wasm32St w) ()
pushValue value = do
    Wasm32St{sp} <- get
    let sp' = sp - byteSizeT @w
    modify $ \st -> st{sp = sp'}
    setWord sp' value
    Wasm32St{mem} <- get
    unless (sp' `elem` ioPorts mem) $
        modify $
            \st -> st{tosRegs = take (tosRegCount st) ((sp', value) : tosRegs st)}

popValue :: forall w. (IsWord w) => State (Wasm32St w) w
popValue = do
    Wasm32St{sp, tosRegs} <- get
    value <- case listToMaybe [v | (addr, v) <- tosRegs, addr == sp] of
        Just v -> forgetTos sp (byteSizeT @w) >> return v
        Nothing -> getWord sp
    modify $ \st -> st{sp = sp + byteSizeT @w}
    return value

forgetTos :: forall w. (IsWord w) => Int -> Int -> State (Wasm32St w) ()
forgetTos addr size = modify $ \st -> st{tosRegs = filter (not . overlaps . fst) (tosRegs st)}
    where
        overlaps slot = slot < addr + size && addr < slot + byteSizeT @w

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

cachedScan :: Int -> State (Wasm32St w) (Either Text Int) -> State (Wasm32St w) (Either Text Int)
cachedScan start scan = do
    Wasm32St{scanCache} <- get
    result <- maybe scan (return . Right) (listToMaybe [end | (s, end) <- scanCache, s == start])
    whenRight_ result $ \end ->
        modify $ \st -> st{scanCache = take (scanCacheSize st) ((start, end) : filter ((/= start) . fst) scanCache)}
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
            \st -> st{branchCache = take (branchCacheSize st) (entry : filter (not . same) (branchCache st))}
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
    case packWords scope of
        Left err -> raiseInternalError [i|control record doesn't fit: #{err}|]
        Right words_ -> do
            forM_ (zip [0 ..] words_) $ \(idx, w) -> setWord (ctrlSp + idx * step) w
            modify $ \st -> st{ctrlSp = ctrlSp'}

retireScopes :: forall w. (IsWord w) => Int -> State (Wasm32St w) ()
retireScopes n = modify $ \st ->
    st{ctrlSp = ctrlSp st - n * scopeRecordBytes @w}

scopeAtDepth :: forall w. (IsWord w) => Int -> State (Wasm32St w) (Maybe (Either Text Scope))
scopeAtDepth depth = fmap decodeAt <$> scopeRecordAtDepth depth
    where
        decodeAt (addr, words_) = first (ctrlCorruptedErr addr) (unpackWords words_)

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
            result <- readScope addr callGet
            case result of
                Left err -> return $ Left $ ctrlCorruptedErr addr err
                Right (Just scope) -> return $ Right (addr, scope)
                Right Nothing -> go outer
        callGet = do
            first_@(tag, _, _) <- getRecordHead
            if tag == CallTag then Just <$> getRecordTail first_ else return Nothing

readScope :: forall w a. (IsWord w) => Int -> BitGet w a -> State (Wasm32St w) (Either Text a)
readScope addr = runBitGetWith (\k -> getWord (addr + k * byteSizeT @w))

scopeRecordWords :: Int
scopeRecordWords = 2

scopeRecordBytes :: forall w. (IsWord w) => Int
scopeRecordBytes = scopeRecordWords * byteSizeT @w

scopeWordAddrs :: forall w. (IsWord w) => Int -> [Int]
scopeWordAddrs addr = [addr + idx * byteSizeT @w | idx <- [0 .. scopeRecordWords - 1]]

tagBits, addrBits, countBits :: Int
tagBits = 2
addrBits = 22
countBits = 8

instance BitPack ControlTag where
    unpack = getEnum tagBits <?> "record kind"
    pack tag = putEnum tagBits tag <?> "record kind"

instance BitPack Scope where
    unpack = getRecordHead >>= getRecordTail
    pack BlockScope{csEnd} = do
        word $ do
            pack BlockTag
            putPad countBits
            putField addrBits csEnd <?> "csEnd"
        word $ putPad 32
    pack LoopScope{csStart, csEnd} = do
        word $ do
            pack LoopTag
            putPad countBits
            putField addrBits csStart <?> "csStart"
        word $ do
            putPad 10
            putField addrBits csEnd <?> "csEnd"
    pack CallScope{csCallerFrameBase, csCallerLocalCount, csReturnPc, csResultCount} = do
        word $ do
            pack CallTag
            putField countBits csCallerLocalCount <?> "csCallerLocalCount"
            putField addrBits csCallerFrameBase <?> "csCallerFrameBase"
        word $ do
            putPad 2
            putField countBits csResultCount <?> "csResultCount"
            putField addrBits csReturnPc <?> "csReturnPc"

getRecordHead :: (IsWord w) => BitGet w (ControlTag, Int, Int)
getRecordHead = word $ do
    tag <- unpack
    count <- getField countBits
    addr <- getField addrBits
    return (tag, count, addr)

getRecordTail :: (IsWord w) => (ControlTag, Int, Int) -> BitGet w Scope
getRecordTail (tag, count, addr) = case tag of
    BlockTag -> do
        word $ getPad 32
        return BlockScope{csEnd = addr}
    LoopTag -> do
        csEnd <- word $ do
            getPad 10
            getField addrBits
        return LoopScope{csStart = addr, csEnd}
    CallTag -> do
        (csResultCount, csReturnPc) <- word $ do
            getPad 2
            resultCount <- getField countBits
            returnPc <- getField addrBits
            return (resultCount, returnPc)
        return CallScope{csCallerFrameBase = addr, csCallerLocalCount = count, csReturnPc, csResultCount}
