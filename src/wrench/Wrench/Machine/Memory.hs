{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Wrench.Machine.Memory (
    sliceMem,
    Mem (..),
    Cell (..),
    Memory (..),
    WordParts (..),
    word32ToHex,
    prepareDump,
    prettyDump,
) where

import Data.Bits (Bits (..), FiniteBits (..))
import Numeric (showHex)
import Relude
import Relude.Extra
import Relude.Unsafe qualified as Unsafe
import Wrench.Machine.Types
import Wrench.Translator.Types

prepareDump :: (ByteSize isa, MachineWord w) => Int -> [Section isa w w] -> Mem isa w
prepareDump memorySize sections =
    let addSection cells offset dump =
            let dump' = zip [offset ..] cells
             in (offset + length dump', dump' <> dump)
        processCode =
            concatMap
                ( \case
                    Mnemonic m ->
                        Instruction m : replicate (byteSize m - 1) InstructionPart
                    _other -> []
                )
        processData =
            concatMap
                ( \case
                    DataToken{dtValue} ->
                        map
                            Value
                            $ case dtValue of
                                DByte bs -> bs
                                DWord ws -> concatMap wordSplit ws
                )
        fromSections =
            snd
                $ foldl'
                    ( \(offset, dump) ->
                        ( \case
                            Code{org, codeTokens} -> addSection (processCode codeTokens) (fromMaybe offset org) dump
                            Data{org, dataTokens} -> addSection (processData dataTokens) (fromMaybe offset org) dump
                        )
                    )
                    (0, [])
                    sections
        dumpSize = maximum1 $ 0 :| keys fromSections
        placeholder = map (,Value 0) [0 .. memorySize - 1]
     in if dumpSize > memorySize
            then
                error $ "error: can not fit translation results in memory, need: " <> show dumpSize <> " available: " <> show memorySize
            else
                Mem
                    { memorySize
                    , memoryData = fromList (placeholder <> fromSections)
                    }

isValue Value{} = True
isValue _ = False

sliceMem :: [Int] -> IntMap (Cell isa w) -> [(Int, Cell isa w)]
sliceMem addrs memoryData = map (\a -> (a, Unsafe.fromJust (memoryData !? a))) addrs

prettyDump ::
    forall w isa.
    (ByteSize isa, MachineWord w, Show isa) =>
    HashMap String w
    -> IntMap (Cell isa w)
    -> String
prettyDump labels mem = intercalate "\n" $ pretty $ toPairs mem
    where
        offset2label :: HashMap Int String
        offset2label = fromList $ map (\(a, b) -> (fromEnum b, a)) $ toPairs labels
        instruction offset n i =
            let place = "mem[" <> show offset <> ".." <> show (offset + n - 1) <> "]"
                label = maybe "" (" \t@" <>) (offset2label !? offset)
             in place <> ": \t" <> show i <> label
        pretty [] = []
        pretty ((offset, Instruction i) : cs) =
            let n = byteSize i
                cs' = drop (n - 1) cs
             in instruction offset n i : pretty cs'
        pretty ((offset, InstructionPart) : cs) = (show offset <> ": \tInstructionPart") : pretty cs
        pretty cs =
            let values = map (second (\case (Value v) -> v; _ -> error "impossible")) $ takeWhile (isValue . snd) cs
                cs' = dropWhile (isValue . snd) cs
             in prettyData values : pretty cs'
        prettyData values = intercalate "\n" $ merge $ mark Nothing values
        mark _label [] = []
        mark label ((a, value) : values) =
            let label' = ((offset2label !? a) <|> label)
             in ((a, label'), value) : mark label' values
        merge [] = []
        merge values@(((a, label), _value) : _restValues) =
            let curValues = takeWhile ((== label) . snd . fst) values
                b = fst $ fst $ Unsafe.last curValues
                restValues = dropWhile ((== label) . snd . fst) values
             in ("mem[" <> show a <> ".." <> show b <> "]: \t" <> hexValues curValues <> maybe "" (("\t@" <>) . show) label)
                    : merge restValues
        hexValues values | all ((== 0) . snd) values && length values >= 16 = "( 00 )"
        hexValues values = toString $ unwords $ map (toText . word8ToHex . snd) values

word8ToHex w =
    let hex = showHex w ""
     in if length hex == 1 then "0" <> hex else hex

word32ToHex w =
    let hex = showHex (fromIntegral (fromIntegral w :: Int32) :: Word32) ""
     in "0x" <> replicate (8 - length hex) '0' <> hex

class Memory m isa w | m -> isa w where
    readInstruction :: m -> Int -> Either Text isa
    readWord :: m -> Int -> Either Text (m, w)
    readByte :: m -> Int -> Either Text (m, Word8)
    writeWord :: m -> Int -> w -> Either Text m
    writeByte :: m -> Int -> Word8 -> Either Text m
    dumpCells :: m -> IntMap (Cell isa w)

instance
    (ByteSize isa, MachineWord w) =>
    Memory (Mem isa w) isa w
    where
    readInstruction Mem{memoryData} idx =
        case memoryData !? idx of
            Just (Instruction i)
                | all
                    ( \addr -> case memoryData !? addr of
                        Just InstructionPart -> True
                        _ -> False
                    )
                    [idx + 1 .. idx + byteSize i - 1] ->
                    Right i
                | otherwise -> Left $ "memory[" <> show idx <> "]: instruction in memory corrupted"
            Just InstructionPart -> Left $ "memory[" <> show idx <> "]: instruction in memory corrupted"
            Just (Value _) -> Left $ "memory[" <> show idx <> "]: can't read instruction from data cell"
            Nothing -> Left $ "memory[" <> show idx <> "]: out of memory"

    readByte mem@Mem{memoryData} idx =
        case memoryData !? idx of
            Just (Value v) -> Right (mem, v)
            Just _ -> Left $ "memory[" <> show idx <> "]: can't read byte from instruction cell"
            Nothing -> Left $ "memory[" <> show idx <> "]: out of memory"

    readWord mem idx =
        let idxs = [idx .. idx + byteSizeT @w - 1]
            values = map (fmap snd . readByte mem) idxs
         in case lefts values of
                [] -> Right (mem, wordCombine $ rights values)
                errs -> Left $ unlines errs

    writeWord Mem{memorySize} idx _
        | idx < 0 || memorySize < idx + byteSizeT @w =
            Left $ "memory[" <> show idx <> "]: out of memory for word access"
    writeWord mem idx word =
        let updates = zip [idx ..] (wordSplit word)
         in foldlM (\m (i, x) -> writeByte m i x) mem updates

    writeByte Mem{memorySize} idx _
        | idx < 0 || memorySize <= idx = Left $ "memory[" <> show idx <> "]: out of memory"
    writeByte mem@Mem{memoryData} idx byte =
        let memoryData' = insert idx (Value byte) memoryData
         in Right $ mem{memoryData = memoryData'}

    dumpCells Mem{memoryData} = memoryData

ioPortInstructionCollision ::
    forall w isa. (ByteSize isa, ByteSizeT w) => IoMem isa w -> Int -> isa -> Bool
ioPortInstructionCollision io addr instr =
    let !n = byteSize instr
        wn = byteSizeT @w
        !result =
            any (\idx -> (idx - n + 1 <= addr && addr <= idx - 1) || (idx + 1 <= addr && addr <= idx + wn - 1)) (ioWordKeys @w io)
     in result

ioPortWordCollision :: forall w isa. (ByteSizeT w) => IoMem isa w -> Int -> Bool
ioPortWordCollision io addr =
    let n = byteSizeT @w
     in any (\idx -> (idx - n + 1 <= addr && addr <= idx - 1) || (idx + 1 <= addr && addr <= idx + n - 1)) (ioWordKeys @w io)

ioPortByteCollision :: forall w isa. (ByteSizeT w) => IoMem isa w -> Int -> Bool
ioPortByteCollision io addr =
    let n = byteSizeT @w
        mkParts idx = [idx + 1 .. idx + n - 1]
        parts = concatMap mkParts (ioWordKeys @w io)
     in (addr `elem` parts)

ioWordKeys :: forall w isa. IoMem isa w -> [Int]
ioWordKeys IoMem{mIoKeys, mSpiDevices} =
    mIoKeys <> concatMap (\d -> [pinOutAddr d, pinInAddr d]) (elems mSpiDevices)

data SpiRegister = SpiPinsOut | SpiPinsIn

pinOutAddr :: SpiDevice w -> Int
pinOutAddr SpiDevice{spiPins = SpiPinsConf{spPinsOutAddr}} = spPinsOutAddr

pinInAddr :: SpiDevice w -> Int
pinInAddr SpiDevice{spiPins = SpiPinsConf{spPinsInAddr}} = spPinsInAddr

findSpiRegister :: forall w isa. Bool -> IoMem isa w -> Int -> Maybe (Int, SpiRegister)
findSpiRegister preferIn IoMem{mSpiKeys, mSpiDevices} addr =
    asum $ map match mSpiKeys
    where
        match base
            | Just device <- mSpiDevices !? base =
                let outMatch = addr == pinOutAddr device
                    inMatch = addr == pinInAddr device
                 in case (outMatch, inMatch, preferIn) of
                        (True, True, True) -> Just (base, SpiPinsIn)
                        (True, True, False) -> Just (base, SpiPinsOut)
                        (True, False, _) -> Just (base, SpiPinsOut)
                        (False, True, _) -> Just (base, SpiPinsIn)
                        _ -> Nothing
            | otherwise = Nothing

findSpiByteRegister :: forall w isa. (ByteSizeT w) => Bool -> IoMem isa w -> Int -> Maybe (SpiRegister, Int, Int)
findSpiByteRegister preferIn IoMem{mSpiKeys, mSpiDevices} addr =
    asum $ map match mSpiKeys
    where
        wordSize = byteSizeT @w
        match base
            | Just device <- mSpiDevices !? base
            , let outAddr = pinOutAddr device
            , outAddr <= addr
            , addr < outAddr + wordSize =
                if preferIn && pinInAddr device == outAddr
                    then Nothing
                    else Just (SpiPinsOut, addr - outAddr, outAddr)
            | Just device <- mSpiDevices !? base
            , let inAddr = pinInAddr device
            , inAddr <= addr
            , addr < inAddr + wordSize =
                Just (SpiPinsIn, addr - inAddr, inAddr)
            | otherwise =
                Nothing

readSpiWord :: forall w isa. (MachineWord w) => IoMem isa w -> Int -> Maybe (Either Text (IoMem isa w, w))
readSpiWord io@IoMem{mSpiDevices} addr =
    findSpiRegister @w True io addr <&> \(base, register) ->
        case mSpiDevices !? base of
            Nothing -> Left $ "iomemory[" <> show addr <> "]: unknown SPI device"
            Just device ->
                case register of
                    SpiPinsOut -> Right (io, spiPinsOutWord device)
                    SpiPinsIn -> Right (io, spiPinsInWord device)

writeSpiWord :: forall w isa. (MachineWord w) => IoMem isa w -> Int -> w -> Maybe (Either Text (IoMem isa w))
writeSpiWord io@IoMem{mSpiDevices, mClock} addr word =
    findSpiRegister @w False io addr <&> \(base, register) ->
        case mSpiDevices !? base of
            Nothing -> Left $ "iomemory[" <> show addr <> "]: unknown SPI device"
            Just device ->
                case register of
                    SpiPinsIn ->
                        Left $ "iomemory[" <> show addr <> "]: can't write to SPI pins-in register"
                    SpiPinsOut ->
                        let device' = writeSpiPins mClock device word
                         in Right io{mSpiDevices = insert base device' mSpiDevices}

readSpiByte :: forall w isa. (MachineWord w) => IoMem isa w -> Int -> Maybe (Either Text (IoMem isa w, Word8))
readSpiByte io addr =
    findSpiByteRegister @w True io addr <&> \(_register, offset, wordAddr) -> do
        case readSpiWord io wordAddr of
            Just result -> do
                (io', word) <- result
                return (io', wordSplit word Unsafe.!! offset)
            Nothing -> Left $ "iomemory[" <> show addr <> "]: unknown SPI device"

writeSpiByte :: forall w isa. (MachineWord w) => IoMem isa w -> Int -> Word8 -> Maybe (Either Text (IoMem isa w))
writeSpiByte io addr byte =
    findSpiByteRegister @w False io addr <&> \(register, offset, wordAddr) ->
        case (register, offset) of
            (SpiPinsOut, 0) -> fromMaybe (Left $ "iomemory[" <> show addr <> "]: unknown SPI device") $ writeSpiWord io wordAddr (byteToWord byte)
            (SpiPinsIn, 0) -> Left $ "iomemory[" <> show addr <> "]: can't write to SPI pins-in register"
            _ -> Left $ "iomemory[" <> show addr <> "]: can't write byte to SPI register part"

popAvailableMiso :: Int -> [(w, Int)] -> Maybe ((w, Int), [(w, Int)])
popAvailableMiso clock = go []
    where
        go _ [] = Nothing
        go earlier (entry@(_, arrivalTick) : rest)
            | arrivalTick <= clock = Just (entry, reverse earlier <> rest)
            | otherwise = go (entry : earlier) rest

spiPinsOutWord :: (Bits w, Num w) => SpiDevice w -> w
spiPinsOutWord SpiDevice{spiPins = SpiPinsConf{spCsBit, spClkBit, spMosiBit}, spiCsPin, spiClkPin, spiMosiPin} =
    bitWord spCsBit spiCsPin .|. bitWord spClkBit spiClkPin .|. bitWord spMosiBit spiMosiPin

spiPinsInWord :: (Bits w, Num w) => SpiDevice w -> w
spiPinsInWord SpiDevice{spiPins = SpiPinsConf{spMisoBit}, spiMisoPin} = bitWord spMisoBit spiMisoPin

bitWord :: (Bits w, Num w) => Int -> Bool -> w
bitWord idx enabled = if enabled then bit idx else 0

writeSpiPins :: forall w. (MachineWord w) => Int -> SpiDevice w -> w -> SpiDevice w
writeSpiPins _clock device word =
    let oldCs = spiCsPin device
        oldClk = spiClkPin device
        oldSoftClock = spiSoftClock device
        SpiPinsConf{spCsBit, spClkBit, spMosiBit} = spiPins device
        mode = spiClockMode device
        newCs = testBit word spCsBit
        newClk = testBit word spClkBit
        newMosi = testBit word spMosiBit
        baseDevice =
            device
                { spiCsPin = newCs
                , spiClkPin = newClk
                , spiMosiPin = newMosi
                }
        afterCs =
            if oldCs && not newCs
                then
                    if spiModePrimeOnActivate mode
                        then primeMiso oldSoftClock baseDevice
                        else baseDevice{spiMisoPin = False}
                else
                    if newCs
                        then baseDevice{spiMisoPin = False, spiMosiShift = 0, spiMosiBits = 0}
                        else baseDevice
        active = not newCs
        onRising = (not oldClk) && newClk
        onFalling = oldClk && (not newClk)
        sampleOnRising = spiModeSampleOnRising mode
        sampleEdge = active && ((sampleOnRising && onRising) || ((not sampleOnRising) && onFalling))
        shiftEdge = active && ((sampleOnRising && onFalling) || ((not sampleOnRising) && onRising))
        afterShift =
            if shiftEdge
                then shiftMiso oldSoftClock afterCs
                else afterCs
     in if sampleEdge
            then
                let softClock' = oldSoftClock + 1
                    sampled = shiftMosi softClock' newMosi afterShift
                 in sampled{spiSoftClock = softClock'}
            else afterShift

spiModeSampleOnRising :: SpiClockMode -> Bool
spiModeSampleOnRising SpiMode0 = True
spiModeSampleOnRising SpiMode1 = False
spiModeSampleOnRising SpiMode2 = False
spiModeSampleOnRising SpiMode3 = True

spiModePrimeOnActivate :: SpiClockMode -> Bool
spiModePrimeOnActivate SpiMode0 = True
spiModePrimeOnActivate SpiMode1 = False
spiModePrimeOnActivate SpiMode2 = True
spiModePrimeOnActivate SpiMode3 = False

primeMiso :: forall w. (MachineWord w) => Int -> SpiDevice w -> SpiDevice w
primeMiso clock device =
    let device' = ensureMisoLoaded clock device
     in device'{spiMisoPin = currentMisoBit device'}

ensureMisoLoaded :: forall w. (MachineWord w) => Int -> SpiDevice w -> SpiDevice w
ensureMisoLoaded _clock device@SpiDevice{spiMisoShift}
    | isJust spiMisoShift = device
ensureMisoLoaded clock device@SpiDevice{spiMisoPending} =
    case popAvailableMiso clock spiMisoPending of
        Nothing -> device
        Just ((w, tick), rest) ->
            device
                { spiMisoPending = rest
                , spiMisoShift = Just SpiMisoShift{smsWord = w, smsBitIndex = finiteBitSize w - 1, smsTick = tick}
                }

currentMisoBit :: (Bits w) => SpiDevice w -> Bool
currentMisoBit SpiDevice{spiMisoShift} =
    case spiMisoShift of
        Just SpiMisoShift{smsWord, smsBitIndex} -> testBit smsWord smsBitIndex
        Nothing -> False

shiftMiso :: forall w. (MachineWord w) => Int -> SpiDevice w -> SpiDevice w
shiftMiso clock device =
    let hadLoadedWord = isJust (spiMisoShift device)
        device0 = ensureMisoLoaded clock device
     in case spiMisoShift device0 of
            Nothing -> device0{spiMisoPin = False}
            _
                | not hadLoadedWord ->
                    device0{spiMisoPin = currentMisoBit device0}
            Just SpiMisoShift{smsWord, smsBitIndex, smsTick} ->
                if smsBitIndex == 0
                    then
                        let finished = (smsWord, smsTick)
                            device1 =
                                device0
                                    { spiMisoShift = Nothing
                                    , spiMisoConsumed = spiMisoConsumed device0 <> [finished]
                                    }
                            device2 = ensureMisoLoaded clock device1
                         in device2{spiMisoPin = currentMisoBit device2}
                    else
                        let shift' = SpiMisoShift{smsWord, smsBitIndex = smsBitIndex - 1, smsTick}
                            device1 = device0{spiMisoShift = Just shift'}
                         in device1{spiMisoPin = currentMisoBit device1}

shiftMosi :: forall w. (MachineWord w) => Int -> Bool -> SpiDevice w -> SpiDevice w
shiftMosi clock mosiBit device@SpiDevice{spiMosiShift, spiMosiBits, spiMosiLog} =
    let newBit :: w = if mosiBit then 1 else 0
        nextWord = (spiMosiShift `shiftL` 1) .|. newBit
        nextBits = spiMosiBits + 1
        wordBits = finiteBitSize spiMosiShift
     in if nextBits >= wordBits
            then
                device
                    { spiMosiShift = 0
                    , spiMosiBits = 0
                    , spiMosiLog = spiMosiLog <> [(nextWord, clock)]
                    }
            else device{spiMosiShift = nextWord, spiMosiBits = nextBits}

instance (ByteSize isa, MachineWord w, Memory (Mem isa w) isa w) => Memory (IoMem isa w) isa w where
    readInstruction io@IoMem{mIoStreams, mIoCells} idx =
        case mIoStreams !? idx of
            Just _ -> Left $ "iomemory[" <> show idx <> "]: instruction in memory corrupted"
            Nothing -> case readInstruction mIoCells idx of
                Left err -> Left err
                Right instr
                    | ioPortInstructionCollision io idx instr ->
                        Left $ "iomemory[" <> show idx <> "]: instruction in memory corrupted"
                    | otherwise -> Right instr

    readByte io@IoMem{mIoByteToWord} idx
        | Just wordIdx <- mIoByteToWord !? idx = do
            (io', word) <- readWord io wordIdx
            return (io', wordSplit word Unsafe.!! (idx - wordIdx))
    readByte io idx
        | Just result <- readSpiByte io idx = result
    readByte io@IoMem{mIoCells} idx = do
        (mIoCells', v) <- readByte mIoCells idx
        return (io{mIoCells = mIoCells'}, v)

    readWord io idx | ioPortWordCollision io idx = Left $ "iomemory[" <> show idx <> "]: can't read word from input port"
    readWord io idx
        | Just result <- readSpiWord io idx = result
    readWord io@IoMem{mIoStreams, mIoCells} idx = do
        case mIoStreams !? idx of
            Just ([], _) -> Left $ "iomemory[" <> show idx <> "]: input is depleted"
            Just (i : is, os) -> do
                let io' = io{mIoStreams = insert idx (is, os) mIoStreams}
                Right (io', i)
            Nothing -> do
                (mIoCells', w) <- readWord mIoCells idx
                return (io{mIoCells = mIoCells'}, w)

    writeWord io idx _word | ioPortWordCollision io idx = Left $ "iomemory[" <> show idx <> "]: can't write word to input port"
    writeWord io idx word
        | Just result <- writeSpiWord io idx word = result
    writeWord io idx word =
        case mIoStreams io !? idx of
            Just (is, os) -> Right io{mIoStreams = insert idx (is, word : os) (mIoStreams io)}
            Nothing -> do
                mIoCells' <- writeWord (mIoCells io) idx word
                return io{mIoCells = mIoCells'}

    writeByte io idx _byte
        | ioPortByteCollision io idx =
            Left $ "iomemory[" <> show idx <> "]: can't write byte to input port"
    writeByte io idx byte
        | Just result <- writeSpiByte io idx byte = result
    writeByte io idx byte =
        case mIoStreams io !? idx of
            Just (is, os) -> Right io{mIoStreams = insert idx (is, byteToWord byte : os) (mIoStreams io)}
            Nothing -> do
                mIoCells' <- writeByte (mIoCells io) idx byte
                return io{mIoCells = mIoCells'}

    dumpCells = memoryData . mIoCells
