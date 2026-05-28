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

ioWordKeys :: forall w isa. (ByteSizeT w) => IoMem isa w -> [Int]
ioWordKeys IoMem{mIoKeys, mSpiKeys} =
    mIoKeys <> concatMap (\idx -> [idx, idx + byteSizeT @w]) mSpiKeys

data SpiRegister = SpiData | SpiStatus | SpiPinsOut | SpiPinsIn

findSpiRegister :: forall w isa. (ByteSizeT w) => IoMem isa w -> Int -> Maybe (Int, SpiRegister)
findSpiRegister IoMem{mSpiKeys, mSpiDevices} addr =
    asum $ map match mSpiKeys
    where
        wordSize = byteSizeT @w
        match base
            | addr == base = (\d -> (base,) <$> firstRegister d) =<< (mSpiDevices !? base)
            | addr == base + wordSize = (\d -> (base,) <$> secondRegister d) =<< (mSpiDevices !? base)
            | otherwise = Nothing
        firstRegister SpiDevice{spiMode} = case spiMode of
            SpiHardware -> Just SpiData
            SpiSoftware -> Just SpiPinsOut
        secondRegister SpiDevice{spiMode} = case spiMode of
            SpiHardware -> Just SpiStatus
            SpiSoftware -> Just SpiPinsIn

findSpiByteRegister :: forall w isa. (ByteSizeT w) => IoMem isa w -> Int -> Maybe (SpiRegister, Int, Int)
findSpiByteRegister io@IoMem{mSpiKeys} addr =
    asum $ map match mSpiKeys
    where
        wordSize = byteSizeT @w
        match base
            | base <= addr && addr < base + wordSize =
                (\(_, r) -> (r, addr - base, base)) <$> findSpiRegister @w io base
            | base + wordSize <= addr && addr < base + 2 * wordSize =
                (\(_, r) -> (r, addr - base - wordSize, base + wordSize)) <$> findSpiRegister @w io (base + wordSize)
            | otherwise = Nothing

readSpiWord :: forall w isa. (MachineWord w) => IoMem isa w -> Int -> Maybe (Either Text (IoMem isa w, w))
readSpiWord io@IoMem{mSpiDevices, mClock} addr =
    findSpiRegister @w io addr <&> \(base, register) ->
        case mSpiDevices !? base of
            Nothing -> Left $ "iomemory[" <> show addr <> "]: unknown SPI device"
            Just device@SpiDevice{spiMisoPending, spiMisoConsumed} ->
                case register of
                    SpiStatus
                        | not (spiHardwareEnabled device) ->
                            Left $ "iomemory[" <> show addr <> "]: SPI status register is disabled"
                        | otherwise ->
                            Right (io, spiStatusWord (spiHardwareClock mClock device) device)
                    SpiData
                        | not (spiHardwareEnabled device) ->
                            Left $ "iomemory[" <> show addr <> "]: SPI data register is disabled"
                        | otherwise ->
                            case popAvailableMiso (spiHardwareClock mClock device) spiMisoPending of
                                Nothing -> Left $ "iomemory[" <> show addr <> "]: SPI input is not ready"
                                Just (misoValue, spiMisoPending') ->
                                    let device' =
                                            device
                                                { spiMisoPending = spiMisoPending'
                                                , spiMisoConsumed = spiMisoConsumed <> [misoValue]
                                                }
                                        io' = io{mSpiDevices = insert base device' mSpiDevices}
                                     in Right (io', fst misoValue)
                    SpiPinsOut
                        | not (spiSoftwareEnabled device) ->
                            Left $ "iomemory[" <> show addr <> "]: SPI pins register is disabled"
                        | otherwise ->
                            Right (io, spiPinsOutWord device)
                    SpiPinsIn
                        | not (spiSoftwareEnabled device) ->
                            Left $ "iomemory[" <> show addr <> "]: SPI pins register is disabled"
                        | otherwise ->
                            Right (io, spiPinsInWord device)

writeSpiWord :: forall w isa. (MachineWord w) => IoMem isa w -> Int -> w -> Maybe (Either Text (IoMem isa w))
writeSpiWord io@IoMem{mSpiDevices, mClock} addr word =
    findSpiRegister @w io addr <&> \(base, register) ->
        case mSpiDevices !? base of
            Nothing -> Left $ "iomemory[" <> show addr <> "]: unknown SPI device"
            Just device@SpiDevice{spiMosiLog} ->
                case register of
                    SpiStatus ->
                        Left $ "iomemory[" <> show addr <> "]: can't write to SPI status register"
                    SpiPinsIn ->
                        Left $ "iomemory[" <> show addr <> "]: can't write to SPI pins-in register"
                    SpiData
                        | not (spiHardwareEnabled device) ->
                            Left $ "iomemory[" <> show addr <> "]: SPI data register is disabled"
                        | otherwise ->
                            let clock = spiHardwareClock mClock device
                                device' = device{spiMosiLog = spiMosiLog <> [(word, clock)]}
                             in Right io{mSpiDevices = insert base device' mSpiDevices}
                    SpiPinsOut
                        | not (spiSoftwareEnabled device) ->
                            Left $ "iomemory[" <> show addr <> "]: SPI pins register is disabled"
                        | otherwise ->
                            let device' = writeSpiPins mClock device word
                             in Right io{mSpiDevices = insert base device' mSpiDevices}

readSpiByte :: forall w isa. (MachineWord w) => IoMem isa w -> Int -> Maybe (Either Text (IoMem isa w, Word8))
readSpiByte io addr =
    findSpiByteRegister @w io addr <&> \(_register, offset, wordAddr) -> do
        case readSpiWord io wordAddr of
            Just result -> do
                (io', word) <- result
                return (io', wordSplit word Unsafe.!! offset)
            Nothing -> Left $ "iomemory[" <> show addr <> "]: unknown SPI device"

writeSpiByte :: forall w isa. (MachineWord w) => IoMem isa w -> Int -> Word8 -> Maybe (Either Text (IoMem isa w))
writeSpiByte io addr byte =
    findSpiByteRegister @w io addr <&> \(register, offset, wordAddr) ->
        case (register, offset) of
            (SpiData, 0) -> fromMaybe (Left $ "iomemory[" <> show addr <> "]: unknown SPI device") $ writeSpiWord io wordAddr (byteToWord byte)
            (SpiStatus, 0) -> Left $ "iomemory[" <> show addr <> "]: can't write to SPI status register"
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

spiStatusWord :: forall w. (Num w) => Int -> SpiDevice w -> w
spiStatusWord clock device =
    if misoReadyAt clock device then 1 else 0

misoReadyAt :: Int -> SpiDevice w -> Bool
misoReadyAt clock SpiDevice{spiMisoPending, spiMisoShift} =
    any ((<= clock) . snd) spiMisoPending || isJust spiMisoShift

spiHardwareEnabled :: SpiDevice w -> Bool
spiHardwareEnabled SpiDevice{spiMode} = spiMode == SpiHardware

spiSoftwareEnabled :: SpiDevice w -> Bool
spiSoftwareEnabled SpiDevice{spiMode} = spiMode == SpiSoftware

spiHardwareClock :: Int -> SpiDevice w -> Int
spiHardwareClock cpuClock SpiDevice{spiClkDiv} = cpuClock `div` spiClkDiv

spiPinsOutWord :: (Bits w, Num w) => SpiDevice w -> w
spiPinsOutWord SpiDevice{spiCsPin, spiClkPin, spiMosiPin} =
    bitWord 0 spiCsPin .|. bitWord 1 spiClkPin .|. bitWord 2 spiMosiPin

spiPinsInWord :: (Bits w, Num w) => SpiDevice w -> w
spiPinsInWord SpiDevice{spiMisoPin} = bitWord 0 spiMisoPin

bitWord :: (Bits w, Num w) => Int -> Bool -> w
bitWord idx enabled = if enabled then bit idx else 0

writeSpiPins :: forall w. (MachineWord w) => Int -> SpiDevice w -> w -> SpiDevice w
writeSpiPins _clock device word =
    let oldCs = spiCsPin device
        oldClk = spiClkPin device
        oldSoftClock = spiSoftClock device
        newCs = testBit word 0
        newClk = testBit word 1
        newMosi = testBit word 2
        baseDevice =
            device
                { spiCsPin = newCs
                , spiClkPin = newClk
                , spiMosiPin = newMosi
                }
        afterCs =
            if oldCs && not newCs
                then primeMiso oldSoftClock baseDevice
                else
                    if newCs
                        then baseDevice{spiMisoPin = False, spiMosiShift = 0, spiMosiBits = 0}
                        else baseDevice
        onRising = (not newCs) && (not oldClk) && newClk
     in if onRising
            then
                let softClock' = oldSoftClock + 1
                    shifted = shiftMiso softClock' (shiftMosi softClock' newMosi afterCs)
                 in shifted{spiSoftClock = softClock'}
            else afterCs

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
    let device0 = ensureMisoLoaded clock device
     in case spiMisoShift device0 of
            Nothing -> device0{spiMisoPin = False}
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
