{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Wrench.Machine.Spi (
    readSpiByte,
    readSpiWord,
    spiDeviceClock,
    spiOutputAddrs,
    spiStatusText,
    spiWaveText,
    spiWordKeys,
    writeSpiByte,
    writeSpiWord,
) where

import Data.Bifunctor qualified as Bi
import Data.Bits (Bits (..))
import Data.Text qualified as T
import Relude
import Relude.Extra
import Relude.Unsafe qualified as Unsafe
import Wrench.Machine.Types

spiWordKeys :: IntMap (SpiDevice w) -> [Int]
spiWordKeys devices = ordNub $ concatMap spiDeviceAddrs (elems devices)

spiDeviceAddrs :: SpiDevice w -> [Int]
spiDeviceAddrs SpiDevice{spiPins = SpiPinsConf{spCsAddr, spClkAddr, spMosiAddr, spMisoAddr}} =
    [spCsAddr, spClkAddr, spMosiAddr, spMisoAddr]

spiOutputAddrs :: IntMap (SpiDevice w) -> [Int]
spiOutputAddrs devices = ordNub $ concatMap spiDeviceOutputAddrs (elems devices)

spiDeviceOutputAddrs :: SpiDevice w -> [Int]
spiDeviceOutputAddrs SpiDevice{spiPins = SpiPinsConf{spCsAddr, spClkAddr, spMosiAddr}} =
    [spCsAddr, spClkAddr, spMosiAddr]

findSpiByteRegister :: forall w isa. (ByteSizeT w) => IoMem isa w -> Int -> Maybe (Int, Int)
findSpiByteRegister IoMem{mSpiDevices} addr =
    asum $ map match (spiWordKeys mSpiDevices)
    where
        wordSize = byteSizeT @w
        match wordAddr
            | wordAddr <= addr && addr < wordAddr + wordSize =
                Just (addr - wordAddr, wordAddr)
            | otherwise =
                Nothing

readSpiWord :: forall w isa. (MachineWord w) => IoMem isa w -> Int -> Maybe (Either Text (IoMem isa w, w))
readSpiWord io@IoMem{mSpiDevices} addr =
    if addr `elem` spiWordKeys mSpiDevices
        then Just $ Right (io, spiPinsWord addr mSpiDevices)
        else Nothing

writeSpiWord :: forall w isa. (MachineWord w) => IoMem isa w -> Int -> w -> Maybe (Either Text (IoMem isa w))
writeSpiWord io@IoMem{mSpiDevices, mClock} addr word
    | addr `notElem` spiWordKeys mSpiDevices = Nothing
    | addr `notElem` spiOutputAddrs mSpiDevices =
        Just $ Left $ "iomemory[" <> show addr <> "]: can't write to SPI input-only pin address"
    | otherwise =
        let devices' = fmap (writeSpiPins mClock addr word) mSpiDevices
         in Just $ Right io{mSpiDevices = devices'}

readSpiByte :: forall w isa. (MachineWord w) => IoMem isa w -> Int -> Maybe (Either Text (IoMem isa w, Word8))
readSpiByte io addr =
    findSpiByteRegister @w io addr <&> \(offset, wordAddr) -> do
        case readSpiWord io wordAddr of
            Just result -> do
                (io', word) <- result
                return (io', wordSplit word Unsafe.!! offset)
            Nothing -> Left $ "iomemory[" <> show addr <> "]: unknown SPI device"

writeSpiByte :: forall w isa. (MachineWord w) => IoMem isa w -> Int -> Word8 -> Maybe (Either Text (IoMem isa w))
writeSpiByte io addr byte =
    findSpiByteRegister @w io addr <&> \(offset, wordAddr) ->
        case offset of
            0 -> fromMaybe (Left $ "iomemory[" <> show addr <> "]: unknown SPI device") $ writeSpiWord io wordAddr (byteToWord byte)
            _ -> Left $ "iomemory[" <> show addr <> "]: can't write byte to SPI register part"

popAvailableMiso :: Int -> [(w, Int, Int)] -> Maybe ((w, Int, Int), [(w, Int, Int)])
popAvailableMiso clock = go []
    where
        go _ [] = Nothing
        go earlier (entry@(_, arrivalTick, _) : rest)
            | arrivalTick <= clock = Just (entry, reverse earlier <> rest)
            | otherwise = go (entry : earlier) rest

spiPinsWord :: (Bits w, Num w) => Int -> IntMap (SpiDevice w) -> w
spiPinsWord addr devices =
    foldl' (.|.) 0 $ map (spiDevicePinsWord addr) (elems devices)

spiDevicePinsWord :: (Bits w, Num w) => Int -> SpiDevice w -> w
spiDevicePinsWord
    addr
    SpiDevice
        { spiPins = SpiPinsConf{spCsAddr, spCsBit, spClkAddr, spClkBit, spMosiAddr, spMosiBit, spMisoAddr, spMisoBit}
        , spiCsPin
        , spiClkPin
        , spiMosiPin
        , spiMisoPin
        } =
        pinBitWord spCsAddr spCsBit spiCsPin
            .|. pinBitWord spClkAddr spClkBit spiClkPin
            .|. pinBitWord spMosiAddr spMosiBit spiMosiPin
            .|. pinBitWord spMisoAddr spMisoBit spiMisoPin
        where
            pinBitWord pinAddr pinBit value =
                if pinAddr == addr then bitWord pinBit value else 0

bitWord :: (Bits w, Num w) => Int -> Bool -> w
bitWord idx enabled = if enabled then bit idx else 0

writeSpiPins :: forall w. (MachineWord w) => Int -> Int -> w -> SpiDevice w -> SpiDevice w
writeSpiPins _clock addr word device =
    let oldCs = spiCsPin device
        oldClk = spiClkPin device
        oldSoftClock = spiSoftClock device
        SpiPinsConf{spCsAddr, spCsBit, spClkAddr, spClkBit, spMosiAddr, spMosiBit} = spiPins device
        mode = spiClockMode device
        newCs = updatePin spCsAddr spCsBit oldCs
        newClk = updatePin spClkAddr spClkBit oldClk
        newMosi = updatePin spMosiAddr spMosiBit (spiMosiPin device)
        updatePin pinAddr pinBit oldValue =
            if pinAddr == addr then testBit word pinBit else oldValue
        deviceWithNewPins =
            device
                { spiCsPin = newCs
                , spiClkPin = newClk
                , spiMosiPin = newMosi
                }
        deviceAfterCs
            | oldCs && not newCs =
                if spiModePrimeOnActivate mode
                    then
                        primeMiso oldSoftClock deviceWithNewPins
                    else
                        deviceWithNewPins{spiMisoPin = False}
            | newCs =
                deviceWithNewPins
                    { spiMisoPin = False
                    , spiMosiShift = 0
                    , spiMosiBits = 0
                    }
            | otherwise = deviceWithNewPins
        active = not newCs
        onRising = not oldClk && newClk
        onFalling = oldClk && not newClk
        sampleOnRising = spiModeSampleOnRising mode
        sampleEdge = active && ((sampleOnRising && onRising) || (not sampleOnRising && onFalling))
        shiftEdge = active && ((sampleOnRising && onFalling) || (not sampleOnRising && onRising))
        afterShift =
            if shiftEdge
                then shiftMiso oldSoftClock deviceAfterCs
                else deviceAfterCs
     in if sampleEdge
            then
                let softClock' = oldSoftClock + 1
                    sampled = shiftMosi softClock' newMosi afterShift
                 in rememberSpiPins sampled{spiSoftClock = softClock'}
            else rememberSpiPins afterShift

rememberSpiPins :: SpiDevice w -> SpiDevice w
rememberSpiPins device@SpiDevice{spiWaveLog} =
    let snapshot = spiPinsSnapshot device
     in case reverse spiWaveLog of
            old : _ | samePins old snapshot -> device
            _ -> device{spiWaveLog = spiWaveLog <> [snapshot]}

spiPinsSnapshot :: SpiDevice w -> SpiPinsSnapshot
spiPinsSnapshot SpiDevice{spiSoftClock, spiCsPin, spiClkPin, spiMosiPin, spiMisoPin} =
    SpiPinsSnapshot
        { spsTick = spiSoftClock
        , spsCsPin = spiCsPin
        , spsClkPin = spiClkPin
        , spsMosiPin = spiMosiPin
        , spsMisoPin = spiMisoPin
        }

samePins :: SpiPinsSnapshot -> SpiPinsSnapshot -> Bool
samePins a b =
    spsCsPin a
        == spsCsPin b
        && spsClkPin a
        == spsClkPin b
        && spsMosiPin a
        == spsMosiPin b
        && spsMisoPin a
        == spsMisoPin b

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
    let (device', _) = loadScheduledMiso clock device
     in device'{spiMisoPin = currentMisoBit device'}

loadScheduledMiso :: Int -> SpiDevice w -> (SpiDevice w, Bool)
loadScheduledMiso clock device@SpiDevice{spiMisoPending} =
    case popAvailableMiso clock spiMisoPending of
        Nothing -> (device, False)
        Just ((w, tick, bits), rest) ->
            ( device
                { spiMisoPending = rest
                , spiMisoShift = Just SpiMisoShift{smsWord = w, smsBitIndex = bits - 1, smsTick = tick}
                , spiMosiFrameBits = bits
                }
            , True
            )

currentMisoBit :: (Bits w) => SpiDevice w -> Bool
currentMisoBit SpiDevice{spiMisoShift} =
    case spiMisoShift of
        Just SpiMisoShift{smsWord, smsBitIndex} -> testBit smsWord smsBitIndex
        Nothing -> False

shiftMiso :: forall w. (MachineWord w) => Int -> SpiDevice w -> SpiDevice w
shiftMiso clock device =
    case spiMisoShift device of
        Nothing ->
            let (device0, loadedNow) = loadScheduledMiso clock device
             in if loadedNow
                    then device0{spiMisoPin = currentMisoBit device0}
                    else device0{spiMisoPin = False}
        Just SpiMisoShift{smsWord, smsBitIndex, smsTick} ->
            if smsBitIndex == 0
                then
                    let finished = (smsWord, smsTick)
                        device1 =
                            device
                                { spiMisoShift = Nothing
                                , spiMisoConsumed = spiMisoConsumed device <> [finished]
                                }
                        (device2, _) = loadScheduledMiso clock device1
                     in device2{spiMisoPin = currentMisoBit device2}
                else
                    let shift' = SpiMisoShift{smsWord, smsBitIndex = smsBitIndex - 1, smsTick}
                        device1 = device{spiMisoShift = Just shift'}
                     in device1{spiMisoPin = currentMisoBit device1}

shiftMosi :: forall w. (MachineWord w) => Int -> Bool -> SpiDevice w -> SpiDevice w
shiftMosi clock mosiBit device@SpiDevice{spiMosiShift, spiMosiBits, spiMosiFrameBits, spiMosiLog} =
    let newBit :: w = if mosiBit then 1 else 0
        nextWord = (spiMosiShift `shiftL` 1) .|. newBit
        nextBits = spiMosiBits + 1
     in if nextBits >= spiMosiFrameBits
            then
                device
                    { spiMosiShift = 0
                    , spiMosiBits = 0
                    , spiMosiLog = spiMosiLog <> [(nextWord, clock)]
                    }
            else device{spiMosiShift = nextWord, spiMosiBits = nextBits}

spiWaveText :: [SpiPinsSnapshot] -> Text
spiWaveText [] = ""
spiWaveText xs =
    T.intercalate "\n\n" $ map renderWaveBlock $ waveBlocks spiWaveBlockWidth waveLines
    where
        waveLines =
            [ ("TICK: ", tickLine xs)
            , ("CS  : ", waveLine spsCsPin xs)
            , ("CLK : ", waveLine spsClkPin xs)
            , ("MOSI: ", waveLine spsMosiPin xs)
            , ("MISO: ", waveLine spsMisoPin xs)
            ]

spiWaveBlockWidth :: Int
spiWaveBlockWidth = spiWaveBlockTicks * spiWaveTickWidth

spiWaveBlockTicks :: Int
spiWaveBlockTicks = 25

spiWaveTickWidth :: Int
spiWaveTickWidth = 4

spiWaveTickLabelStep :: Int
spiWaveTickLabelStep = 5

waveBlocks :: Int -> [(Text, Text)] -> [[(Text, Text)]]
waveBlocks width lines'
    | all (T.null . snd) lines' = []
    | otherwise =
        let block = map (Bi.second (T.take width)) lines'
            rest = map (Bi.second (T.drop width)) lines'
         in block : waveBlocks width rest

renderWaveBlock :: [(Text, Text)] -> Text
renderWaveBlock block =
    T.intercalate
        "\n"
        [name <> line | (name, line) <- block]

tickLine :: [SpiPinsSnapshot] -> Text
tickLine xs =
    T.concat [tickCell tick | tick <- waveTicks xs]

tickCell :: Int -> Text
tickCell tick
    | tick `mod` spiWaveTickLabelStep == 0 = T.take spiWaveTickWidth $ show tick <> T.replicate spiWaveTickWidth " "
    | otherwise = T.replicate spiWaveTickWidth " "

waveTicks :: [SpiPinsSnapshot] -> [Int]
waveTicks [] = []
waveTicks (firstSnapshot : rest) =
    [spsTick firstSnapshot .. spsTick (lastSnapshot firstSnapshot rest)]

waveLine :: (SpiPinsSnapshot -> Bool) -> [SpiPinsSnapshot] -> Text
waveLine _ [] = ""
waveLine pin (firstSnapshot : rest) =
    T.concat $ waveLineCells pin firstSnapshot (snapshotsByTick (firstSnapshot : rest))

waveLineCells :: (SpiPinsSnapshot -> Bool) -> SpiPinsSnapshot -> [[SpiPinsSnapshot]] -> [Text]
waveLineCells _ _ [] = []
waveLineCells pin previous (snapshots : rest) =
    let bits = map pin snapshots
        cell = waveCell (pin previous) bits
        previous' = lastSnapshot previous snapshots
     in cell : waveLineCells pin previous' rest

snapshotsByTick :: [SpiPinsSnapshot] -> [[SpiPinsSnapshot]]
snapshotsByTick [] = []
snapshotsByTick xs =
    [filter (\snapshot -> spsTick snapshot == tick) xs | tick <- waveTicks xs]

lastSnapshot :: SpiPinsSnapshot -> [SpiPinsSnapshot] -> SpiPinsSnapshot
lastSnapshot previous [] = previous
lastSnapshot _ (snapshot : rest) = lastSnapshot snapshot rest

waveCell :: Bool -> [Bool] -> Text
waveCell firstBit bits =
    T.pack $ take spiWaveTickWidth $ drawBits (firstBit : bits)

drawBits :: [Bool] -> String
drawBits [] = []
drawBits [bitValue] = repeat $ levelChar bitValue
drawBits (a : b : rest)
    | a == b = levelChar a : drawBits (b : rest)
    | otherwise = levelChar a : edgeChar a b : drawBits (b : rest)

levelChar :: Bool -> Char
levelChar True = '‾'
levelChar False = '_'

edgeChar :: Bool -> Bool -> Char
edgeChar False True = '/'
edgeChar True False = '\\'
edgeChar _ _ = '_'

spiStatusText :: Int -> SpiDevice w -> Text
spiStatusText clock SpiDevice{spiMisoPending, spiMisoShift} =
    if any pendingMisoReady spiMisoPending || isJust spiMisoShift then "miso_ready" else "miso_empty"
    where
        pendingMisoReady (_, tick, _) = tick <= clock

spiDeviceClock :: Int -> SpiDevice w -> Int
spiDeviceClock _hwClock SpiDevice{spiSoftClock} = spiSoftClock
