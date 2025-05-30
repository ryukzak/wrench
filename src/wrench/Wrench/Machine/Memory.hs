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

import Data.Bits (FiniteBits, finiteBitSize)
import Data.Default (Default, def)
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
    forall w isa. (ByteSize isa, Default w, FiniteBits w) => IoMem isa w -> Int -> isa -> Bool
ioPortInstructionCollision IoMem{mIoKeys} addr instr =
    let !n = byteSize instr
        wn = finiteBitSize (def :: w) `div` 8
        !result = any (\idx -> (idx - n + 1 <= addr && addr <= idx - 1) || (idx + 1 <= addr && addr <= idx + wn - 1)) mIoKeys
     in result

ioPortWordCollision :: forall w isa. (Default w, FiniteBits w) => IoMem isa w -> Int -> Bool
ioPortWordCollision IoMem{mIoKeys} addr =
    let n = finiteBitSize (def :: w) `div` 8
     in any (\idx -> (idx - n + 1 <= addr && addr <= idx - 1) || (idx + 1 <= addr && addr <= idx + n - 1)) mIoKeys

ioPortByteCollision :: forall w isa. (Default w, FiniteBits w) => IoMem isa w -> Int -> Bool
ioPortByteCollision IoMem{mIoKeys} addr =
    let n = finiteBitSize (def :: w) `div` 8
        mkParts idx = [idx + 1 .. idx + n - 1]
        parts = concatMap mkParts mIoKeys
     in (addr `elem` parts)

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
    readByte io@IoMem{mIoCells} idx = do
        (mIoCells', v) <- readByte mIoCells idx
        return (io{mIoCells = mIoCells'}, v)

    readWord io idx | ioPortWordCollision io idx = Left $ "iomemory[" <> show idx <> "]: can't read word from input port"
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
    writeWord io idx word =
        case mIoStreams io !? idx of
            Just (is, os) -> Right io{mIoStreams = insert idx (is, word : os) (mIoStreams io)}
            Nothing -> do
                mIoCells' <- writeWord (mIoCells io) idx word
                return io{mIoCells = mIoCells'}

    writeByte io idx _byte
        | ioPortByteCollision io idx =
            Left $ "iomemory[" <> show idx <> "]: can't write byte to input port"
    writeByte io idx byte =
        case mIoStreams io !? idx of
            Just (is, os) -> Right io{mIoStreams = insert idx (is, byteToWord byte : os) (mIoStreams io)}
            Nothing -> do
                mIoCells' <- writeByte (mIoCells io) idx byte
                return io{mIoCells = mIoCells'}

    dumpCells = memoryData . mIoCells
