{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Machine.Memory (
    sliceMem,
    Mem,
    Cell (..),
    Memory (..),
    WordParts (..),
    word32ToHex,
    prepareDump,
    prettyDump,
) where

import Data.Default (def)
import Machine.Types
import Numeric (showHex)
import Relude
import Relude.Extra
import Relude.Unsafe qualified as Unsafe
import Translator.Types

prepareDump :: (MachineWord w, ByteLength isa) => Maybe Int -> [Section isa w w] -> Mem isa w
prepareDump memorySize sections =
    let mSize = fromMaybe (foldr ((+) . byteLength) 0 sections) memorySize
     in fromList
            $ zip
                [0 .. mSize - 1]
                ( concatMap
                    ( \case
                        Code{codeTokens} ->
                            ( concatMap
                                ( \case
                                    Mnemonic m ->
                                        Instruction m : replicate (byteLength m - 1) InstructionPart
                                    _other -> []
                                )
                                codeTokens
                            )
                        Data{dataTokens} ->
                            ( concatMap
                                ( \DataToken{dtValue} ->
                                    map
                                        Value
                                        $ case dtValue of
                                            DByte bs -> bs
                                            DWord ws -> concatMap wordSplit ws
                                )
                                dataTokens
                            )
                    )
                    sections
                    <> repeat (Value 0)
                )

isValue Value{} = True
isValue _ = False

sliceMem addrs dump = map (\a -> (a, Unsafe.fromJust (dump !? a))) addrs

prettyDump ::
    forall w isa.
    (MachineWord w, ByteLength isa, Show isa) =>
    HashMap String w
    -> Mem isa w
    -> String
prettyDump labels dump = intercalate "\n" $ pretty $ toPairs dump
    where
        offset2label :: HashMap Int String
        offset2label = fromList $ map (\(a, b) -> (fromEnum b, a)) $ toPairs labels
        instruction offset n i =
            let place = "mem[" <> show offset <> ".." <> show (offset + n - 1) <> "]"
                label = maybe "" (" \t@" <>) (offset2label !? offset)
             in place <> ": \t" <> show i <> label
        pretty [] = []
        pretty ((offset, Instruction i) : cs) =
            let n = byteLength i
                cs' = drop (n - 1) cs
             in instruction offset n i : pretty cs'
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
    readInstruction :: Int -> State m isa
    readWord :: Int -> State m w
    writeWord :: Int -> w -> State m ()

instance
    (MachineWord w) =>
    Memory (Mem isa w) isa w
    where
    readInstruction idx = do
        mem <- get
        case mem !? idx of
            Just (Instruction i) -> return i
            Just InstructionPart -> error $ "Can't interpret value as instruction: " <> show idx <> " value: InstructionPart"
            Just (Value v) -> error $ "Can't interpret value as instruction: " <> show idx <> " value: " <> show v
            Nothing -> error "Out of memory."

    readWord idx = do
        mem <- get
        let idxs = [idx .. idx + byteLength (def :: w) - 1]
            bytes = map (getValue mem) idxs
        return $ wordCombine bytes
        where
            getValue mem i =
                case mem !? i of
                    Just (Value v) -> v
                    Just _ -> 0xAA -- error $ "Can't interpret instruction as data at " <> show i <> " value: " <> show x
                    Nothing -> error $ "Out of memory at index: " <> show i

    writeWord idx word = modify $ \mem ->
        let updates = zip [idx ..] (wordSplit word)
         in foldl' (\m (i, x) -> insert i (Value x) m) mem updates

instance (Memory (Mem isa w) isa w) => Memory (IoMem isa w) isa w where
    readInstruction idx = do
        IoMem{mIoStreams, mIoCells} <- get
        case mIoStreams !? idx of
            Just _ -> error $ "Can't read instruction from input port: " <> show idx
            Nothing -> return $ evalState (readInstruction idx) mIoCells

    readWord idx = do
        io@IoMem{mIoStreams, mIoCells} <- get
        case mIoStreams !? idx of
            Just ([], _) -> error "Input is depleted."
            Just (i : is, os) -> do
                put io{mIoStreams = insert idx (is, os) mIoStreams}
                return i
            Nothing -> return $ evalState (readWord idx) mIoCells

    writeWord idx word = do
        m@IoMem{mIoStreams, mIoCells} <- get
        case mIoStreams !? idx of
            Just (is, os) -> put m{mIoStreams = insert idx (is, word : os) mIoStreams}
            Nothing -> do
                let mIoCells' = execState (writeWord idx word) mIoCells
                put m{mIoCells = mIoCells'}
