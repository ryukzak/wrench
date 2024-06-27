{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Machine.Memory (
    sliceMem,
    Mem,
    Cell (..),
    Memory (..),
    WordParts (..),
    prepareDump,
    prettyDump,
)
where

import Data.Default (def)
import Machine.Types
import Numeric (showHex)
import Relude
import Relude.Extra
import Relude.Unsafe qualified as Unsafe
import Translator.Types

prepareDump :: (MachineWord w, ByteLength isa) => Int -> [Section isa w w] -> Mem isa w
prepareDump memorySize sections =
    fromList
        $ zip
            [0 .. memorySize - 1]
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

prettyCell (Instruction isa) = show isa
prettyCell InstructionPart = "."
prettyCell (Value b) = word8ToHex b

sliceMem addrs dump = map (\a -> (a, Unsafe.fromJust (dump !? a))) addrs

prettyDump ::
    forall w isa.
    (MachineWord w, ByteLength isa, Show isa) =>
    HashMap String w -> Mem isa w -> String
prettyDump labels dump = intercalate "\n" $ pretty $ toPairs dump
    where
        offset2label :: HashMap w String
        offset2label = fromList $ map (\(a, b) -> (b, a)) $ toPairs labels
        instruction offset n i =
            let place = "mem[" <> show offset <> ".." <> show (offset + n - 1) <> "]"
                label = maybe "" (" \t@" <>) (offset2label !? toEnum offset)
             in place <> ": \t" <> show i <> label
        pretty [] = []
        pretty ((offset, Instruction i) : cs) =
            let n = byteLength i
                cs' = drop (n - 1) cs
             in instruction offset n i : pretty cs'
        pretty cs =
            let values = takeWhile (isValue . snd) cs
                cs' = dropWhile (isValue . snd) cs
                values' = toString $ unwords $ map (toText . prettyCell . snd) values
                offset = fst $ Unsafe.head values
             in ("mem[" <> show offset <> ".." <> show (offset + length values - 1) <> "]: \t" <> values') : pretty cs'

word8ToHex w =
    let hex = showHex w ""
     in if length hex == 1 then "0" <> hex else hex

class Memory m isa w | m -> isa w where
    readInstruction :: Int -> State m isa
    readWord :: Int -> State m w
    writeWord :: Int -> w -> State m ()

instance
    (MachineWord w, Show isa) =>
    Memory (Mem isa w) isa w
    where
    readInstruction idx = do
        mem <- get
        case mem !? idx of
            Just (Instruction i) -> return i
            Just InstructionPart -> error "Can't read instruction from partial data."
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
                    Just x -> error $ "Can't interpret instruction as data at " <> show i <> " value: " <> show x
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
