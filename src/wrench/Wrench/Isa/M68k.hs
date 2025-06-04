{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- NOTE: http://wpage.unina.it/rcanonic/didattica/ce1/docs/68000.pdf

module Wrench.Isa.M68k (
    Isa (..),
    Argument (..),
    Mode (..),
    M68kState,
    MachineState (..),
    IndexRegister (..),
    DataReg (..),
    dataRegisters,
    AddrReg (..),
) where

import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.Default (Default, def)
import Data.Text qualified as T
import Relude
import Relude.Extra
import Relude.Unsafe qualified as Unsafe
import Text.Megaparsec (choice, oneOf, try)
import Text.Megaparsec.Char (hspace, hspace1, string)
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Report
import Wrench.Translator.Parser.Misc
import Wrench.Translator.Parser.Types
import Wrench.Translator.Types

data Mode
    = Long
    | Byte
    deriving (Eq, Show)

longMode = void (string ".l") >> return Long

byteMode = void (string ".b") >> return Byte

data DataReg = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7
    deriving (Eq, Generic, Hashable, Read, Show)

dataRegisters :: [DataReg]
dataRegisters = [D0, D1, D2, D3, D4, D5, D6, D7]

instance (Default w) => Default (HashMap DataReg w) where
    def = fromList $ map (,def) dataRegisters

data AddrReg = A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7
    deriving (Eq, Generic, Hashable, Read, Show)

instance (Default w) => Default (HashMap AddrReg w) where
    def = fromList $ map (,def) [A0, A1, A2, A3, A4, A5, A6, A7]

data IndexRegister = AddrIndex AddrReg | DataIndex DataReg
    deriving (Eq, Show)

-- | Note that for (A0)+ and −(A0), the actual increment or decrement value is dependent on the operand size: a byte access adjusts the address register by 1, a word by 2, and a long by 4.
data Argument w l
    = DirectDataReg DataReg
    | DirectAddrReg AddrReg
    | -- | Address with a 16-bit signed offset, e.g. 16(A0)
      -- TODO: Register indirect with index register & 8-bit signed offset e.g. 8(A0,D0) or 8(A0,A1)
      IndirectAddrReg Int AddrReg (Maybe IndexRegister)
    | -- | Address with pre-decrement, e.g. −(A0)
      IndirectAddrRegPreDecrement AddrReg
    | -- | Address with post-increment, e.g. (A0)+
      IndirectAddrRegPostIncrement AddrReg
    | Immediate l
    deriving (Eq, Show)

-- | The 'Isa' type represents the instruction set architecture for the M68k machine.
-- Each constructor corresponds to a specific instruction.
data Isa w l
    = Move {mode :: Mode, src, dst :: Argument w l}
    | MoveA {mode :: Mode, src, dst :: Argument w l}
    | Not {mode :: Mode, dst :: Argument w l}
    | And {mode :: Mode, src, dst :: Argument w l}
    | Or {mode :: Mode, src, dst :: Argument w l}
    | Xor {mode :: Mode, src, dst :: Argument w l}
    | Add {mode :: Mode, src, dst :: Argument w l}
    | Sub {mode :: Mode, src, dst :: Argument w l}
    | Mul {mode :: Mode, src, dst :: Argument w l}
    | Div {mode :: Mode, src, dst :: Argument w l}
    | Asl {mode :: Mode, src, dst :: Argument w l}
    | Asr {mode :: Mode, src, dst :: Argument w l}
    | Lsl {mode :: Mode, src, dst :: Argument w l}
    | Lsr {mode :: Mode, src, dst :: Argument w l}
    | Jmp {ref :: l}
    | Bcc {ref :: l}
    | Bcs {ref :: l}
    | Beq {ref :: l}
    | Bne {ref :: l}
    | Blt {ref :: l}
    | Bgt {ref :: l}
    | Ble {ref :: l}
    | Bge {ref :: l}
    | Bmi {ref :: l}
    | Bpl {ref :: l}
    | Bvc {ref :: l}
    | Bvs {ref :: l}
    | Halt
    deriving (Eq, Show)

instance CommentStart (Isa w l) where
    commentStart = ";"

instance (MachineWord w) => MnemonicParser (Isa w (Ref w)) where
    mnemonic =
        choice
            [ cmd2args "move" Move (longMode <|> byteMode) src dst
            , cmd2args "movea" MoveA longMode src addrRegister
            , cmd1args "not" Not (longMode <|> byteMode) dst
            , cmd2args "and" And (longMode <|> byteMode) src dst
            , cmd2args "or" Or (longMode <|> byteMode) src dst
            , cmd2args "xor" Xor (longMode <|> byteMode) src dst
            , cmd2args "add" Add (longMode <|> byteMode) src dst
            , cmd2args "sub" Sub (longMode <|> byteMode) src dst
            , cmd2args "mul" Mul (longMode <|> byteMode) src dst
            , cmd2args "div" Div (longMode <|> byteMode) src dst
            , cmd2args "asl" Asl (longMode <|> byteMode) (dataRegister <|> immidiate) dst
            , cmd2args "asr" Asr (longMode <|> byteMode) (dataRegister <|> immidiate) dst
            , cmd2args "lsl" Lsl (longMode <|> byteMode) (dataRegister <|> immidiate) dst
            , cmd2args "lsr" Lsr (longMode <|> byteMode) (dataRegister <|> immidiate) dst
            , branchCmd "jmp" Jmp reference
            , branchCmd "bcc" Bcc reference
            , branchCmd "bcs" Bcs reference
            , branchCmd "beq" Beq reference
            , branchCmd "bne" Bne reference
            , branchCmd "blt" Blt reference
            , branchCmd "bgt" Bgt reference
            , branchCmd "ble" Ble reference
            , branchCmd "bge" Bge reference
            , branchCmd "bmi" Bmi reference
            , branchCmd "bpl" Bpl reference
            , branchCmd "bvc" Bvc reference
            , branchCmd "bvs" Bvs reference
            , cmd0args "halt" Halt
            ]
        where
            src = dataRegister <|> allIndirectAddr <|> immidiate
            dst = dataRegister <|> allIndirectAddr

cmd0args :: String -> Isa w (Ref w) -> Parser (Isa w (Ref w))
cmd0args mnemonic constructor = try $ do
    void $ string mnemonic
    eol' ";"
    return constructor

cmd1args ::
    String
    -> (Mode -> a -> Isa w (Ref w))
    -> Parser Mode
    -> Parser a
    -> Parser (Isa w (Ref w))
cmd1args mnemonic constructor modeP dstP = try $ do
    m <- do
        void $ string mnemonic
        m <- modeP
        hspace1
        return m
    a <- dstP
    eol' ";"
    return $ constructor m a

cmd2args ::
    String
    -> (Mode -> a -> b -> Isa w (Ref w))
    -> Parser Mode
    -> Parser a
    -> Parser b
    -> Parser (Isa w (Ref w))
cmd2args mnemonic constructor modeP srcP dstP = do
    m <- try $ do
        void $ string mnemonic
        m <- modeP
        hspace1
        return m
    a <- srcP
    comma
    b <- dstP
    eol' ";"
    return $ constructor m a b

branchCmd mnemonic constructor ref = do
    try $ do
        void $ string mnemonic
        hspace1
    a <- ref
    eol' ";"
    return $ constructor a

comma :: Parser ()
comma = hspace >> void (string ",") >> hspace

dataRegister' = try $ do
    void (string "D")
    n <- oneOf ['0' .. '7']
    return $ Unsafe.read ['D', n]

dataRegister = DirectDataReg <$> dataRegister'

addrRegister' = try $ do
    void (string "A")
    n <- oneOf ['0' .. '7']
    return $ Unsafe.read ['A', n]

addrRegister = DirectAddrReg <$> addrRegister'

indirectAddrRegister = try $ do
    offset <- readMaybe <$> choice [hexNum, num]
    void (string "(")
    hspace
    void (string "A")
    n <- oneOf ['0' .. '7']
    hspace
    index <- optional $ do
        void (string ",")
        hspace
        choice [DataIndex <$> dataRegister', AddrIndex <$> addrRegister']
    void (string ")")
    return $ IndirectAddrReg (fromMaybe 0 offset) (Unsafe.read ['A', n]) index

indirectAddrRegPreDecrement = try $ do
    void (string "-(")
    hspace
    void (string "A")
    n <- oneOf ['0' .. '7']
    hspace
    void (string ")")
    return $ IndirectAddrRegPreDecrement $ Unsafe.read ['A', n]

indirectAddrRegPostIncrement = try $ do
    void (string "(")
    hspace
    void (string "A")
    n <- oneOf ['0' .. '7']
    hspace
    void (string ")+")
    return $ IndirectAddrRegPostIncrement $ Unsafe.read ['A', n]

allIndirectAddr = indirectAddrRegPreDecrement <|> indirectAddrRegPostIncrement <|> indirectAddrRegister

immidiate :: (MachineWord w) => Parser (Argument w (Ref w))
immidiate = Immediate <$> reference

instance DerefMnemonic (Isa w) w where
    derefMnemonic f _offset i =
        let derefArg (DirectDataReg r) = DirectDataReg r
            derefArg (DirectAddrReg r) = DirectAddrReg r
            derefArg (IndirectAddrReg offset r index) = IndirectAddrReg offset r index
            derefArg (IndirectAddrRegPreDecrement r) = IndirectAddrRegPreDecrement r
            derefArg (IndirectAddrRegPostIncrement r) = IndirectAddrRegPostIncrement r
            derefArg (Immediate l) = Immediate $ deref' f l
         in case i of
                Move{mode, src, dst} -> Move mode (derefArg src) (derefArg dst)
                MoveA{mode, src, dst} -> MoveA mode (derefArg src) (derefArg dst)
                Not{mode, dst} -> Not mode (derefArg dst)
                And{mode, src, dst} -> And mode (derefArg src) (derefArg dst)
                Or{mode, src, dst} -> Or mode (derefArg src) (derefArg dst)
                Xor{mode, src, dst} -> Xor mode (derefArg src) (derefArg dst)
                Add{mode, src, dst} -> Add mode (derefArg src) (derefArg dst)
                Sub{mode, src, dst} -> Sub mode (derefArg src) (derefArg dst)
                Mul{mode, src, dst} -> Mul mode (derefArg src) (derefArg dst)
                Div{mode, src, dst} -> Div mode (derefArg src) (derefArg dst)
                Asl{mode, src, dst} -> Asl mode (derefArg src) (derefArg dst)
                Asr{mode, src, dst} -> Asr mode (derefArg src) (derefArg dst)
                Lsl{mode, src, dst} -> Lsl mode (derefArg src) (derefArg dst)
                Lsr{mode, src, dst} -> Lsr mode (derefArg src) (derefArg dst)
                Jmp{ref} -> Jmp (deref' f ref)
                Bcc{ref} -> Bcc (deref' f ref)
                Bcs{ref} -> Bcs (deref' f ref)
                Beq{ref} -> Beq (deref' f ref)
                Bne{ref} -> Bne (deref' f ref)
                Blt{ref} -> Blt (deref' f ref)
                Bgt{ref} -> Bgt (deref' f ref)
                Ble{ref} -> Ble (deref' f ref)
                Bge{ref} -> Bge (deref' f ref)
                Bmi{ref} -> Bmi (deref' f ref)
                Bpl{ref} -> Bpl (deref' f ref)
                Bvc{ref} -> Bvc (deref' f ref)
                Bvs{ref} -> Bvs (deref' f ref)
                Halt -> Halt

instance (ByteSizeT w) => ByteSize (Argument w l) where
    byteSize (DirectDataReg _) = 0
    byteSize (DirectAddrReg _) = 0
    byteSize (IndirectAddrReg{}) = 2
    byteSize (IndirectAddrRegPreDecrement _) = 0
    byteSize (IndirectAddrRegPostIncrement _) = 0
    byteSize (Immediate _) = byteSizeT @w

instance (ByteSizeT w) => ByteSize (Isa w l) where
    byteSize (Move _mode src dst) = 2 + byteSize src + byteSize dst
    byteSize (MoveA _mode src dst) = 2 + byteSize src + byteSize dst
    byteSize (Not _mode dst) = 2 + byteSize dst
    byteSize (And _mode src dst) = 2 + byteSize src + byteSize dst
    byteSize (Or _mode src dst) = 2 + byteSize src + byteSize dst
    byteSize (Xor _mode src dst) = 2 + byteSize src + byteSize dst
    byteSize (Add _mode src dst) = 2 + byteSize src + byteSize dst
    byteSize (Sub _mode src dst) = 2 + byteSize src + byteSize dst
    byteSize (Mul _mode src dst) = 2 + byteSize src + byteSize dst
    byteSize (Div _mode src dst) = 2 + byteSize src + byteSize dst
    byteSize (Asl _mode src dst) = 2 + byteSize src + byteSize dst
    byteSize (Asr _mode src dst) = 2 + byteSize src + byteSize dst
    byteSize (Lsl _mode src dst) = 2 + byteSize src + byteSize dst
    byteSize (Lsr _mode src dst) = 2 + byteSize src + byteSize dst
    byteSize (Jmp _) = 6
    byteSize (Bcc _) = 6
    byteSize (Bcs _) = 6
    byteSize (Beq _) = 6
    byteSize (Bne _) = 6
    byteSize (Blt _) = 6
    byteSize (Bgt _) = 6
    byteSize (Ble _) = 6
    byteSize (Bge _) = 6
    byteSize (Bmi _) = 6
    byteSize (Bpl _) = 6
    byteSize (Bvc _) = 6
    byteSize (Bvs _) = 6
    byteSize Halt = 2

type M68kState w = MachineState (IoMem (Isa w w) w) w

data MachineState mem w = State
    { pc :: Int
    , dataRegs :: HashMap DataReg w
    , addrRegs :: HashMap AddrReg w
    , mem :: mem
    , stopped :: Bool
    , internalError :: Maybe Text
    , nFlag, zFlag, vFlag, cFlag :: Bool
    }
    deriving (Show)

setPc :: forall w. Int -> State (MachineState (IoMem (Isa w w) w) w) ()
setPc addr = modify $ \st -> st{pc = addr}

nextPc :: (MachineWord w) => State (MachineState (IoMem (Isa w w) w) w) ()
nextPc = do
    instructionFetch >>= \case
        Right (pc, instruction) -> setPc (pc + byteSize instruction)
        Left err -> raiseInternalError $ "nextPc: " <> err

raiseInternalError :: Text -> State (MachineState (IoMem (Isa w w) w) w) ()
raiseInternalError msg = modify $ \st -> st{internalError = Just msg}

instance (MachineWord w) => InitState (IoMem (Isa w w) w) (MachineState (IoMem (Isa w w) w) w) where
    initState pc dump =
        State
            { pc
            , dataRegs = def
            , addrRegs = def
            , mem = dump
            , stopped = False
            , internalError = Nothing
            , nFlag = False
            , zFlag = True
            , vFlag = False
            , cFlag = False
            }

instance (MachineWord w) => StateInterspector (MachineState (IoMem (Isa w w) w) w) (IoMem (Isa w w) w) (Isa w w) w where
    programCounter State{pc} = pc
    memoryDump State{mem} = mem
    ioStreams State{mem = IoMem{mIoStreams}} = mIoStreams
    reprState labels st v
        | Just v' <- defaultView labels st v = v'
    reprState labels st@State{addrRegs, dataRegs} v =
        case T.splitOn ":" v of
            [r] -> reprState labels st (r <> ":dec")
            [r, f]
                | Just r' <- readMaybe (toString r)
                , Just r'' <- dataRegs !? r' ->
                    viewRegister f r''
                | Just r' <- readMaybe (toString r)
                , Just r'' <- addrRegs !? r' ->
                    viewRegister f r''
            _ -> errorView v

indirectAddr f r index = do
    State{addrRegs, dataRegs} <- get
    let offset = fromEnum $ case index of
            Just (DataIndex r) -> fromMaybe (error $ "invalid register: " <> show r) $ dataRegs !? r
            Just (AddrIndex r) -> fromMaybe (error $ "invalid register: " <> show r) $ addrRegs !? r
            Nothing -> 0
    case addrRegs !? r of
        Just addr -> return $ offset + f (fromEnum addr)
        Nothing -> error $ "Invalid register: " <> show r

readMemoryWord addr = do
    st@State{mem} <- get
    case readWord mem addr of
        Right (mem', w) -> do
            put st{mem = mem'}
            return w
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return def

writeMemoryWord addr w = do
    st@State{mem} <- get
    case writeWord mem addr w of
        Right mem' -> do
            put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err

fetchWord :: (MachineWord w) => Argument w w -> State (MachineState (IoMem (Isa w w) w) w) w
fetchWord (DirectDataReg r) = do
    State{dataRegs} <- get
    return $ fromMaybe (error $ "invalid register: " <> show r) (dataRegs !? r)
fetchWord (IndirectAddrReg offset r index) = do
    addr <- indirectAddr (+ offset) r index
    readMemoryWord addr
fetchWord (IndirectAddrRegPreDecrement r) = do
    addr <- indirectAddr (\a -> a - 4) r Nothing
    storeWord (DirectAddrReg r) $ toEnum addr
    readMemoryWord addr
fetchWord (IndirectAddrRegPostIncrement r) = do
    addr <- indirectAddr id r Nothing
    w <- readMemoryWord addr
    storeWord (DirectAddrReg r) $ toEnum (addr + 4)
    return w
fetchWord (Immediate v) = return v
fetchWord arg = error $ "can not fetch word: " <> show arg

storeWord :: (MachineWord w) => Argument w w -> w -> State (MachineState (IoMem (Isa w w) w) w) ()
storeWord (DirectDataReg r) v = modify $ \st@State{dataRegs} -> st{dataRegs = insert r v dataRegs, zFlag = v == 0, nFlag = v < 0}
storeWord (DirectAddrReg r) v = modify $ \st@State{addrRegs} -> st{addrRegs = insert r v addrRegs}
storeWord (IndirectAddrReg offset r index) v = do
    addr <- indirectAddr (+ offset) r index
    writeMemoryWord addr v
storeWord (IndirectAddrRegPreDecrement r) v = do
    addr <- indirectAddr (\a -> a - 4) r Nothing
    storeWord (DirectAddrReg r) $ toEnum addr
    writeMemoryWord addr v
storeWord (IndirectAddrRegPostIncrement r) v = do
    addr <- indirectAddr id r Nothing
    storeWord (DirectAddrReg r) $ toEnum (addr + 4)
    writeMemoryWord addr v
storeWord arg _ = error $ "can not store word: " <> show arg

readMemoryByte addr = do
    st@State{mem} <- get
    case readByte mem addr of
        Right (mem', b) -> do
            put st{mem = mem'}
            return $ toSign b
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return def

writeMemoryByte addr b = do
    st@State{mem} <- get
    case writeByte mem addr (fromSign b) of
        Right mem' -> do
            put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err

fetchByte :: (MachineWord w) => Argument w w -> State (MachineState (IoMem (Isa w w) w) w) Int8
fetchByte (DirectDataReg r) = do
    State{dataRegs} <- get
    return $ maybe (error $ "invalid register: " <> show r) (fromInteger . toInteger) (dataRegs !? r)
fetchByte (IndirectAddrReg offset r index) = do
    addr <- indirectAddr (+ offset) r index
    readMemoryByte addr
fetchByte (IndirectAddrRegPreDecrement r) = do
    addr <- indirectAddr (subtract 1) r Nothing
    storeWord (DirectAddrReg r) $ toEnum addr
    readMemoryByte addr
fetchByte (IndirectAddrRegPostIncrement r) = do
    addr <- indirectAddr id r Nothing
    b <- readMemoryByte addr
    storeWord (DirectAddrReg r) $ toEnum (addr + 1)
    return b
fetchByte (Immediate v) = return $ fromInteger $ toInteger v
fetchByte arg = error $ "can not fetch byte: " <> show arg

storeByte :: forall w. (MachineWord w) => Argument w w -> Int8 -> State (MachineState (IoMem (Isa w w) w) w) ()
storeByte (DirectDataReg r) v = do
    st@State{dataRegs} <- get
    let w = fromMaybe (error $ "invalid register: " <> show r) $ dataRegs !? r
        w' = (w .&. 0xFFFFFF00) .|. fromInteger (toInteger v)
    put st{dataRegs = insert r w' dataRegs, zFlag = v == 0, nFlag = v < 0}
storeByte (IndirectAddrReg offset r index) v = do
    addr <- indirectAddr (+ offset) r index
    writeMemoryByte addr v
storeByte (IndirectAddrRegPreDecrement r) v = do
    addr <- indirectAddr (subtract 1) r Nothing
    storeWord (DirectAddrReg r) $ toEnum addr
    writeMemoryByte addr v
storeByte (IndirectAddrRegPostIncrement r) v = do
    addr <- indirectAddr id r Nothing
    storeWord (DirectAddrReg r) $ toEnum (addr + 1)
    writeMemoryByte addr v
storeByte (Immediate _) _ = error "impossible to store into immediate destination"
storeByte arg _ = error $ "can not store byte: " <> show arg

instance (MachineWord w) => Machine (MachineState (IoMem (Isa w w) w) w) (Isa w w) w where
    instructionFetch =
        get
            <&> ( \case
                    State{stopped = True} -> Left halted
                    State{internalError = Just err} -> Left err
                    State{pc, mem} -> do
                        instruction <- readInstruction mem pc
                        return (pc, instruction)
                )

    instructionExecute _pc instruction = do
        case instruction of
            Move{mode = Long, src, dst} -> wordCmd1 src dst id
            Move{mode = Byte, src, dst} -> byteCmd1 src dst id
            MoveA{mode = Long, src, dst} -> wordCmd1 src dst id
            MoveA{mode = Byte} -> error "not implemented"
            Not{mode = Long, dst} -> wordCmd1 dst dst complement
            Not{mode = Byte, dst} -> byteCmd1 dst dst complement
            And{mode = Long, src, dst} -> wordCmd2 src dst (.&.)
            And{mode = Byte, src, dst} -> byteCmd2 src dst (.&.)
            Or{mode = Long, src, dst} -> wordCmd2 src dst (.|.)
            Or{mode = Byte, src, dst} -> byteCmd2 src dst (.|.)
            Xor{mode = Long, src, dst} -> wordCmd2 src dst xor
            Xor{mode = Byte, src, dst} -> byteCmd2 src dst xor
            Add{mode = Long, src, dst} -> wordCmd2Ext src dst addExt
            Add{mode = Byte, src, dst} -> byteCmd2Ext src dst addExt
            Sub{mode = Long, src, dst} -> wordCmd2Ext src dst subExt
            Sub{mode = Byte, src, dst} -> byteCmd2Ext src dst subExt
            Mul{mode = Long, src, dst} -> wordCmd2Ext src dst mulExt
            Mul{mode = Byte, src, dst} -> byteCmd2Ext src dst mulExt
            Div{mode = Long, src, dst} -> wordCmd2 src dst div
            Div{mode = Byte, src, dst} -> byteCmd2 src dst div
            Asl{mode = Long, src, dst} -> wordCmd2 src dst (\d s -> shiftL d (fromEnum s))
            Asl{mode = Byte, src, dst} -> byteCmd2 src dst (\d s -> shiftL d (fromEnum s))
            Asr{mode = Long, src, dst} -> wordCmd2 src dst (\d s -> shiftR d (fromEnum s))
            Asr{mode = Byte, src, dst} -> byteCmd2 src dst (\d s -> shiftR d (fromEnum s))
            Lsl{mode = Long, src, dst} -> wordCmd2 src dst lShiftL
            Lsl{mode = Byte, src, dst} -> byteCmd2 src dst lShiftL
            Lsr{mode = Long, src, dst} -> wordCmd2 src dst lShiftR
            Lsr{mode = Byte, src, dst} -> byteCmd2 src dst lShiftR
            Jmp{ref} -> branch ref True
            Bcc{ref} -> get >>= branch ref . not . cFlag
            Bcs{ref} -> get >>= branch ref . cFlag
            Beq{ref} -> get >>= branch ref . zFlag
            Bne{ref} -> get >>= branch ref . not . zFlag
            Blt{ref} -> get >>= branch ref . (\st -> nFlag st /= vFlag st)
            Bgt{ref} -> get >>= branch ref . (\st -> not (zFlag st) && (nFlag st == vFlag st))
            Ble{ref} -> get >>= branch ref . (\st -> zFlag st || (nFlag st /= vFlag st))
            Bge{ref} -> get >>= branch ref . (\st -> nFlag st == vFlag st)
            Bmi{ref} -> get >>= branch ref . nFlag
            Bpl{ref} -> get >>= branch ref . not . nFlag
            Bvc{ref} -> get >>= branch ref . not . vFlag
            Bvs{ref} -> get >>= branch ref . vFlag
            Halt -> modify $ \st -> st{stopped = True}
        where
            branch addr True = setPc $ fromEnum addr
            branch _addr False = nextPc
            wordCmd1 src dst f = do
                a <- fetchWord src
                storeWord dst $ f a
                nextPc
            wordCmd2 src dst f = do
                a <- fetchWord dst
                b <- fetchWord src
                storeWord dst $ f a b
                nextPc
            wordCmd2Ext src dst f = do
                a <- fetchWord dst
                b <- fetchWord src
                let Ext{value, carry, overflow} = f a b
                storeWord dst value
                modify $ \st ->
                    st
                        { nFlag = value < 0
                        , zFlag = value == 0
                        , vFlag = overflow
                        , cFlag = carry
                        }
                nextPc
            byteCmd1 src dst f = do
                a <- fetchByte src
                storeByte dst $ f a
                nextPc
            byteCmd2 src dst f = do
                a <- fetchByte dst
                b <- fetchByte src
                storeByte dst $ f a b
                nextPc
            byteCmd2Ext src dst f = do
                a <- fetchByte dst
                b <- fetchByte src
                let Ext{value, carry, overflow} = f a b
                storeByte dst value
                modify $ \st ->
                    st
                        { nFlag = value < 0
                        , zFlag = value == 0
                        , vFlag = overflow
                        , cFlag = carry
                        }
                nextPc
