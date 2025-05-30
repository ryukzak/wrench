{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- NOTE: http://wpage.unina.it/rcanonic/didattica/ce1/docs/68000.pdf

module Wrench.Isa.M68k (
    Isa (..),
    M68kState,
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
    -- -- | Byte
    deriving (Eq, Show)

data DataReg = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7
    deriving (Eq, Generic, Hashable, Read, Show)

instance (Default w) => Default (HashMap DataReg w) where
    def = fromList $ map (,def) [D0, D1, D2, D3, D4, D5, D6, D7]

data AddrReg = A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7
    deriving (Eq, Generic, Hashable, Read, Show)

instance (Default w) => Default (HashMap AddrReg w) where
    def = fromList $ map (,def) [A0, A1, A2, A3, A4, A5, A6, A7]

data Argument w l
    = DirectDataReg DataReg
    | DirectAddrReg AddrReg
    | IndirectAddrReg Int AddrReg
    | -- | IndirectAddrRegPostIncrement AddrReg
      -- | IndirectAddrRegPreDecrement AddrReg
      -- | IndirectWithIndexRegister Int AddrReg DataReg
      Immediate l
    deriving (Show)

-- Address with post-increment, e.g. (A0)+
-- Address with pre-decrement, e.g. −(A0)
-- Address with a 16-bit signed offset, e.g. 16(A0)
-- Register indirect with index register & 8-bit signed offset e.g. 8(A0,D0) or 8(A0,A1)
-- Note that for (A0)+ and −(A0), the actual increment or decrement value is dependent on the operand size: a byte access adjusts the address register by 1, a word by 2, and a long by 4.

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
    deriving (Show)

instance CommentStart (Isa w l) where
    commentStart = ";"

instance (MachineWord w) => MnemonicParser (Isa w (Ref w)) where
    mnemonic =
        choice
            [ cmd2args "move" Move src dst
            , cmd2args "movea" MoveA src addrRegister
            , cmd1args "not" Not dst
            , cmd2args "and" And src dst
            , cmd2args "or" Or src dst
            , cmd2args "xor" Xor src dst
            , cmd2args "add" Add src dst
            , cmd2args "sub" Sub src dst
            , cmd2args "mul" Mul src dst
            , cmd2args "div" Div src dst
            , cmd2args "asl" Asl (dataRegister <|> immidiate) dst
            , cmd2args "asr" Asr (dataRegister <|> immidiate) dst
            , cmd2args "lsl" Lsl (dataRegister <|> immidiate) dst
            , cmd2args "lsr" Lsr (dataRegister <|> immidiate) dst
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
            src = dataRegister <|> indirectAddrRegister <|> immidiate
            dst = dataRegister <|> indirectAddrRegister

cmd0args :: String -> Isa w (Ref w) -> Parser (Isa w (Ref w))
cmd0args mnemonic constructor = try $ do
    void $ string mnemonic
    eol' ";"
    return constructor

cmd1args ::
    String
    -> (Mode -> a -> Isa w (Ref w))
    -> Parser a
    -> Parser (Isa w (Ref w))
cmd1args mnemonic constructor dstP = try $ do
    m <- do
        void $ string mnemonic
        m <- cmdMode
        hspace1
        return m
    a <- dstP
    eol' ";"
    return $ constructor m a

cmd2args ::
    String
    -> (Mode -> a -> b -> Isa w (Ref w))
    -> Parser a
    -> Parser b
    -> Parser (Isa w (Ref w))
cmd2args mnemonic constructor srcP dstP = do
    m <- try $ do
        void $ string mnemonic
        m <- cmdMode
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

cmdMode = void (string ".l") >> return Long

comma :: Parser ()
comma = hspace >> void (string ",") >> hspace

dataRegister = try $ do
    void (string "D")
    n <- oneOf ['0' .. '7']
    return $ DirectDataReg $ Unsafe.read ['D', n]

addrRegister = try $ do
    void (string "A")
    n <- oneOf ['0' .. '7']
    return $ DirectAddrReg $ Unsafe.read ['A', n]

indirectAddrRegister = try $ do
    void (string "(")
    hspace
    void (string "A")
    n <- oneOf ['0' .. '7']
    hspace
    void (string ")")
    return $ IndirectAddrReg 0 $ Unsafe.read ['A', n]

immidiate :: (MachineWord w) => Parser (Argument w (Ref w))
immidiate = Immediate <$> reference

instance DerefMnemonic (Isa w) w where
    derefMnemonic f _offset i =
        let derefArg (DirectDataReg r) = DirectDataReg r
            derefArg (DirectAddrReg r) = DirectAddrReg r
            derefArg (IndirectAddrReg offset r) = IndirectAddrReg offset r
            -- derefArg (IndirectWithIndexRegister offset r d) = IndirectWithIndexRegister offset r d
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

instance ByteLength (Isa w l) where
    byteLength _ = 4 -- Simplified assumption: all instructions are 2 bytes.

type M68kState w = MachineState (IoMem (Isa w w) w) w

data MachineState mem w = State
    { pc :: Int
    , dr :: HashMap DataReg w
    , ar :: HashMap AddrReg w
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
        Right (pc, instruction) -> setPc (pc + byteLength instruction)
        Left err -> raiseInternalError $ "nextPc: " <> err

raiseInternalError :: Text -> State (MachineState (IoMem (Isa w w) w) w) ()
raiseInternalError msg = modify $ \st -> st{internalError = Just msg}

instance (MachineWord w) => InitState (IoMem (Isa w w) w) (MachineState (IoMem (Isa w w) w) w) where
    initState pc dump =
        State
            { pc
            , dr = def
            , ar = def
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
    reprState labels st@State{ar, dr} v =
        case T.splitOn ":" v of
            [r] -> reprState labels st (r <> ":dec")
            [r, f]
                | Just r' <- readMaybe (toString r)
                , Just r'' <- dr !? r' ->
                    viewRegister f r''
                | Just r' <- readMaybe (toString r)
                , Just r'' <- ar !? r' ->
                    viewRegister f r''
            _ -> errorView v

fetch :: (MachineWord w) => Mode -> Argument w w -> State (MachineState (IoMem (Isa w w) w) w) w
fetch _ (DirectDataReg r) = do
    State{dr} <- get
    return $ fromMaybe (error $ "invalid register: " <> show r) (dr !? r)
fetch _ (IndirectAddrReg offset r) = do
    st@State{ar, mem} <- get
    let addr = maybe (error $ "Invalid register: " <> show r) ((+ offset) . fromEnum) (ar !? r)
    case readWord mem addr of
        Right (mem', w) -> do
            put st{mem = mem'}
            return w
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return def
fetch _ (Immediate v) = return v
fetch _ (DirectAddrReg _) = error "not implemented"

store :: (MachineWord w) => Mode -> Argument w w -> w -> State (MachineState (IoMem (Isa w w) w) w) ()
store _ (DirectDataReg r) v = modify $ \st@State{dr} -> st{dr = insert r v dr, zFlag = v == 0, nFlag = v < 0}
store _ (DirectAddrReg r) v = modify $ \st@State{ar} -> st{ar = insert r v ar}
store _ (IndirectAddrReg offset r) v = do
    st@State{ar, mem} <- get
    let addr = maybe (error "Invalid register") ((+ offset) . fromEnum) (ar !? r)
    case writeWord mem addr v of
        Right mem' -> do
            put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err
store _ (Immediate _) _ = error "impossible to store into immediate destination"

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

    instructionStep = do
        (_pc, instruction) <- either (error . ("internal error: " <>)) id <$> instructionFetch
        case instruction of
            Move{mode, src, dst} -> cmd1 mode src dst id
            MoveA{mode, src, dst} -> cmd1 mode src dst id
            Not{mode, dst} -> cmd1 mode dst dst complement
            And{mode, src, dst} -> cmd2 mode src dst (.&.)
            Or{mode, src, dst} -> cmd2 mode src dst (.|.)
            Xor{mode, src, dst} -> cmd2 mode src dst xor
            Add{mode, src, dst} -> cmd2Ext mode src dst addExt
            Sub{mode, src, dst} -> cmd2Ext mode src dst subExt
            Mul{mode, src, dst} -> cmd2Ext mode src dst mulExt
            Div{mode, src, dst} -> cmd2 mode src dst div
            Asl{mode, src, dst} -> cmd2 mode src dst (\d s -> shiftL d (fromEnum s))
            Asr{mode, src, dst} -> cmd2 mode src dst (\d s -> shiftR d (fromEnum s))
            Lsl{mode, src, dst} -> cmd2 mode src dst lShiftL
            Lsr{mode, src, dst} -> cmd2 mode src dst lShiftR
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
            cmd1 mode src dst f = do
                a <- fetch mode src
                let result = f a
                store mode dst result
                nextPc
            cmd2 mode src dst f = do
                a <- fetch mode dst
                b <- fetch mode src
                let result = f a b
                store mode dst result
                nextPc
            cmd2Ext mode src dst f = do
                a <- fetch mode dst
                b <- fetch mode src
                let Ext{value, carry, overflow} = f a b
                store mode dst value
                modify $ \st ->
                    st
                        { nFlag = value < 0
                        , zFlag = value == 0
                        , vFlag = overflow
                        , cFlag = carry
                        }
                nextPc
