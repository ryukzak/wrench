{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Inspired by https://riscv.org/wp-content/uploads/2017/05/riscv-spec-v2.2.pdf
module Isa.Risc (
    Risc (..),
    Register (..),
    registers,
    MachineState (..),
)
where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Default
import Machine.Memory
import Machine.Types
import Relude
import Relude.Extra
import Relude.Unsafe qualified as Unsafe
import Text.Megaparsec (choice)
import Text.Megaparsec.Char (char, hspace, hspace1, string)
import Translator.Parser.Misc
import Translator.Parser.Types
import Translator.Types

-- * ISA

data Register
    = Zero
    | Ra
    | Sp
    | Gp
    | Tp
    | T0
    | T1
    | T2
    | S0Fp
    | S1
    | A0
    | A1
    | A2
    | A3
    | A4
    | A5
    | A6
    | A7
    | S2
    | S3
    | S4
    | S5
    | S6
    | S7
    | S8
    | S9
    | S10
    | S11
    | T3
    | T4
    | T5
    | T6
    deriving (Show, Eq, Generic, Read)

allRegisters = [Zero, Ra, Sp, Gp, Tp, T0, T1, T2, S0Fp, S1, A0, A1, A2, A3, A4, A5, A6, A7, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, T3, T4, T5, T6]

instance Hashable Register

instance (Default w) => Default (HashMap Register w) where
    def = fromList $ map (,def) allRegisters

data Risc w l
    = Addi {rd, rs1 :: Register, k :: l}
    | Add {rd, rs1, rs2 :: Register}
    | Sub {rd, rs1, rs2 :: Register}
    | Mul {rd, rs1, rs2 :: Register}
    | Mulh {rd, rs1, rs2 :: Register}
    | -- | rd = rs1 << rs2 (5 bit), logical shift
      Sll {rd, rs1, rs2 :: Register}
    | -- | rd = rs1 >> rs2 (5 bit), logical shift
      Srl {rd, rs1, rs2 :: Register}
    | -- | rd = rs1 >> rs2 (5 bit), arithmetic shift
      Sra {rd, rs1, rs2 :: Register}
    | -- | rd = rs1 & rs2
      And {rd, rs1, rs2 :: Register}
    | -- | rd = rs1 | rs2
      Or {rd, rs1, rs2 :: Register}
    | -- | rd = rs1 ^ rs2
      Xor {rd, rs1, rs2 :: Register}
    | -- | rd = rs
      Mv {rd, rs :: Register}
    | -- | rs2 -> M[offsetRs1]
      Sw {rs2 :: Register, offsetRs1 :: MemRef w}
    | -- | Rd + K << 12
      Lui {rd :: Register, k :: l}
    | -- | Rd + K << 12
      Li {rd :: Register, k :: l}
    | -- | rd <- M[rs]
      Lw {rd :: Register, offsetRs1 :: MemRef w}
    | -- | PC <- PC + K
      J {k :: l}
    | -- | if rs1 = 0 then pc += k
      Beqz {rs1 :: Register, k :: l}
    | -- | if rs1 /= 0 then pc += k
      Bnez {rs1 :: Register, k :: l}
    | -- | if rs1 > rs2 then pc += k
      Bgt {rs1, rs2 :: Register, k :: l}
    | -- | if rs1 <= rs2 then pc += k
      Ble {rs1, rs2 :: Register, k :: l}
    | -- | if rs1 > rs2 (unsigned) then pc += k
      Bgtu {rs1, rs2 :: Register, k :: l}
    | -- | if rs1 <= rs2 (unsigned) then pc += k
      Bleu {rs1, rs2 :: Register, k :: l}
    | Halt
    deriving (Show)

-- * Parser

register :: Parser Register
register =
    choice
        [ string "zero" >> return Zero
        , string "ra" >> return Ra
        , string "sp" >> return Sp
        , string "gp" >> return Gp
        , string "tp" >> return Tp
        , string "t0" >> return T0
        , string "t1" >> return T1
        , string "t2" >> return T2
        , string "s0" >> return S0Fp
        , string "fp" >> return S0Fp
        , string "s1" >> return S1
        , string "a0" >> return A0
        , string "a1" >> return A1
        , string "a2" >> return A2
        , string "a3" >> return A3
        , string "a4" >> return A4
        , string "a5" >> return A5
        , string "a6" >> return A6
        , string "a7" >> return A7
        , string "s2" >> return S2
        , string "s3" >> return S3
        , string "s4" >> return S4
        , string "s5" >> return S5
        , string "s6" >> return S6
        , string "s7" >> return S7
        , string "s8" >> return S8
        , string "s9" >> return S9
        , string "s10" >> return S10
        , string "s11" >> return S11
        , string "t3" >> return T3
        , string "t4" >> return T4
        , string "t5" >> return T5
        , string "t6" >> return T6
        ]

data MemRef w = MemRef {mrOffset :: w, mrReg :: Register} deriving (Show)

memRef :: (MachineWord w) => Parser (MemRef w)
memRef = choice [regWithOffset, register <&> MemRef 0]
    where
        regWithOffset = do
            mrOffset <- Unsafe.read <$> choice [hexNum, num]
            void $ char '('
            mrReg <- register
            void $ char ')'
            return MemRef{mrOffset, mrReg}

instance (MachineWord w) => MnemonicParser (Risc w (Ref w)) where
    mnemonic =
        hspace *> cmd <* eol'
        where
            cmd =
                choice
                    [ cmd3args "addi" Addi register register reference
                    , cmd3args "add" Add register register register
                    , cmd3args "sub" Sub register register register
                    , cmd3args "mul" Mul register register register
                    , cmd3args "mulh" Mulh register register register
                    , cmd3args "sll" Sll register register register
                    , cmd3args "srl" Srl register register register
                    , cmd3args "sra" Sra register register register
                    , cmd3args "and" And register register register
                    , cmd3args "or" Or register register register
                    , cmd3args "xor" Xor register register register
                    , cmd2args "mv" Mv register register
                    , cmd2args "sw" Sw register memRef
                    , cmd2args "lui" Lui register reference
                    , cmd2args "li" Li register reference
                    , cmd2args "lw" Lw register memRef
                    , cmd1args "j" J reference
                    , cmd2args "beqz" Beqz register reference
                    , cmd2args "bnez" Bnez register reference
                    , cmd3args "bgt" Bgt register register reference
                    , cmd3args "ble" Ble register register reference
                    , cmd3args "bgtu" Bgtu register register reference
                    , cmd3args "bleu" Bleu register register reference
                    , string "halt" >> return Halt
                    ]

instance (MachineWord w) => DerefMnemonic (Risc w) w where
    derefMnemonic f offset i =
        let relF = fmap (\x -> x - offset) . f
         in case i of
                J{k} -> J $ deref' relF k
                Addi{rd, rs1, k} -> Addi{rd, rs1, k = deref' f k}
                Add{rd, rs1, rs2} -> Add{rd, rs1, rs2}
                Sub{rd, rs1, rs2} -> Sub{rd, rs1, rs2}
                Mul{rd, rs1, rs2} -> Mul{rd, rs1, rs2}
                Mulh{rd, rs1, rs2} -> Mulh{rd, rs1, rs2}
                Sll{rd, rs1, rs2} -> Sll{rd, rs1, rs2}
                Srl{rd, rs1, rs2} -> Srl{rd, rs1, rs2}
                Sra{rd, rs1, rs2} -> Sra{rd, rs1, rs2}
                And{rd, rs1, rs2} -> And{rd, rs1, rs2}
                Or{rd, rs1, rs2} -> Or{rd, rs1, rs2}
                Xor{rd, rs1, rs2} -> Xor{rd, rs1, rs2}
                Mv{rd, rs} -> Mv{rd, rs}
                Sw{rs2, offsetRs1} -> Sw{rs2, offsetRs1}
                Lui{rd, k} -> Lui{rd, k = deref' f k}
                Li{rd, k} -> Li{rd, k = deref' f k}
                Lw{rd, offsetRs1} -> Lw{rd, offsetRs1}
                Beqz{rs1, k} -> Beqz rs1 $ deref' relF k
                Bnez{rs1, k} -> Bnez rs1 $ deref' relF k
                Bgt{rs1, rs2, k} -> Bgt rs1 rs2 $ deref' relF k
                Ble{rs1, rs2, k} -> Ble rs1 rs2 $ deref' relF k
                Bgtu{rs1, rs2, k} -> Bgtu rs1 rs2 $ deref' relF k
                Bleu{rs1, rs2, k} -> Bleu rs1 rs2 $ deref' relF k
                Halt -> Halt

instance ByteLength (Risc w l) where
    byteLength _ = 4

comma = hspace >> string "," >> hspace

cmd1args mnemonic constructor a =
    constructor <$> (string mnemonic *> hspace1 *> a)

cmd2args mnemonic constructor a b =
    constructor
        <$> (string mnemonic *> hspace1 *> a)
        <*> (comma *> b)

cmd3args mnemonic constructor a b c =
    constructor
        <$> (string mnemonic *> hspace1 *> a)
        <*> (comma *> b)
        <*> (comma *> c)

-- * Machine

data MachineState mem w = State
    { pc :: Int
    , mem :: mem
    , regs :: HashMap Register w
    , stopped :: Bool
    }
    deriving (Show)

setPc :: forall w. Int -> State (MachineState (IoMem (Risc w w) w) w) ()
setPc addr = modify $ \st -> st{pc = addr}

nextPc :: forall w. (ByteLength w, Default w) => State (MachineState (IoMem (Risc w w) w) w) ()
nextPc = do
    State{pc} <- get
    setPc (pc + byteLength (def :: w))

getReg r = do
    State{regs} <- get
    return $ fromMaybe (error "Wrong register ID") $ regs !? r

setReg r value = modify $ \st@State{regs} -> st{regs = insert r value regs}

getWord addr = do
    st@State{mem} <- get
    let (w, mem') = runState (readWord addr) mem
    put st{mem = mem'}
    return w

setWord addr w = do
    st@State{mem} <- get
    let mem' = execState (writeWord addr w) mem
    put st{mem = mem'}

instance (MachineWord w) => InitState (IoMem (Risc w w) w) (MachineState (IoMem (Risc w w) w) w) where
    initState pc dump = State{pc, mem = dump, regs = def, stopped = False}

instance StateInterspector (MachineState (IoMem (Risc w w) w) w) (Risc w w) w Register where
    registers State{regs} = regs
    memoryDump State{mem = IoMem{mIoCells}} = mIoCells
    ioStreams State{mem = IoMem{mIoStreams}} = mIoStreams

instance (MachineWord w) => Machine (MachineState (IoMem (Risc w w) w) w) (Risc w w) w where
    instructionFetch =
        get
            <&> ( \case
                    State{stopped = True} -> Nothing
                    State{pc, mem} -> do
                        let instruction = evalState (readInstruction pc) mem
                        Just (pc, instruction)
                )

    instructionStep = do
        (tmp :: Maybe (Int, Risc w w)) <- instructionFetch
        let (_pc, instruction) = fromMaybe (error "Can't fetch instruction.") tmp
        case instruction of
            Addi{rd, rs1, k} -> do
                rs1' <- getReg rs1
                setReg rd (rs1' + k)
                nextPc
            Add{rd, rs1, rs2} -> rOperation rs1 rs2 rd id id (+)
            Sub{rd, rs1, rs2} -> rOperation rs1 rs2 rd id id (-)
            Mul{rd, rs1, rs2} -> rOperation rs1 rs2 rd id id (*)
            Mulh{rd, rs1, rs2} ->
                rOperation
                    rs1
                    rs2
                    rd
                    fromIntegral
                    fromIntegral
                    ( \(r1 :: Integer) r2 ->
                        let x = r1 * r2
                            shift = 8 * byteLength (def :: w)
                         in fromIntegral (x `shiftR` shift)
                    )
            -- FIXME: check logic/arithmetical shift
            Sll{rd, rs1, rs2} -> rOperation rs1 rs2 rd id id (\r1 r2 -> r1 `shiftL` fromEnum r2)
            Srl{rd, rs1, rs2} -> rOperation rs1 rs2 rd id id (\r1 r2 -> r1 `shiftR` fromEnum r2)
            Sra{rd, rs1, rs2} -> rOperation rs1 rs2 rd id id (\r1 r2 -> r1 `shiftR` fromEnum r2)
            And{rd, rs1, rs2} -> rOperation rs1 rs2 rd id id (.&.)
            Or{rd, rs1, rs2} -> rOperation rs1 rs2 rd id id (.|.)
            Xor{rd, rs1, rs2} -> rOperation rs1 rs2 rd id id xor
            Mv{rd, rs} -> do
                rs' <- getReg rs
                setReg rd rs'
                nextPc
            Sw{rs2, offsetRs1 = MemRef{mrOffset, mrReg}} -> do
                rs2' <- getReg rs2
                rs1' <- getReg mrReg
                setWord (fromEnum (rs1' + mrOffset)) rs2'
                nextPc
            Lui{rd, k} -> do
                w <- getWord $ fromEnum k
                setReg rd (w `shiftR` 12)
                nextPc
            Li{rd, k} -> do
                setReg rd k
                nextPc
            Lw{rd, offsetRs1 = MemRef{mrOffset, mrReg}} -> do
                rs1' <- getReg mrReg
                w <- getWord $ fromEnum (mrOffset + rs1')
                setReg rd w
                nextPc
            J{k} -> do
                State{pc} <- get
                setPc (pc + fromEnum k)
            Beqz{rs1, k} -> do
                State{pc} <- get
                rs1' <- getReg rs1
                if rs1' == 0
                    then setPc (pc + fromEnum k)
                    else nextPc
            Bnez{rs1, k} -> do
                State{pc} <- get
                rs1' <- getReg rs1
                if rs1' /= 0
                    then setPc (pc + fromEnum k)
                    else nextPc
            Bgt{rs1, rs2, k} -> do
                State{pc} <- get
                rs1' <- fromSign <$> getReg rs1
                rs2' <- fromSign <$> getReg rs2
                if rs1' > rs2'
                    then setPc (pc + fromEnum k)
                    else nextPc
            Ble{rs1, rs2, k} -> do
                State{pc} <- get
                rs1' <- fromSign <$> getReg rs1
                rs2' <- fromSign <$> getReg rs2
                if rs1' <= rs2'
                    then setPc (pc + fromEnum k)
                    else nextPc
            Bgtu{rs1, rs2, k} -> do
                State{pc} <- get
                rs1' <- getReg rs1
                rs2' <- getReg rs2
                if rs1' > rs2'
                    then setPc (pc + fromEnum k)
                    else nextPc
            Bleu{rs1, rs2, k} -> do
                State{pc} <- get
                rs1' <- getReg rs1
                rs2' <- getReg rs2
                if rs1' <= rs2'
                    then setPc (pc + fromEnum k)
                    else nextPc
            Halt -> modify $ \st -> st{stopped = True}
        where
            rOperation rs1 rs2 rd f1 f2 fd = do
                r1 <- f1 <$> getReg rs1
                r2 <- f2 <$> getReg rs2
                setReg rd (fd r1 r2)
                nextPc
