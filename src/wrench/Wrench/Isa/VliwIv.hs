{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Inspired by VLIW architectures and RISC-V
module Wrench.Isa.VliwIv (
    Isa (..),
    MachineState (..),
    VliwIvState,
    Register (..),
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Default
import Data.Text qualified as T
import Relude
import Relude.Extra
import Relude.Unsafe qualified as Unsafe
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)
import Text.Megaparsec (choice)
import Text.Megaparsec.Char (char, hspace, string)
import Wrench.Machine.Memory
import Wrench.Machine.Types (
    ByteSizeT (..),
    InitState (..),
    IoMem (..),
    Machine (..),
    StateInterspector (..),
    fromSign,
    halted,
    lShiftR,
    signBitAnd,
 )
import Wrench.Report
import Wrench.Translator.Parser.Misc (eol', hexNum, num, reference, referenceWithDirective)
import Wrench.Translator.Parser.Types
import Wrench.Translator.Types

-- * Registers

data Register
    = X0 -- Zero
    | X1 -- Ra
    | X2 -- Sp
    | X3 -- Gp
    | X4 -- Tp
    | X5 -- T0
    | X6 -- T1
    | X7 -- T2
    | X8 -- S0Fp
    | X9 -- S1
    | X10 -- A0
    | X11 -- A1
    | X12 -- A2
    | X13 -- A3
    | X14 -- A4
    | X15 -- A5
    | X16 -- A6
    | X17 -- A7
    | X18 -- S2
    | X19 -- S3
    | X20 -- S4
    | X21 -- S5
    | X22 -- S6
    | X23 -- S7
    | X24 -- S8
    | X25 -- S9
    | X26 -- S10
    | X27 -- S11
    | X28 -- T3
    | X29 -- T4
    | X30 -- T5
    | X31 -- T6
    deriving (Eq, Generic, Read, Show)

allRegisters =
    [ X0
    , X1
    , X2
    , X3
    , X4
    , X5
    , X6
    , X7
    , X8
    , X9
    , X10
    , X11
    , X12
    , X13
    , X14
    , X15
    , X16
    , X17
    , X18
    , X19
    , X20
    , X21
    , X22
    , X23
    , X24
    , X25
    , X26
    , X27
    , X28
    , X29
    , X30
    , X31
    ]

instance Hashable Register

instance (Default w) => Default (HashMap Register w) where
    def = fromList $ map (,def) allRegisters

-- * Slot Operations

data MemoryOp w l
    = Lw {lwRd :: Register, lwOffsetRs1 :: MemRef w}
    | Sw {swRs2 :: Register, swOffsetRs1 :: MemRef w}
    | Sb {sbRs2 :: Register, sbOffsetRs1 :: MemRef w}
    | NopM
    deriving (Show)

data AluOp w l
    = Addi {addiRd, addiRs1 :: Register, addiK :: l}
    | Add {addRd, addRs1, addRs2 :: Register}
    | Sub {subRd, subRs1, subRs2 :: Register}
    | Mul {mulRd, mulRs1, mulRs2 :: Register}
    | Mulh {mulhRd, mulhRs1, mulhRs2 :: Register}
    | Div {divRd, divRs1, divRs2 :: Register}
    | Rem {remRd, remRs1, remRs2 :: Register}
    | Sll {sllRd, sllRs1, sllRs2 :: Register}
    | Srl {srlRd, srlRs1, srlRs2 :: Register}
    | Sra {sraRd, sraRs1, sraRs2 :: Register}
    | And {andRd, andRs1, andRs2 :: Register}
    | Or {orRd, orRs1, orRs2 :: Register}
    | Xor {xorRd, xorRs1, xorRs2 :: Register}
    | Slti {sltiRd, sltiRs1 :: Register, sltiK :: l}
    | Lui {luiRd :: Register, luiK :: l}
    | Mv {mvRd, mvRs :: Register}
    | NopA
    deriving (Show)

data ControlOp w l
    = J {jK :: l}
    | Jal {jalRd :: Register, jalK :: l}
    | Jr {jrRs :: Register}
    | Beqz {beqzRs1 :: Register, beqzK :: l}
    | Bnez {bnezRs1 :: Register, bnezK :: l}
    | Bgt {bgtRs1, bgtRs2 :: Register, bgtK :: l}
    | Ble {bleRs1, bleRs2 :: Register, bleK :: l}
    | Bgtu {bgtuRs1, bgtuRs2 :: Register, bgtuK :: l}
    | Bleu {bleuRs1, bleuRs2 :: Register, bleuK :: l}
    | Beq {beqRs1, beqRs2 :: Register, beqK :: l}
    | Bne {bneRs1, bneRs2 :: Register, bneK :: l}
    | Blt {bltRs1, bltRs2 :: Register, bltK :: l}
    | Halt
    | NopC
    deriving (Show)

-- * ISA Bundle

data Isa w l = Isa
    { memOp :: MemoryOp w l
    , alu1 :: AluOp w l
    , alu2 :: AluOp w l
    , ctrlOp :: ControlOp w l
    }
    deriving (Show)

-- * Parser Helpers

register :: Parser Register
register =
    choice
        [ string "x0" >> return X0
        , string "x10" >> return X10
        , string "x11" >> return X11
        , string "x12" >> return X12
        , string "x13" >> return X13
        , string "x14" >> return X14
        , string "x15" >> return X15
        , string "x16" >> return X16
        , string "x17" >> return X17
        , string "x18" >> return X18
        , string "x19" >> return X19
        , string "x1" >> return X1
        , string "x20" >> return X20
        , string "x21" >> return X21
        , string "x22" >> return X22
        , string "x23" >> return X23
        , string "x24" >> return X24
        , string "x25" >> return X25
        , string "x26" >> return X26
        , string "x27" >> return X27
        , string "x28" >> return X28
        , string "x29" >> return X29
        , string "x2" >> return X2
        , string "x30" >> return X30
        , string "x31" >> return X31
        , string "x3" >> return X3
        , string "x4" >> return X4
        , string "x5" >> return X5
        , string "x6" >> return X6
        , string "x7" >> return X7
        , string "x8" >> return X8
        , string "x9" >> return X9
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

instance CommentStart (Isa _a _b) where
    commentStart = ";"

parseMemOp :: (MachineWord w) => Parser (MemoryOp w (Ref w))
parseMemOp =
    choice
        [ cmd2args "lw" Lw register memRef
        , cmd2args "sw" Sw register memRef
        , cmd2args "sb" Sb register memRef
        , string "nop" >> return NopM
        ]

parseAluOp :: (MachineWord w) => Parser (AluOp w (Ref w))
parseAluOp =
    choice
        [ cmd3args "addi" Addi register register referenceWithDirective
        , cmd3args "add" Add register register register
        , cmd3args "sub" Sub register register register
        , cmd3args "mul" Mul register register register
        , cmd3args "mulh" Mulh register register register
        , cmd3args "div" Div register register register
        , cmd3args "rem" Rem register register register
        , cmd3args "sll" Sll register register register
        , cmd3args "srl" Srl register register register
        , cmd3args "sra" Sra register register register
        , cmd3args "and" And register register register
        , cmd3args "or" Or register register register
        , cmd3args "xor" Xor register register register
        , cmd3args "slti" Slti register register referenceWithDirective
        , cmd2args "lui" Lui register referenceWithDirective
        , cmd2args "mv" Mv register register
        , string "nop" >> return NopA
        ]

parseCtrlOp :: (MachineWord w) => Parser (ControlOp w (Ref w))
parseCtrlOp =
    choice
        [ cmd1args "j" J reference
        , cmd2args "jal" Jal register reference
        , cmd1args "jr" Jr register
        , cmd2args "beqz" Beqz register reference
        , cmd2args "bnez" Bnez register reference
        , cmd3args "bgt" Bgt register register reference
        , cmd3args "ble" Ble register register reference
        , cmd3args "bgtu" Bgtu register register reference
        , cmd3args "bleu" Bleu register register reference
        , cmd3args "beq" Beq register register reference
        , cmd3args "bne" Bne register register reference
        , cmd3args "blt" Blt register register reference
        , string "halt" >> return Halt
        , string "nop" >> return NopC
        ]

instance (MachineWord w) => MnemonicParser (Isa w (Ref w)) where
    mnemonic = do
        hspace
        memOp <- parseMemOp
        hspace >> string "|" >> hspace
        alu1 <- parseAluOp
        hspace >> string "|" >> hspace
        alu2 <- parseAluOp
        hspace >> string "|" >> hspace
        ctrlOp <- parseCtrlOp
        eol' (commentStart @(Isa _ _))
        return Isa{memOp, alu1, alu2, ctrlOp}

instance (MachineWord w) => DerefMnemonic (Isa w) w where
    derefMnemonic f offset i@Isa{memOp, alu1, alu2, ctrlOp} =
        i
            { memOp = derefMem f offset memOp
            , alu1 = derefAlu f offset alu1
            , alu2 = derefAlu f offset alu2
            , ctrlOp = derefCtrl f offset ctrlOp
            }
        where
            relF = fmap (\x -> x - offset) . f
            derefMem _ _ NopM = NopM
            derefMem _ _ (Lw lwRd lwOffsetRs1) = Lw lwRd lwOffsetRs1
            derefMem _ _ (Sw swRs2 swOffsetRs1) = Sw swRs2 swOffsetRs1
            derefMem _ _ (Sb sbRs2 sbOffsetRs1) = Sb sbRs2 sbOffsetRs1
            derefAlu f _ (Addi addiRd addiRs1 addiK) = Addi addiRd addiRs1 $ deref' f addiK
            derefAlu f _ (Slti sltiRd sltiRs1 sltiK) = Slti sltiRd sltiRs1 $ deref' f sltiK
            derefAlu f _ (Lui luiRd luiK) = Lui luiRd $ deref' f luiK
            derefAlu _ _ (Add addRd addRs1 addRs2) = Add addRd addRs1 addRs2
            derefAlu _ _ (Sub subRd subRs1 subRs2) = Sub subRd subRs1 subRs2
            derefAlu _ _ (Mul mulRd mulRs1 mulRs2) = Mul mulRd mulRs1 mulRs2
            derefAlu _ _ (Mulh mulhRd mulhRs1 mulhRs2) = Mulh mulhRd mulhRs1 mulhRs2
            derefAlu _ _ (Div divRd divRs1 divRs2) = Div divRd divRs1 divRs2
            derefAlu _ _ (Rem remRd remRs1 remRs2) = Rem remRd remRs1 remRs2
            derefAlu _ _ (Sll sllRd sllRs1 sllRs2) = Sll sllRd sllRs1 sllRs2
            derefAlu _ _ (Srl srlRd srlRs1 srlRs2) = Srl srlRd srlRs1 srlRs2
            derefAlu _ _ (Sra sraRd sraRs1 sraRs2) = Sra sraRd sraRs1 sraRs2
            derefAlu _ _ (And andRd andRs1 andRs2) = And andRd andRs1 andRs2
            derefAlu _ _ (Or orRd orRs1 orRs2) = Or orRd orRs1 orRs2
            derefAlu _ _ (Xor xorRd xorRs1 xorRs2) = Xor xorRd xorRs1 xorRs2
            derefAlu _ _ (Mv mvRd mvRs) = Mv mvRd mvRs
            derefAlu _ _ NopA = NopA
            derefCtrl _ _ (J jK) = J $ deref' relF jK
            derefCtrl _ _ (Jal jalRd jalK) = Jal jalRd $ deref' relF jalK
            derefCtrl _ _ (Jr jrRs) = Jr jrRs
            derefCtrl _ _ (Beqz beqzRs1 beqzK) = Beqz beqzRs1 $ deref' relF beqzK
            derefCtrl _ _ (Bnez bnezRs1 bnezK) = Bnez bnezRs1 $ deref' relF bnezK
            derefCtrl _ _ (Bgt bgtRs1 bgtRs2 bgtK) = Bgt bgtRs1 bgtRs2 $ deref' relF bgtK
            derefCtrl _ _ (Ble bleRs1 bleRs2 bleK) = Ble bleRs1 bleRs2 $ deref' relF bleK
            derefCtrl _ _ (Bgtu bgtuRs1 bgtuRs2 bgtuK) = Bgtu bgtuRs1 bgtuRs2 $ deref' relF bgtuK
            derefCtrl _ _ (Bleu bleuRs1 bleuRs2 bleuK) = Bleu bleuRs1 bleuRs2 $ deref' relF bleuK
            derefCtrl _ _ (Beq beqRs1 beqRs2 beqK) = Beq beqRs1 beqRs2 $ deref' relF beqK
            derefCtrl _ _ (Bne bneRs1 bneRs2 bneK) = Bne bneRs1 bneRs2 $ deref' relF bneK
            derefCtrl _ _ (Blt bltRs1 bltRs2 bltK) = Blt bltRs1 bltRs2 $ deref' relF bltK
            derefCtrl _ _ Halt = Halt
            derefCtrl _ _ NopC = NopC

instance ByteSize (Isa w l) where
    byteSize _ = 16

comma = hspace >> string "," >> hspace

cmdMnemonic mnemonic = string (mnemonic <> " ") <|> string (mnemonic <> "\t")

cmd1args mnemonic constructor a =
    constructor <$> (cmdMnemonic mnemonic *> hspace *> a)

cmd2args mnemonic constructor a b =
    constructor
        <$> (cmdMnemonic mnemonic *> hspace *> a)
        <*> (comma *> b)

cmd3args mnemonic constructor a b c =
    constructor
        <$> (cmdMnemonic mnemonic *> hspace *> a)
        <*> (comma *> b)
        <*> (comma *> c)

-- * Machine

type VliwIvState w = MachineState (IoMem (Isa w w) w) w

data MachineState mem w = State
    { pc :: Int
    , mem :: mem
    , regs :: HashMap Register w
    , stopped :: Bool
    , internalError :: Maybe Text
    }
    deriving (Show)

setPc :: forall w. Int -> State (MachineState (IoMem (Isa w w) w) w) ()
setPc addr = modify $ \st -> st{pc = addr}

nextPc :: forall w. State (MachineState (IoMem (Isa w w) w) w) ()
nextPc = do
    State{pc} <- get
    setPc (pc + 16) -- Bundle size 16 bytes

raiseInternalError :: Text -> State (MachineState (IoMem (Isa w w) w) w) ()
raiseInternalError msg = modify $ \st -> st{internalError = Just msg}

getReg r = do
    State{regs} <- get
    case regs !? r of
        Just value -> return value
        Nothing -> do
            raiseInternalError $ "wrong register: " <> show r
            return def

setReg r value = modify $ \st@State{regs} -> st{regs = insert r value regs}

getWord addr = do
    st@State{mem} <- get
    case readWord mem addr of
        Right (mem', w) -> do
            put st{mem = mem'}
            return w
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return def

setWord addr w = do
    st@State{mem} <- get
    case writeWord mem addr w of
        Right mem' -> do
            put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err

setByte addr byte = do
    st@State{mem} <- get
    case writeByte mem addr byte of
        Right mem' -> do
            put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err

instance (MachineWord w) => InitState (IoMem (Isa w w) w) (MachineState (IoMem (Isa w w) w) w) where
    initState pc dump =
        State
            { pc
            , mem = dump
            , regs = def
            , stopped = False
            , internalError = Nothing
            }

instance (MachineWord w) => StateInterspector (MachineState (IoMem (Isa w w) w) w) (IoMem (Isa w w) w) (Isa w w) w where
    programCounter State{pc} = pc
    memoryDump State{mem} = mem
    ioStreams State{mem = IoMem{mIoStreams}} = mIoStreams
    reprState labels st v
        | Just v' <- defaultView labels st v = v'
    reprState labels st@State{regs} v =
        case T.splitOn ":" v of
            [r] -> reprState labels st (r <> ":dec")
            [r, f]
                | Just r' <- readMaybe (toString r)
                , Just r'' <- regs !? r' ->
                    viewRegister f r''
            _ -> errorView v

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

    instructionExecute _pc Isa{memOp, alu1, alu2, ctrlOp} = do
        -- Phase 1: Read all source operands and compute results (without modifying state)
        memResult <- computeMem memOp
        alu1Result <- computeAlu alu1
        alu2Result <- computeAlu alu2

        -- Phase 2: Apply all register writes simultaneously in random order
        let results = [applyMemResult memResult, applyAluResult alu1Result, applyAluResult alu2Result]
        let shuffledResults = unsafePerformIO $ shuffleList results
        forM_ shuffledResults id

        -- Phase 3: Execute control operation (always last, may branch)
        branched <- execCtrl ctrlOp

        -- If no branch taken, advance PC
        unless branched nextPc
        where
            shuffleList :: [a] -> IO [a]
            shuffleList [] = return []
            shuffleList [x] = return [x]
            shuffleList xs = do
                idx <- randomRIO (0, length xs - 1)
                let (before, item : after) = splitAt idx xs
                rest <- shuffleList (before ++ after)
                return (item : rest)
            -- Compute memory operation result without applying it
            computeMem :: MemoryOp w w -> State (MachineState (IoMem (Isa w w) w) w) (Maybe (Register, w))
            computeMem NopM = return Nothing
            computeMem (Lw lwRd (MemRef mrOffset mrReg)) = do
                rs1' <- getReg mrReg
                w <- getWord $ fromEnum (mrOffset + rs1')
                return $ Just (lwRd, w)
            computeMem (Sw swRs2 (MemRef mrOffset mrReg)) = do
                rs2' <- getReg swRs2
                mrReg' <- getReg mrReg
                setWord (fromEnum (mrReg' + mrOffset)) rs2'
                return Nothing
            computeMem (Sb sbRs2 (MemRef mrOffset mrReg)) = do
                rs2' <- getReg sbRs2
                mrReg' <- getReg mrReg
                setByte (fromEnum (mrReg' + mrOffset)) $ fromIntegral rs2'
                return Nothing

            -- Apply memory operation result
            applyMemResult :: Maybe (Register, w) -> State (MachineState (IoMem (Isa w w) w) w) ()
            applyMemResult Nothing = return ()
            applyMemResult (Just (reg, val)) = setReg reg val

            -- Compute ALU operation result without applying it
            computeAlu :: AluOp w w -> State (MachineState (IoMem (Isa w w) w) w) (Maybe (Register, w))
            computeAlu NopA = return Nothing
            computeAlu (Addi addiRd addiRs1 addiK) = do
                rs1' <- getReg addiRs1
                return $ Just (addiRd, rs1' + (addiK `signBitAnd` 0x00000FFF))
            computeAlu (Add addRd addRs1 addRs2) = aluOpCompute addRd addRs1 addRs2 id id (+)
            computeAlu (Sub subRd subRs1 subRs2) = aluOpCompute subRd subRs1 subRs2 id id (-)
            computeAlu (Mul mulRd mulRs1 mulRs2) = aluOpCompute mulRd mulRs1 mulRs2 id id (*)
            computeAlu (Mulh mulhRd mulhRs1 mulhRs2) =
                aluOpCompute
                    mulhRd
                    mulhRs1
                    mulhRs2
                    fromIntegral
                    fromIntegral
                    ( \(r1 :: Integer) r2 ->
                        let x = r1 * r2
                            shift = 8 * byteSizeT @w
                         in fromIntegral (x `shiftR` shift)
                    )
            computeAlu (Div divRd divRs1 divRs2) = aluOpCompute divRd divRs1 divRs2 id id div
            computeAlu (Rem remRd remRs1 remRs2) = aluOpCompute remRd remRs1 remRs2 id id rem
            computeAlu (Sll sllRd sllRs1 sllRs2) = aluOpCompute sllRd sllRs1 sllRs2 id id (\r1 r2 -> r1 `shiftL` fromEnum r2)
            computeAlu (Srl srlRd srlRs1 srlRs2) = aluOpCompute srlRd srlRs1 srlRs2 id id lShiftR
            computeAlu (Sra sraRd sraRs1 sraRs2) = aluOpCompute sraRd sraRs1 sraRs2 id id (\r1 r2 -> r1 `shiftR` fromEnum r2)
            computeAlu (And andRd andRs1 andRs2) = aluOpCompute andRd andRs1 andRs2 id id (.&.)
            computeAlu (Or orRd orRs1 orRs2) = aluOpCompute orRd orRs1 orRs2 id id (.|.)
            computeAlu (Xor xorRd xorRs1 xorRs2) = aluOpCompute xorRd xorRs1 xorRs2 id id xor
            computeAlu (Slti sltiRd sltiRs1 sltiK) = do
                rs1' <- getReg sltiRs1
                return $ Just (sltiRd, if rs1' < sltiK then 1 else 0)
            computeAlu (Lui luiRd luiK) = return $ Just (luiRd, (luiK .&. 0x000FFFFF) `shiftL` 12)
            computeAlu (Mv mvRd mvRs) = do
                val <- getReg mvRs
                return $ Just (mvRd, val)

            -- Apply ALU operation result
            applyAluResult :: Maybe (Register, w) -> State (MachineState (IoMem (Isa w w) w) w) ()
            applyAluResult Nothing = return ()
            applyAluResult (Just (reg, val)) = setReg reg val

            -- Helper for ALU operations with two source registers
            aluOpCompute rd rs1 rs2 f1 f2 fd = do
                r1 <- f1 <$> getReg rs1
                r2 <- f2 <$> getReg rs2
                return $ Just (rd, fd r1 r2)

            execCtrl NopC = return False
            execCtrl (J jK) = do
                State{pc} <- get
                setPc (pc + fromEnum jK)
                return True
            execCtrl (Jal jalRd jalK) = do
                State{pc} <- get
                setReg jalRd (toEnum pc + 16)
                setPc (pc + fromEnum jalK)
                return True
            execCtrl (Jr jrRs) = do
                rs' <- getReg jrRs
                setPc (fromEnum rs')
                return True
            execCtrl (Beqz beqzRs1 beqzK) = branchIf beqzRs1 beqzK (== 0)
            execCtrl (Bnez bnezRs1 bnezK) = branchIf bnezRs1 bnezK (/= 0)
            execCtrl (Bgt bgtRs1 bgtRs2 bgtK) = branchIf2 bgtRs1 bgtRs2 bgtK (>)
            execCtrl (Ble bleRs1 bleRs2 bleK) = branchIf2 bleRs1 bleRs2 bleK (<=)
            execCtrl (Bgtu bgtuRs1 bgtuRs2 bgtuK) = branchIf2u bgtuRs1 bgtuRs2 bgtuK (>)
            execCtrl (Bleu bleuRs1 bleuRs2 bleuK) = branchIf2u bleuRs1 bleuRs2 bleuK (<=)
            execCtrl (Beq beqRs1 beqRs2 beqK) = branchIf2 beqRs1 beqRs2 beqK (==)
            execCtrl (Bne bneRs1 bneRs2 bneK) = branchIf2 bneRs1 bneRs2 bneK (/=)
            execCtrl (Blt bltRs1 bltRs2 bltK) = branchIf2 bltRs1 bltRs2 bltK (<)
            execCtrl Halt = do
                modify $ \st -> st{stopped = True}
                return True

            branchIf rs1 k cond = do
                State{pc} <- get
                rs1' <- getReg rs1
                if cond rs1'
                    then do
                        setPc (pc + fromEnum k)
                        return True
                    else return False

            branchIf2 rs1 rs2 k cond = do
                State{pc} <- get
                rs1' <- getReg rs1
                rs2' <- getReg rs2
                if cond rs1' rs2'
                    then do
                        setPc (pc + fromEnum k)
                        return True
                    else return False

            branchIf2u rs1 rs2 k cond = do
                State{pc} <- get
                rs1' <- fromSign <$> getReg rs1
                rs2' <- fromSign <$> getReg rs2
                if cond rs1' rs2'
                    then do
                        setPc (pc + fromEnum k)
                        return True
                    else return False
