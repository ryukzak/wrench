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
import System.Random (StdGen, mkStdGen, uniformR)
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
    deriving (Eq, Generic, Read, Show)

allRegisters =
    [ Zero
    , Ra
    , Sp
    , Gp
    , Tp
    , T0
    , T1
    , T2
    , S0Fp
    , S1
    , A0
    , A1
    , A2
    , A3
    , A4
    , A5
    , A6
    , A7
    , S2
    , S3
    , S4
    , S5
    , S6
    , S7
    , S8
    , S9
    , S10
    , S11
    , T3
    , T4
    , T5
    , T6
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
    , alu1Op :: AluOp w l
    , alu2Op :: AluOp w l
    , ctrlOp :: ControlOp w l
    }
    deriving (Show)

-- * Parser Helpers

register :: Parser Register
register =
    choice
        [ string "zero" >> return Zero
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
        , string "ra" >> return Ra
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
        , string "sp" >> return Sp
        , string "t5" >> return T5
        , string "t6" >> return T6
        , string "gp" >> return Gp
        , string "tp" >> return Tp
        , string "t0" >> return T0
        , string "t1" >> return T1
        , string "t2" >> return T2
        , string "s0fp" >> return S0Fp
        , string "s1" >> return S1
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
        alu1Op <- parseAluOp
        hspace >> string "|" >> hspace
        alu2Op <- parseAluOp
        hspace >> string "|" >> hspace
        ctrlOp <- parseCtrlOp
        eol' (commentStart @(Isa _ _))
        return Isa{memOp, alu1Op, alu2Op, ctrlOp}

instance DerefMnemonic (MemoryOp w) w where
    derefMnemonic _ _ NopM = NopM
    derefMnemonic _ _ (Lw lwRd lwOffsetRs1) = Lw lwRd lwOffsetRs1
    derefMnemonic _ _ (Sw swRs2 swOffsetRs1) = Sw swRs2 swOffsetRs1
    derefMnemonic _ _ (Sb sbRs2 sbOffsetRs1) = Sb sbRs2 sbOffsetRs1

instance DerefMnemonic (AluOp w) w where
    derefMnemonic f _ (Addi addiRd addiRs1 addiK) = Addi addiRd addiRs1 $ deref' f addiK
    derefMnemonic f _ (Slti sltiRd sltiRs1 sltiK) = Slti sltiRd sltiRs1 $ deref' f sltiK
    derefMnemonic f _ (Lui luiRd luiK) = Lui luiRd $ deref' f luiK
    derefMnemonic _ _ (Add addRd addRs1 addRs2) = Add addRd addRs1 addRs2
    derefMnemonic _ _ (Sub subRd subRs1 subRs2) = Sub subRd subRs1 subRs2
    derefMnemonic _ _ (Mul mulRd mulRs1 mulRs2) = Mul mulRd mulRs1 mulRs2
    derefMnemonic _ _ (Mulh mulhRd mulhRs1 mulhRs2) = Mulh mulhRd mulhRs1 mulhRs2
    derefMnemonic _ _ (Div divRd divRs1 divRs2) = Div divRd divRs1 divRs2
    derefMnemonic _ _ (Rem remRd remRs1 remRs2) = Rem remRd remRs1 remRs2
    derefMnemonic _ _ (Sll sllRd sllRs1 sllRs2) = Sll sllRd sllRs1 sllRs2
    derefMnemonic _ _ (Srl srlRd srlRs1 srlRs2) = Srl srlRd srlRs1 srlRs2
    derefMnemonic _ _ (Sra sraRd sraRs1 sraRs2) = Sra sraRd sraRs1 sraRs2
    derefMnemonic _ _ (And andRd andRs1 andRs2) = And andRd andRs1 andRs2
    derefMnemonic _ _ (Or orRd orRs1 orRs2) = Or orRd orRs1 orRs2
    derefMnemonic _ _ (Xor xorRd xorRs1 xorRs2) = Xor xorRd xorRs1 xorRs2
    derefMnemonic _ _ (Mv mvRd mvRs) = Mv mvRd mvRs
    derefMnemonic _ _ NopA = NopA

instance (MachineWord w) => DerefMnemonic (ControlOp w) w where
    derefMnemonic f offset (J jK) = J $ deref' (fmap (\x -> x - offset) . f) jK
    derefMnemonic f offset (Jal jalRd jalK) = Jal jalRd $ deref' (fmap (\x -> x - offset) . f) jalK
    derefMnemonic _ _ (Jr jrRs) = Jr jrRs
    derefMnemonic f offset (Beqz beqzRs1 beqzK) = Beqz beqzRs1 $ deref' (fmap (\x -> x - offset) . f) beqzK
    derefMnemonic f offset (Bnez bnezRs1 bnezK) = Bnez bnezRs1 $ deref' (fmap (\x -> x - offset) . f) bnezK
    derefMnemonic f offset (Bgt bgtRs1 bgtRs2 bgtK) = Bgt bgtRs1 bgtRs2 $ deref' (fmap (\x -> x - offset) . f) bgtK
    derefMnemonic f offset (Ble bleRs1 bleRs2 bleK) = Ble bleRs1 bleRs2 $ deref' (fmap (\x -> x - offset) . f) bleK
    derefMnemonic f offset (Bgtu bgtuRs1 bgtuRs2 bgtuK) = Bgtu bgtuRs1 bgtuRs2 $ deref' (fmap (\x -> x - offset) . f) bgtuK
    derefMnemonic f offset (Bleu bleuRs1 bleuRs2 bleuK) = Bleu bleuRs1 bleuRs2 $ deref' (fmap (\x -> x - offset) . f) bleuK
    derefMnemonic f offset (Beq beqRs1 beqRs2 beqK) = Beq beqRs1 beqRs2 $ deref' (fmap (\x -> x - offset) . f) beqK
    derefMnemonic f offset (Bne bneRs1 bneRs2 bneK) = Bne bneRs1 bneRs2 $ deref' (fmap (\x -> x - offset) . f) bneK
    derefMnemonic f offset (Blt bltRs1 bltRs2 bltK) = Blt bltRs1 bltRs2 $ deref' (fmap (\x -> x - offset) . f) bltK
    derefMnemonic _ _ Halt = Halt
    derefMnemonic _ _ NopC = NopC

instance (MachineWord w) => DerefMnemonic (Isa w) w where
    derefMnemonic f offset i@Isa{memOp, alu1Op, alu2Op, ctrlOp} =
        i
            { memOp = derefMnemonic f offset memOp
            , alu1Op = derefMnemonic f offset alu1Op
            , alu2Op = derefMnemonic f offset alu2Op
            , ctrlOp = derefMnemonic f offset ctrlOp
            }

instance ByteSize (Isa w l) where
    byteSize _ = 11

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
    , randoms :: [Int]
    }
    deriving (Show)

getRandoms :: forall w. Int -> State (MachineState (IoMem (Isa w w) w) w) [Int]
getRandoms n = do
    State{randoms} <- get
    let (taken, rest) = splitAt n randoms
    modify $ \st -> st{randoms = rest}
    return taken

randomInts :: (Int, Int) -> StdGen -> [Int]
randomInts range gen = 
    let (val, gen') = uniformR range gen
    in val : randomInts range gen'

setPc :: forall w. Int -> State (MachineState (IoMem (Isa w w) w) w) ()
setPc addr = modify $ \st -> st{pc = addr}

nextPc :: forall w. State (MachineState (IoMem (Isa w w) w) w) ()
nextPc = do
    State{pc} <- get
    setPc (pc + 11) -- Bundle size 11 bytes

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
    initState pc dump@IoMem{mSeed} =
        State
            { pc
            , mem = dump
            , regs = def
            , stopped = False
            , internalError = Nothing
            , randoms = randomInts (0, maxBound) (mkStdGen mSeed)
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

    instructionExecute _pc Isa{memOp, alu1Op, alu2Op, ctrlOp} = do
        -- Phase 1: Read all source operands and compute results (without modifying state)
        memResult <- computeMem memOp
        alu1OpResult <- computeAlu alu1Op
        alu2OpResult <- computeAlu alu2Op

        -- Phase 2: Apply all register writes simultaneously in random order
        let results = [applyMemResult memResult, applyAluResult alu1OpResult, applyAluResult alu2OpResult]
        shuffledResults <- shuffleList results
        forM_ shuffledResults id

        -- Phase 3: Execute control operation (always last, may branch)
        branched <- execCtrl ctrlOp

        -- If no branch taken, advance PC
        unless branched nextPc

        where
            shuffleList :: [a] -> State (MachineState (IoMem (Isa w w) w) w) [a]
            shuffleList [] = return []
            shuffleList [x] = return [x]
            shuffleList xs = do
                indices <- getRandoms (length xs)
                return $ shuffle xs (map (`mod` length xs) indices)
            
            shuffle :: [a] -> [Int] -> [a]
            shuffle [] _ = []
            shuffle [x] _ = [x]
            shuffle xs (idx:rest) = 
                case splitAt idx xs of
                    (before, item : after) -> item : shuffle (before ++ after) rest
                    _ -> xs
            shuffle xs [] = xs

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
                setReg jalRd (toEnum pc + 11)
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
