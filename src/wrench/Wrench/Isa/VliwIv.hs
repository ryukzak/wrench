{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Inspired by VLIW architectures and RISC-V
module Wrench.Isa.VliwIv (
    VliwIvIsa (..),
    VliwIvSt (..),
    Register (..),
    VliwLoadAcc,
    emptyVliwLoad,
) where

import Data.Bits (bit, shiftL, shiftR, (.&.), (.|.))
import Data.Default
import Data.Text qualified as T
import Relude
import Relude.Extra
import Text.Megaparsec (choice, getOffset, setOffset)
import Text.Megaparsec.Char (char, hspace, string)
import Wrench.Machine.Memory
import Wrench.Machine.Types (
    ByteSizeT (..),
    InitState (..),
    Inspectable (..),
    IoMem (..),
    Machine (..),
    MemOf,
    StateEnv (..),
    fromSign,
    halted,
 )
import Wrench.Machine.Word (fitSigned, i10, i15, i17, i20, lShiftR, u20)
import Wrench.Report
import Wrench.Report.View (Stat (..), withRenames)
import Wrench.Translator.Parser.Misc (comma, eol', reference, referenceWithDirective, wordLiteral)
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
    | Lb {lbRd :: Register, lbOffsetRs1 :: MemRef w}
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
    | Jalr {jalrRd, jalrRs1 :: Register, jalrK :: l}
    | Beqz {beqzRs1 :: Register, beqzK :: l}
    | Bnez {bnezRs1 :: Register, bnezK :: l}
    | Blt {bltRs1, bltRs2 :: Register, bltK :: l}
    | Bge {bgeRs1, bgeRs2 :: Register, bgeK :: l}
    | Bltu {bltuRs1, bltuRs2 :: Register, bltuK :: l}
    | Bgeu {bgeuRs1, bgeuRs2 :: Register, bgeuK :: l}
    | Beq {beqRs1, beqRs2 :: Register, beqK :: l}
    | Bne {bneRs1, bneRs2 :: Register, bneK :: l}
    | Halt
    | NopC
    deriving (Show)

-- * ISA Bundle

data VliwIvIsa w l = VliwIvIsa
    { memOp :: MemoryOp w l
    , alu1Op :: AluOp w l
    , alu2Op :: AluOp w l
    , ctrlOp :: ControlOp w l
    }
    deriving (Show)

-- * Load-level accumulator

-- | Per-run histogram of how many slots were active in each executed bundle.
--   @vlByLoad@ maps active-slot count (0..4) to the number of bundles with
--   that load. Used by the @vliw:*@ report variables to summarise how well
--   the program exploits the four-wide pipeline.
newtype VliwLoadAcc = VliwLoadAcc {vlByLoad :: IntMap Int}
    deriving (Eq, Show)

emptyVliwLoad :: VliwLoadAcc
emptyVliwLoad = VliwLoadAcc mempty

isMemActive :: MemoryOp w l -> Bool
isMemActive NopM = False
isMemActive _ = True

isAluActive :: AluOp w l -> Bool
isAluActive NopA = False
isAluActive _ = True

isCtrlActive :: ControlOp w l -> Bool
isCtrlActive NopC = False
isCtrlActive _ = True

bundleActiveCount :: VliwIvIsa w l -> Int
bundleActiveCount VliwIvIsa{memOp, alu1Op, alu2Op, ctrlOp} =
    bool 0 1 (isMemActive memOp)
        + bool 0 1 (isAluActive alu1Op)
        + bool 0 1 (isAluActive alu2Op)
        + bool 0 1 (isCtrlActive ctrlOp)

recordBundle :: VliwIvIsa w l -> VliwLoadAcc -> VliwLoadAcc
recordBundle isa (VliwLoadAcc m) =
    VliwLoadAcc $ alter (Just . maybe 1 (+ 1)) (bundleActiveCount isa) m

vliwLoadPercent :: VliwLoadAcc -> Int
vliwLoadPercent (VliwLoadAcc m) =
    let total = sum m
        active = sum [k * n | (k, n) <- toPairs m]
     in if total == 0 then 0 else (active * 100) `div` (total * 4)

-- | Average number of active slots (processing units) used per executed
--   bundle, rendered with two decimal places. An empty histogram renders as
--   @"0.00"@.
vliwAvgLoad :: VliwLoadAcc -> Text
vliwAvgLoad (VliwLoadAcc m) =
    let total = sum m
        active = sum [k * n | (k, n) <- toPairs m]
     in if total == 0
            then "0.00"
            else
                let scaled = (active * 100) `div` total
                    whole = scaled `div` 100
                    frac = scaled `mod` 100
                 in show whole <> "." <> (if frac < 10 then "0" else "") <> show frac

-- | Render the per-load histogram as @"K:N (P%)"@ pairs separated by commas,
--   one per non-empty bucket. @K@ is the active-slot count, @N@ is the
--   number of bundles in that bucket, @P@ is its percent share of executed
--   bundles. Empty buckets are omitted; an empty histogram renders as @"-"@.
renderBundlesByLoad :: VliwLoadAcc -> Text
renderBundlesByLoad (VliwLoadAcc m) =
    let nonZero = [(k, n) | k <- [0 .. 4 :: Int], let n = lookupDefault 0 k m, n > 0]
        total = sum (map snd nonZero)
        pct n = if total == 0 then 0 else (n * 100) `div` total
        fmt (k, n) =
            (show k :: Text) <> ":" <> show n <> " (" <> show (pct n) <> "%)"
     in if null nonZero then "-" else T.intercalate ", " (map fmt nonZero)

-- * Parser Helpers

-- | Assembly names of the registers, in parser order (@s10@ before @s1@).
registerNames :: [(Text, Register)]
registerNames =
    [ ("zero", Zero)
    , ("a0", A0)
    , ("a1", A1)
    , ("a2", A2)
    , ("a3", A3)
    , ("a4", A4)
    , ("a5", A5)
    , ("a6", A6)
    , ("a7", A7)
    , ("s2", S2)
    , ("s3", S3)
    , ("ra", Ra)
    , ("s4", S4)
    , ("s5", S5)
    , ("s6", S6)
    , ("s7", S7)
    , ("s8", S8)
    , ("s9", S9)
    , ("s10", S10)
    , ("s11", S11)
    , ("t3", T3)
    , ("t4", T4)
    , ("sp", Sp)
    , ("t5", T5)
    , ("t6", T6)
    , ("gp", Gp)
    , ("tp", Tp)
    , ("t0", T0)
    , ("t1", T1)
    , ("t2", T2)
    , ("s0fp", S0Fp)
    , ("s1", S1)
    ]

register :: Parser Register
register = choice [string (toString name) >> return r | (name, r) <- registerNames]

data MemRef w = MemRef {mrOffset :: w, mrReg :: Register} deriving (Show)

-- | Check the offset against the field it is encoded into while parsing, so the
-- caret lands under the operand. The memory slot takes a bare number rather
-- than a label, so there is nothing to resolve first.
requireSignedField :: (IsWord w) => Int -> Int -> w -> Parser w
requireSignedField fieldBits operandPos value
    | value == fitSigned fieldBits value = return value
    | otherwise = do
        setOffset operandPos
        fail $
            concat
                [ "offset "
                , show value
                , " doesn't fit the "
                , show fieldBits
                , "-bit signed field of a 14 byte bundle, expected "
                , show lo
                , ".."
                , show hi
                ]
    where
        hi = bit (fieldBits - 1) - 1 :: Integer
        lo = negate (bit (fieldBits - 1)) :: Integer

memRef :: (IsWord w) => Parser (MemRef w)
memRef = choice [regWithOffset, register <&> MemRef 0]
    where
        memRefOffsetBits = 11
        regWithOffset = do
            operandPos <- getOffset
            offset <- wordLiteral
            void $ char '('
            mrReg <- register
            void $ char ')'
            mrOffset <- requireSignedField memRefOffsetBits operandPos offset
            return MemRef{mrOffset, mrReg}

instance CommentStart (VliwIvIsa _a _b) where
    commentStart = ";"

parseMemOp :: (IsWord w) => Parser (MemoryOp w (Ref w))
parseMemOp =
    choice
        [ cmd2args "lw" Lw register memRef
        , cmd2args "lb" Lb register memRef
        , cmd2args "sw" Sw register memRef
        , cmd2args "sb" Sb register memRef
        , string "nop" >> return NopM
        , pure NopM
        ]

parseAluOp :: (IsWord w) => Parser (AluOp w (Ref w))
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
        , pure NopA
        ]

parseCtrlOp :: (IsWord w) => Parser (ControlOp w (Ref w))
parseCtrlOp =
    choice
        [ cmd1args "j" J reference
        , cmd2args "jal" Jal register reference
        , cmd3args "jalr" Jalr register register referenceWithDirective
        , cmd2args "beqz" Beqz register reference
        , cmd2args "bnez" Bnez register reference
        , cmd3args "blt" Blt register register reference
        , cmd3args "bge" Bge register register reference
        , cmd3args "bltu" Bltu register register reference
        , cmd3args "bgeu" Bgeu register register reference
        , cmd3args "beq" Beq register register reference
        , cmd3args "bne" Bne register register reference
        , string "halt" >> return Halt
        , string "nop" >> return NopC
        , pure NopC
        ]

instance (IsWord w) => MnemonicParser (VliwIvIsa w (Ref w)) where
    mnemonic = do
        hspace
        alu1Op <- parseAluOp
        hspace >> string "/" >> hspace
        alu2Op <- parseAluOp
        hspace >> string "/" >> hspace
        memOp <- parseMemOp
        hspace >> string "/" >> hspace
        ctrlOp <- parseCtrlOp
        eol' (commentStart @(VliwIvIsa _ _))
        return VliwIvIsa{memOp, alu1Op, alu2Op, ctrlOp}

aluImmErr :: Text -> Text -> Text
aluImmErr m x = m <> ": ALU imm " <> x <> " is not in -65536..65535"

instance DerefMnemonic (MemoryOp w) w where
    derefMnemonic _ _ NopM = NopM
    derefMnemonic _ _ (Lw lwRd lwOffsetRs1) = Lw lwRd lwOffsetRs1
    derefMnemonic _ _ (Lb lbRd lbOffsetRs1) = Lb lbRd lbOffsetRs1
    derefMnemonic _ _ (Sw swRs2 swOffsetRs1) = Sw swRs2 swOffsetRs1
    derefMnemonic _ _ (Sb sbRs2 sbOffsetRs1) = Sb sbRs2 sbOffsetRs1

luiImmErr :: Text -> Text -> Text
luiImmErr m x = m <> ": U-type imm " <> x <> " is not in 0..1048575"

instance (IsWord w) => DerefMnemonic (AluOp w) w where
    derefMnemonic f _ (Addi addiRd addiRs1 addiK) = Addi addiRd addiRs1 $ resolveRef (i17 (aluImmErr "addi")) f addiK
    derefMnemonic f _ (Slti sltiRd sltiRs1 sltiK) = Slti sltiRd sltiRs1 $ resolveRef (i17 (aluImmErr "slti")) f sltiK
    derefMnemonic f _ (Lui luiRd luiK) = Lui luiRd $ resolveRef (u20 (luiImmErr "lui")) f luiK
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

branchDispErr, linkDispErr, jumpDispErr, ctrlImmErr :: Text -> Text -> Text
branchDispErr m x = m <> ": CTRL disp " <> x <> " is not in -512..511"
ctrlImmErr m x = m <> ": CTRL imm " <> x <> " is not in -512..511"
linkDispErr m x = m <> ": CTRL disp " <> x <> " is not in -16384..16383"
jumpDispErr m x = m <> ": CTRL disp " <> x <> " is not in -524288..524287"

instance (IsWord w) => DerefMnemonic (ControlOp w) w where
    derefMnemonic f offset (J jK) = J $ resolveRef (i20 (jumpDispErr "j") . subtract offset) f jK
    derefMnemonic f offset (Jal jalRd jalK) = Jal jalRd $ resolveRef (i15 (linkDispErr "jal") . subtract offset) f jalK
    derefMnemonic f _ (Jalr jalrRd jalrRs1 jalrK) = Jalr jalrRd jalrRs1 $ resolveRef (i10 (ctrlImmErr "jalr")) f jalrK
    derefMnemonic f offset (Beqz beqzRs1 beqzK) = Beqz beqzRs1 $ resolveRef (i15 (linkDispErr "beqz") . subtract offset) f beqzK
    derefMnemonic f offset (Bnez bnezRs1 bnezK) = Bnez bnezRs1 $ resolveRef (i15 (linkDispErr "bnez") . subtract offset) f bnezK
    derefMnemonic f offset (Blt bltRs1 bltRs2 bltK) = Blt bltRs1 bltRs2 $ resolveRef (i10 (branchDispErr "blt") . subtract offset) f bltK
    derefMnemonic f offset (Bge bgeRs1 bgeRs2 bgeK) = Bge bgeRs1 bgeRs2 $ resolveRef (i10 (branchDispErr "bge") . subtract offset) f bgeK
    derefMnemonic f offset (Bltu bltuRs1 bltuRs2 bltuK) = Bltu bltuRs1 bltuRs2 $ resolveRef (i10 (branchDispErr "bltu") . subtract offset) f bltuK
    derefMnemonic f offset (Bgeu bgeuRs1 bgeuRs2 bgeuK) = Bgeu bgeuRs1 bgeuRs2 $ resolveRef (i10 (branchDispErr "bgeu") . subtract offset) f bgeuK
    derefMnemonic f offset (Beq beqRs1 beqRs2 beqK) = Beq beqRs1 beqRs2 $ resolveRef (i10 (branchDispErr "beq") . subtract offset) f beqK
    derefMnemonic f offset (Bne bneRs1 bneRs2 bneK) = Bne bneRs1 bneRs2 $ resolveRef (i10 (branchDispErr "bne") . subtract offset) f bneK
    derefMnemonic _ _ Halt = Halt
    derefMnemonic _ _ NopC = NopC

instance (IsWord w) => DerefMnemonic (VliwIvIsa w) w where
    derefMnemonic f offset i@VliwIvIsa{memOp, alu1Op, alu2Op, ctrlOp} =
        i
            { memOp = derefMnemonic f offset memOp
            , alu1Op = derefMnemonic f offset alu1Op
            , alu2Op = derefMnemonic f offset alu2Op
            , ctrlOp = derefMnemonic f offset ctrlOp
            }

instance ByteSize (VliwIvIsa w l) where
    byteSize _ = 14

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

data VliwIvSt w = VliwIvSt
    { pc :: Int
    , mem :: IoMem (VliwIvIsa w w) w
    , regs :: HashMap Register w
    , stopped :: Bool
    , internalError :: Maybe Text
    , randoms :: [Int]
    , vliwLoad :: !VliwLoadAcc
    }
    deriving (Show)

getRandoms :: forall w. Int -> State (VliwIvSt w) [Int]
getRandoms n = do
    VliwIvSt{randoms} <- get
    let (taken, rest) = splitAt n randoms
    modify $ \st -> st{randoms = rest}
    return taken

setPc :: forall w. Int -> State (VliwIvSt w) ()
setPc addr = modify $ \st -> st{pc = addr}

nextPc :: forall w. State (VliwIvSt w) ()
nextPc = do
    VliwIvSt{pc} <- get
    setPc (pc + 14) -- Bundle size 14 bytes

raiseInternalError :: Text -> State (VliwIvSt w) ()
raiseInternalError msg = modify $ \st -> st{internalError = Just msg}

getReg r = do
    VliwIvSt{regs} <- get
    case regs !? r of
        Just value -> return value
        Nothing -> do
            raiseInternalError $ "wrong register: " <> show r
            return def

setReg Zero _ = return ()
setReg r value = modify $ \st@VliwIvSt{regs} -> st{regs = insert r value regs}

getWord addr = do
    st@VliwIvSt{mem} <- get
    case readWord mem addr of
        Right (mem', w) -> do
            put st{mem = mem'}
            return w
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return def

setWord addr w = do
    st@VliwIvSt{mem} <- get
    case writeWord mem addr w of
        Right mem' -> do
            put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err

getByte addr = do
    st@VliwIvSt{mem} <- get
    case readByte mem addr of
        Right (mem', b) -> do
            put st{mem = mem'}
            return b
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return 0

setByte addr byte = do
    st@VliwIvSt{mem} <- get
    case writeByte mem addr byte of
        Right mem' -> do
            put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err

type instance MemOf (VliwIvSt w) = IoMem (VliwIvIsa w w) w

instance (IsWord w) => InitState (VliwIvSt w) where
    initState pc dump randomStream =
        VliwIvSt
            { pc
            , mem = dump
            , regs = def
            , stopped = False
            , internalError = Nothing
            , randoms = randomStream
            , vliwLoad = emptyVliwLoad
            }

instance (IsWord w) => Inspectable (VliwIvSt w) where
    type WordOf (VliwIvSt w) = w
    type IsaOf (VliwIvSt w) = VliwIvIsa w w

    programCounter VliwIvSt{pc} = pc
    memoryDump VliwIvSt{mem} = mem
    ioStreams VliwIvSt{mem = IoMem{mIoStreams}} = mIoStreams
    isHalted VliwIvSt{stopped} = stopped

    views =
        withRenames
            [(show r, name) | r <- allRegisters, name : _ <- [asmNames r]]
            [ valueView name ("Register `" <> T.intercalate "`/`" (asmNames r) <> "`.") (fromMaybe 0 . (!? r) . regs . seState)
            | r <- allRegisters
            , name <- asmNames r
            ]
        where
            asmNames r = [n | (n, r') <- registerNames, r' == r]

    isaStats =
        [ Stat "vliw:load-percent" "Average slot use over the run, in percent of the four slots." $ \StateEnv{seFinal} ->
            show (vliwLoadPercent (vliwLoad seFinal)) <> "%"
        , Stat "vliw:avg-load" "Average active slots per executed bundle, `0.00`-`4.00`." (vliwAvgLoad . vliwLoad . seFinal)
        , Stat
            "vliw:bundles-by-load"
            "Executed bundles by active slot count: `K:N (P%)`."
            (renderBundlesByLoad . vliwLoad . seFinal)
        ]

instance (IsWord w) => Machine (VliwIvSt w) (VliwIvIsa w w) w where
    instructionFetch = do
        st <- get
        case st of
            VliwIvSt{stopped = True} -> return $ Left halted
            VliwIvSt{internalError = Just err} -> return $ Left err
            VliwIvSt{pc, mem} ->
                case readInstruction mem pc of
                    Left err -> return $ Left err
                    Right (mem', instruction) -> do
                        put st{mem = mem'}
                        return $ Right (pc, instruction)

    instructionExecute _pc bundle@VliwIvIsa{memOp, alu1Op, alu2Op, ctrlOp} = do
        -- Tally per-bundle slot usage for the vliw:* report variables.
        modify $ \st -> st{vliwLoad = recordBundle bundle (vliwLoad st)}
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
            shuffleList :: [a] -> State (VliwIvSt w) [a]
            shuffleList [] = return []
            shuffleList [x] = return [x]
            shuffleList xs = do
                indices <- getRandoms (length xs)
                return $ shuffle xs (map (`mod` length xs) indices)

            shuffle :: [a] -> [Int] -> [a]
            shuffle [] _ = []
            shuffle [x] _ = [x]
            shuffle xs (idx : rest) =
                case splitAt idx xs of
                    (before, item : after) -> item : shuffle (before ++ after) rest
                    _ -> xs
            shuffle xs [] = xs

            -- Compute memory operation result without applying it
            computeMem :: MemoryOp w w -> State (VliwIvSt w) (Maybe (Register, w))
            computeMem NopM = return Nothing
            computeMem (Lw lwRd (MemRef mrOffset mrReg)) = do
                rs1' <- getReg mrReg
                w <- getWord $ fromEnum (mrOffset + rs1')
                return $ Just (lwRd, w)
            computeMem (Lb lbRd (MemRef mrOffset mrReg)) = do
                rs1' <- getReg mrReg
                b <- getByte $ fromEnum (mrOffset + rs1')
                return $ Just (lbRd, fromIntegral (fromIntegral b :: Int8))
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
            applyMemResult :: Maybe (Register, w) -> State (VliwIvSt w) ()
            applyMemResult Nothing = return ()
            applyMemResult (Just (reg, val)) = setReg reg val

            -- Compute ALU operation result without applying it
            computeAlu :: AluOp w w -> State (VliwIvSt w) (Maybe (Register, w))
            computeAlu NopA = return Nothing
            computeAlu (Addi addiRd addiRs1 addiK) = do
                rs1' <- getReg addiRs1
                return $ Just (addiRd, rs1' + addiK)
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
            computeAlu (Div divRd divRs1 divRs2) = aluOpCompute divRd divRs1 divRs2 id id $ \r1 r2 -> case r2 of
                0 -> -1
                -1 | r1 == minBound -> r1
                _ -> quot r1 r2
            computeAlu (Rem remRd remRs1 remRs2) = aluOpCompute remRd remRs1 remRs2 id id $ \r1 r2 -> case r2 of
                0 -> r1
                -1 | r1 == minBound -> 0
                _ -> rem r1 r2
            computeAlu (Sll sllRd sllRs1 sllRs2) = aluOpCompute sllRd sllRs1 sllRs2 id id (\r1 r2 -> r1 `shiftL` (fromEnum r2 .&. 0x1F))
            computeAlu (Srl srlRd srlRs1 srlRs2) = aluOpCompute srlRd srlRs1 srlRs2 id id (\r1 r2 -> lShiftR r1 (r2 .&. 0x1F))
            computeAlu (Sra sraRd sraRs1 sraRs2) = aluOpCompute sraRd sraRs1 sraRs2 id id (\r1 r2 -> r1 `shiftR` (fromEnum r2 .&. 0x1F))
            computeAlu (And andRd andRs1 andRs2) = aluOpCompute andRd andRs1 andRs2 id id (.&.)
            computeAlu (Or orRd orRs1 orRs2) = aluOpCompute orRd orRs1 orRs2 id id (.|.)
            computeAlu (Xor xorRd xorRs1 xorRs2) = aluOpCompute xorRd xorRs1 xorRs2 id id xor
            computeAlu (Slti sltiRd sltiRs1 sltiK) = do
                rs1' <- getReg sltiRs1
                return $ Just (sltiRd, if rs1' < sltiK then 1 else 0)
            computeAlu (Lui luiRd luiK) = return $ Just (luiRd, luiK `shiftL` 12)
            computeAlu (Mv mvRd mvRs) = do
                val <- getReg mvRs
                return $ Just (mvRd, val)

            -- Apply ALU operation result
            applyAluResult :: Maybe (Register, w) -> State (VliwIvSt w) ()
            applyAluResult Nothing = return ()
            applyAluResult (Just (reg, val)) = setReg reg val

            -- Helper for ALU operations with two source registers
            aluOpCompute rd rs1 rs2 f1 f2 fd = do
                r1 <- f1 <$> getReg rs1
                r2 <- f2 <$> getReg rs2
                return $ Just (rd, fd r1 r2)

            execCtrl NopC = return False
            execCtrl (J jK) = do
                VliwIvSt{pc} <- get
                setPc (pc + fromEnum jK)
                return True
            execCtrl (Jal jalRd jalK) = do
                VliwIvSt{pc} <- get
                setReg jalRd (toEnum pc + 14)
                setPc (pc + fromEnum jalK)
                return True
            execCtrl (Jalr jalrRd jalrRs1 jalrK) = do
                VliwIvSt{pc} <- get
                rs1' <- getReg jalrRs1
                setReg jalrRd (toEnum pc + 14)
                setPc (fromEnum (rs1' + jalrK))
                return True
            execCtrl (Beqz beqzRs1 beqzK) = branchIf beqzRs1 beqzK (== 0)
            execCtrl (Bnez bnezRs1 bnezK) = branchIf bnezRs1 bnezK (/= 0)
            execCtrl (Blt bltRs1 bltRs2 bltK) = branchIf2 bltRs1 bltRs2 bltK (<)
            execCtrl (Bge bgeRs1 bgeRs2 bgeK) = branchIf2 bgeRs1 bgeRs2 bgeK (>=)
            execCtrl (Bltu bltuRs1 bltuRs2 bltuK) = branchIf2u bltuRs1 bltuRs2 bltuK (<)
            execCtrl (Bgeu bgeuRs1 bgeuRs2 bgeuK) = branchIf2u bgeuRs1 bgeuRs2 bgeuK (>=)
            execCtrl (Beq beqRs1 beqRs2 beqK) = branchIf2 beqRs1 beqRs2 beqK (==)
            execCtrl (Bne bneRs1 bneRs2 bneK) = branchIf2 bneRs1 bneRs2 bneK (/=)
            execCtrl Halt = do
                modify $ \st -> st{stopped = True}
                return True

            branchIf rs1 k cond = do
                VliwIvSt{pc} <- get
                rs1' <- getReg rs1
                if cond rs1'
                    then do
                        setPc (pc + fromEnum k)
                        return True
                    else return False

            branchIf2 rs1 rs2 k cond = do
                VliwIvSt{pc} <- get
                rs1' <- getReg rs1
                rs2' <- getReg rs2
                if cond rs1' rs2'
                    then do
                        setPc (pc + fromEnum k)
                        return True
                    else return False

            branchIf2u rs1 rs2 k cond = do
                VliwIvSt{pc} <- get
                rs1' <- fromSign <$> getReg rs1
                rs2' <- fromSign <$> getReg rs2
                if cond rs1' rs2'
                    then do
                        setPc (pc + fromEnum k)
                        return True
                    else return False
