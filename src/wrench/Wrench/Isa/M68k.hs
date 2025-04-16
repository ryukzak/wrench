{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Isa.M68k (
    Isa(..),
    MachineState(..),
) where

import Data.Bits (Bits(..), complement, shiftL, shiftR, (.&.), (.|.))
import Data.Default (def)
import Data.Text qualified as T
import Machine.Memory
import Machine.Types
import Relude
import Report
import Text.Megaparsec (choice, try)
import Text.Megaparsec.Char (hspace, hspace1, string)
import Translator.Parser.Misc
import Translator.Parser.Types
import Translator.Types

-- | The 'Isa' type represents the instruction set architecture for the M68k machine.
-- Each constructor corresponds to a specific instruction.
data Isa w l
    = Move l l
    | Lea l l
    | Exg l l
    | Add l l
    | Sub l l
    | Mul l l
    | Div l l
    | Neg l
    | And l l
    | Or l l
    | Eor l l
    | Lsl l l
    | Lsr l l
    | Bra l
    | Beq l
    | Bne l
    | Jmp l
    | Push l
    | Pop l
    | Halt
    deriving (Show)

instance CommentStart (Isa w l) where
    commentStart = ";"

instance (MachineWord w) => MnemonicParser (Isa w (Ref w)) where
    mnemonic =
        choice
            [ cmd2args "move" Move reference reference
            , cmd2args "lea" Lea reference reference
            , cmd2args "exg" Exg reference reference
            , cmd2args "add" Add reference reference
            , cmd2args "sub" Sub reference reference
            , cmd2args "mul" Mul reference reference
            , cmd2args "div" Div reference reference
            , cmd1args "neg" Neg reference
            , cmd2args "and" And reference reference
            , cmd2args "or" Or reference reference
            , cmd2args "eor" Eor reference reference
            , cmd2args "lsl" Lsl reference reference
            , cmd2args "lsr" Lsr reference reference
            , cmd1args "bra" Bra reference
            , cmd1args "beq" Beq reference
            , cmd1args "bne" Bne reference
            , cmd1args "jmp" Jmp reference
            , cmd1args "push" Push reference
            , cmd1args "pop" Pop reference
            , cmdMnemonic0 "halt" >> return Halt
            ]

cmdMnemonic0 :: String -> Parser ()
cmdMnemonic0 mnemonic = try $ do
    hspace
    void (string mnemonic)
    hspace1 <|> eol' ";"

cmdMnemonic1 :: String -> Parser (Ref w) -> Parser (Ref w)
cmdMnemonic1 mnemonic refParser = try $ do
    void hspace
    void (string mnemonic)
    hspace1
    ref <- refParser
    hspace1 <|> eol' ";"
    return ref

cmd1args :: String -> (l -> Isa w l) -> Parser l -> Parser (Isa w l)
cmd1args mnemonic constructor a =
    constructor <$> (cmdMnemonic mnemonic *> hspace *> a)

cmd2args :: String -> (l -> l -> Isa w l) -> Parser l -> Parser l -> Parser (Isa w l)
cmd2args mnemonic constructor a b =
    constructor
        <$> (cmdMnemonic mnemonic *> hspace *> a)
        <*> (comma *> b)

comma = hspace >> string "," >> hspace

instance (MachineWord w) => DerefMnemonic (Isa w) w where
    derefMnemonic f offset i =
        let relF = fmap (\x -> x - offset) . f
         in case i of
                Move src dst -> Move (deref' f src) (deref' f dst)
                Lea src dst -> Lea (deref' f src) (deref' f dst)
                Exg r1 r2 -> Exg (deref' f r1) (deref' f r2)
                Add src dst -> Add (deref' f src) (deref' f dst)
                Sub src dst -> Sub (deref' f src) (deref' f dst)
                Mul src dst -> Mul (deref' f src) (deref' f dst)
                Div src dst -> Div (deref' f src) (deref' f dst)
                Neg dst -> Neg (deref' f dst)
                And src dst -> And (deref' f src) (deref' f dst)
                Or src dst -> Or (deref' f src) (deref' f dst)
                Eor src dst -> Eor (deref' f src) (deref' f dst)
                Lsl cnt dst -> Lsl (deref' f cnt) (deref' f dst)
                Lsr cnt dst -> Lsr (deref' f cnt) (deref' f dst)
                Bra addr -> Bra (deref' relF addr)
                Beq addr -> Beq (deref' relF addr)
                Bne addr -> Bne (deref' relF addr)
                Jmp addr -> Jmp (deref' f addr)
                Push src -> Push (deref' f src)
                Pop dst -> Pop (deref' f dst)
                Halt -> Halt

instance ByteLength (Isa w l) where
    byteLength _ = 2 -- Simplified assumption: all instructions are 2 bytes.

data MachineState mem w = State
    { pc :: Int
    , regs :: HashMap Text w
    , mem :: mem
    , stopped :: Bool
    , internalError :: Maybe Text
    }
    deriving (Show)

instance (MachineWord w) => InitState (IoMem (Isa w w) w) (MachineState (IoMem (Isa w w) w) w) where
    initState pc dump =
        State
            { pc
            , regs = def
            , mem = dump
            , stopped = False
            , internalError = Nothing
            }

instance (MachineWord w) => StateInterspector (MachineState (IoMem (Isa w w) w) w) (Isa w w) w where
    programCounter State{pc} = pc
    memoryDump State{mem = IoMem{mIoCells}} = mIoCells
    ioStreams State{mem = IoMem{mIoStreams}} = mIoStreams
    reprState labels st v
        | Just v' <- defaultView labels st v = v'
    reprState labels st@State{regs} v =
        case T.splitOn ":" v of
            [r] -> reprState labels st (r <> ":dec")
            [r, f]
                | Just r' <- regs !? r ->
                    viewRegister f r'
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

    instructionStep = do
        ((_pc, instruction) :: (Int, Isa w w)) <- either (error . ("internal error: " <>)) id <$> instructionFetch
        case instruction of
            Move src dst -> do
                srcVal <- getOperand src
                setOperand dst srcVal
                nextPc
            Lea src dst -> do
                addr <- getEffectiveAddress src
                setOperand dst addr
                nextPc
            Exg r1 r2 -> do
                val1 <- getOperand r1
                val2 <- getOperand r2
                setOperand r1 val2
                setOperand r2 val1
                nextPc
            Add src dst -> binaryOp src dst (+)
            Sub src dst -> binaryOp src dst (-)
            Mul src dst -> binaryOp src dst (*)
            Div src dst -> binaryOp src dst div
            Neg dst -> unaryOp dst negate
            And src dst -> binaryOp src dst (.&.)
            Or src dst -> binaryOp src dst (.|.)
            Eor src dst -> binaryOp src dst xor
            Lsl cnt dst -> shiftOp cnt dst shiftL
            Lsr cnt dst -> shiftOp cnt dst shiftR
            Bra addr -> branch addr
            Beq addr -> conditionalBranch addr (== 0)
            Bne addr -> conditionalBranch addr (/= 0)
            Jmp addr -> jump addr
            Push src -> do
                val <- getOperand src
                pushStack val
                nextPc
            Pop dst -> do
                val <- popStack
                setOperand dst val
                nextPc
            Halt -> modify $ \st -> st{stopped = True}
        where
            binaryOp src dst op = do
                srcVal <- getOperand src
                dstVal <- getOperand dst
                setOperand dst (dstVal `op` srcVal)
                nextPc

            unaryOp dst op = do
                dstVal <- getOperand dst
                setOperand dst (op dstVal)
                nextPc

            shiftOp cnt dst op = do
                cntVal <- getOperand cnt
                dstVal <- getOperand dst
                setOperand dst (dstVal `op` fromEnum cntVal)
                nextPc

            branch addr = setPc (fromEnum addr)

            conditionalBranch addr cond = do
                val <- getOperand (Ref "SR") -- Simplified: Assume SR holds condition flags
                if cond val
                    then branch addr
                    else nextPc

            jump addr = setPc (fromEnum addr)

            pushStack val = do
                sp <- getOperand (Ref "A7")
                setOperand (Ref "A7") (sp - 2)
                setWord (fromEnum sp) val

            popStack = do
                sp <- getOperand (Ref "A7")
                val <- getWord (fromEnum sp)
                setOperand (Ref "A7") (sp + 2)
                return val
