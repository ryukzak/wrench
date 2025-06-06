{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Wrench.Isa.Acc32 (
    Isa (..),
    Acc32State,
) where

import Data.Bits (Bits (..), complement, shiftL, shiftR, (.&.))
import Data.Default (def)
import Data.Text qualified as T
import Relude
import Text.Megaparsec (choice, try)
import Text.Megaparsec.Char (hspace, hspace1, string)
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Report
import Wrench.Translator.Parser.Misc
import Wrench.Translator.Parser.Types
import Wrench.Translator.Types

-- | The 'Isa' type represents the instruction set architecture for the Acc32 machine.
-- Each constructor corresponds to a specific instruction.
data Isa w l
    = -- | Syntax: @load_imm <address>@ Load an immediate value into the accumulator.
      LoadImm l
    | -- | Syntax: @load_addr <address>@ Load a value from a specific address into the accumulator.
      LoadAddr l
    | -- | Syntax: @load <offset>@ Load a value from a relative address into the accumulator.
      Load l
    | -- | Syntax: @load_ind <address>@ Load a value from an indirect address into the accumulator.
      LoadInd l
    | -- | Syntax: @store_addr <address>@ Store the accumulator value into a specific address.
      StoreAddr l
    | -- | Syntax: @store <offset>@ Store the accumulator value into a relative address.
      Store l
    | -- | Syntax: @store_ind <address>@ Store the accumulator value into an indirect address.
      StoreInd l
    | -- | Syntax: @add <address>@ Add a value from a specific address to the accumulator.
      Add l
    | -- | Syntax: @sub <address>@ Subtract from the accumulator a value from a specific address.
      Sub l
    | -- | Syntax: @mul <address>@ Multiply the accumulator by a value from a specific address.
      Mul l
    | -- | Syntax: @div <address>@ Divide the accumulator by a value from a specific address.
      Div l
    | -- | Syntax: @rem <address>@ Compute the remainder of the accumulator divided by a value from a specific address.
      Rem l
    | -- | Syntax: @clc@ Clear carry flag
      Clv
    | -- | Syntax: @clc@ Clear carry flag
      Clc
    | -- | Syntax: @shiftl <address>@ Shift the accumulator left by a number of bits from a specific address.
      ShiftL l
    | -- | Syntax: @shiftr <address>@ Shift the accumulator right by a number of bits from a specific address.
      ShiftR l
    | -- | Syntax: @and <address>@ Perform a bitwise AND on the accumulator with a value from a specific address.
      And l
    | -- | Syntax: @or <address>@ Perform a bitwise OR on the accumulator with a value from a specific address.
      Or l
    | -- | Syntax: @xor <address>@ Perform a bitwise XOR on the accumulator with a value from a specific address.
      Xor l
    | -- | Syntax: @not@ Perform a bitwise NOT on the accumulator.
      Not
    | -- | Syntax: @jmp <address>@ Jump to a specific address.
      Jmp l
    | -- | Syntax: @beqz <address>@ Jump to a specific address if the accumulator is zero.
      Beqz l
    | -- | Syntax: @bnez <address>@ Jump to a specific address if the accumulator is not zero.
      Bnez l
    | -- | Syntax: @bgt <address>@ Jump to a specific address if the accumulator is greater than zero.
      Bgz l
    | -- | Syntax: @ble <address>@ Jump to a specific address if the accumulator is less than zero.
      Blz l
    | Bvs l
    | Bvc l
    | Bcs l
    | Bcc l
    | -- | Syntax: @halt@ Halt the machine.
      Halt
    deriving (Show)

instance CommentStart (Isa w l) where
    commentStart = ";"

instance (MachineWord w) => MnemonicParser (Isa w (Ref w)) where
    mnemonic =
        choice
            [ LoadImm <$> cmdMnemonic1 "load_imm" reference
            , LoadAddr <$> cmdMnemonic1 "load_addr" reference
            , LoadInd <$> cmdMnemonic1 "load_ind" reference
            , Load <$> cmdMnemonic1 "load" reference16
            , StoreAddr <$> cmdMnemonic1 "store_addr" reference
            , StoreInd <$> cmdMnemonic1 "store_ind" reference
            , Store <$> cmdMnemonic1 "store" reference16
            , Add <$> cmdMnemonic1 "add" reference16
            , Sub <$> cmdMnemonic1 "sub" reference16
            , Mul <$> cmdMnemonic1 "mul" reference16
            , Div <$> cmdMnemonic1 "div" reference16
            , Rem <$> cmdMnemonic1 "rem" reference16
            , cmdMnemonic0 "clv" >> return Clv
            , cmdMnemonic0 "clc" >> return Clc
            , ShiftL <$> cmdMnemonic1 "shiftl" reference16
            , ShiftR <$> cmdMnemonic1 "shiftr" reference16
            , And <$> cmdMnemonic1 "and" reference16
            , Or <$> cmdMnemonic1 "or" reference16
            , Xor <$> cmdMnemonic1 "xor" reference16
            , cmdMnemonic0 "not" >> return Not
            , Jmp <$> cmdMnemonic1 "jmp" reference
            , Beqz <$> cmdMnemonic1 "beqz" reference
            , Bnez <$> cmdMnemonic1 "bnez" reference
            , Bgz <$> cmdMnemonic1 "bgt" reference
            , Blz <$> cmdMnemonic1 "ble" reference
            , Bvs <$> cmdMnemonic1 "bvs" reference
            , Bvc <$> cmdMnemonic1 "bvc" reference
            , Bcs <$> cmdMnemonic1 "bcs" reference
            , Bcc <$> cmdMnemonic1 "bcc" reference
            , cmdMnemonic0 "halt" >> return Halt
            ]

reference16 :: (MachineWord w) => Parser (Ref w)
reference16 = referenceWithFn (`signBitAnd` 0x0000FFFF)

cmdMnemonic0 :: String -> Parser ()
cmdMnemonic0 mnemonic = try $ do
    hspace
    void (string mnemonic)
    hspace1 <|> eol' "\\"

cmdMnemonic1 :: String -> Parser (Ref w) -> Parser (Ref w)
cmdMnemonic1 mnemonic refParser = try $ do
    void hspace
    void (string mnemonic)
    hspace1
    ref <- refParser
    hspace1 <|> eol' "\\"
    return ref

instance (MachineWord w) => DerefMnemonic (Isa w) w where
    derefMnemonic f offset i =
        let relF = fmap (\x -> x - offset) . f
         in case i of
                LoadImm l -> LoadImm (deref' f l)
                LoadAddr l -> LoadAddr (deref' f l)
                Load l -> Load (deref' relF l)
                LoadInd l -> LoadInd (deref' f l)
                StoreAddr l -> StoreAddr (deref' f l)
                Store l -> Store (deref' relF l)
                StoreInd l -> StoreInd (deref' f l)
                Add l -> Add (deref' f l)
                Sub l -> Sub (deref' f l)
                Mul l -> Mul (deref' f l)
                Div l -> Div (deref' f l)
                Rem l -> Rem (deref' f l)
                Clv -> Clv
                Clc -> Clc
                ShiftL l -> ShiftL (deref' f l)
                ShiftR l -> ShiftR (deref' f l)
                And l -> And (deref' f l)
                Or l -> Or (deref' f l)
                Xor l -> Xor (deref' f l)
                Not -> Not
                Beqz l -> Beqz (deref' f l)
                Bnez l -> Bnez (deref' f l)
                Bgz l -> Bgz (deref' f l)
                Blz l -> Blz (deref' f l)
                Bvs l -> Bvs (deref' f l)
                Bvc l -> Bvc (deref' f l)
                Bcs l -> Bcs (deref' f l)
                Bcc l -> Bcc (deref' f l)
                Jmp l -> Jmp (deref' f l)
                Halt -> Halt

instance ByteSize (Isa w l) where
    byteSize LoadImm{} = 5
    byteSize LoadAddr{} = 5
    byteSize LoadInd{} = 5
    byteSize StoreAddr{} = 5
    byteSize StoreInd{} = 5
    byteSize Beqz{} = 5
    byteSize Bnez{} = 5
    byteSize Bgz{} = 5
    byteSize Blz{} = 5
    byteSize Bvs{} = 5
    byteSize Bvc{} = 5
    byteSize Bcs{} = 5
    byteSize Bcc{} = 5
    byteSize Jmp{} = 5
    byteSize Not = 1
    byteSize Clv = 1
    byteSize Clc = 1
    byteSize Halt = 1
    byteSize _ = 3

type Acc32State w = MachineState (IoMem (Isa w w) w) w

data MachineState mem w = State
    { pc :: Int
    , acc :: w
    , overflowFlag :: Bool
    , carryFlag :: Bool
    , ram :: mem
    , stopped :: Bool
    , internalError :: Maybe Text
    }
    deriving (Show)

instance (MachineWord w) => InitState (IoMem (Isa w w) w) (MachineState (IoMem (Isa w w) w) w) where
    initState pc dump =
        State
            { acc = 0
            , overflowFlag = False
            , carryFlag = False
            , ram = dump
            , stopped = False
            , pc = pc
            , internalError = Nothing
            }

setPc :: forall w. Int -> State (MachineState (IoMem (Isa w w) w) w) ()
setPc addr = modify $ \st -> st{pc = addr}

setOverflowFlag :: forall w. Bool -> State (MachineState (IoMem (Isa w w) w) w) ()
setOverflowFlag overflowFlag = modify $ \st -> st{overflowFlag}

setCarryFlag :: forall w. Bool -> State (MachineState (IoMem (Isa w w) w) w) ()
setCarryFlag carryFlag = modify $ \st -> st{carryFlag}

nextPc :: (MachineWord w) => State (MachineState (IoMem (Isa w w) w) w) ()
nextPc = do
    instructionFetch >>= \case
        Right (pc, instruction) -> setPc (pc + byteSize instruction)
        Left err -> raiseInternalError $ "nextPc: " <> err

raiseInternalError :: Text -> State (MachineState (IoMem (Isa w w) w) w) ()
raiseInternalError msg = modify $ \st -> st{internalError = Just msg}

getWord addr = do
    st@State{ram} <- get
    case readWord ram addr of
        Right (ram', w) -> put st{ram = ram'} >> return w
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return def

setWord addr w = do
    st@State{ram} <- get
    case writeWord ram addr w of
        Right ram' -> put st{ram = ram'}
        Left err -> raiseInternalError $ "memory access error: " <> err

setAcc w = modify $ \st -> st{acc = w}

getAcc :: State (MachineState (IoMem (Isa w w) w) w) w
getAcc = acc <$> get

getOverflowFlag :: State (MachineState (IoMem (Isa w w) w) w) Bool
getOverflowFlag = overflowFlag <$> get

getCarryFlag :: State (MachineState (IoMem (Isa w w) w) w) Bool
getCarryFlag = carryFlag <$> get

instance (MachineWord w) => StateInterspector (MachineState (IoMem (Isa w w) w) w) (IoMem (Isa w w) w) (Isa w w) w where
    programCounter State{pc} = pc
    memoryDump State{ram} = ram
    ioStreams State{ram = IoMem{mIoStreams}} = mIoStreams
    reprState labels st v
        | Just v' <- defaultView labels st v = v'
    reprState labels st@State{acc, overflowFlag, carryFlag} v =
        case T.splitOn ":" v of
            ["V"] -> if overflowFlag then "1" else "0"
            ["C"] -> if carryFlag then "1" else "0"
            [r] -> reprState labels st (r <> ":dec")
            ["Acc", f] -> viewRegister f acc
            [r, _] -> unknownView r
            _ -> errorView v

instance (MachineWord w) => Machine (MachineState (IoMem (Isa w w) w) w) (Isa w w) w where
    instructionFetch =
        get
            <&> ( \case
                    State{stopped = True} -> Left halted
                    State{internalError = Just err} -> Left err
                    State{pc, ram} -> do
                        instruction <- readInstruction ram pc
                        Right (pc, instruction)
                )
    instructionExecute pc instruction =
        case instruction of
            LoadImm a -> setAcc a >> nextPc
            LoadAddr a -> do
                value <- getWord $ fromEnum a
                setAcc value
                nextPc
            Load a -> getWord (pc + fromEnum a) >>= setAcc >> nextPc
            LoadInd a -> do
                addr <- getWord $ fromEnum a
                value <- getWord $ fromEnum addr
                setAcc value
                nextPc
            StoreAddr a -> getAcc >>= setWord (fromEnum a) >> nextPc
            StoreInd a -> do
                addr <- getWord $ fromEnum a
                acc <- getAcc
                setWord (fromEnum addr) acc
                nextPc
            Store a -> getAcc >>= setWord (fromEnum (pc + fromEnum a)) >> nextPc
            Add a -> withExt addExt a
            Sub a -> withExt subExt a
            Mul a -> withExt mulExt a
            Div a -> withAcc div a
            Rem a -> withAcc rem a
            Clv -> setOverflowFlag False >> nextPc
            Clc -> setCarryFlag False >> nextPc
            ShiftL a -> withAcc (\x y -> shiftL x (fromEnum y)) a
            ShiftR a -> withAcc (\x y -> shiftR x (fromEnum y)) a
            And a -> withAcc (.&.) a
            Or a -> withAcc (.|.) a
            Xor a -> withAcc xor a
            Not -> getAcc >>= setAcc . complement >> nextPc
            Jmp a -> setPc (fromEnum a)
            Beqz a -> condJmp (== 0) a
            Bnez a -> condJmp (/= 0) a
            Bgz a -> condJmp (> 0) a
            Blz a -> condJmp (< 0) a
            Bvs a -> getOverflowFlag >>= \overflow -> if overflow then setPc (fromEnum a) else nextPc
            Bvc a -> getOverflowFlag >>= \overflow -> if not overflow then setPc (fromEnum a) else nextPc
            Bcs a -> getCarryFlag >>= \carry -> if carry then setPc (fromEnum a) else nextPc
            Bcc a -> getCarryFlag >>= \carry -> if not carry then setPc (fromEnum a) else nextPc
            Halt -> modify $ \st -> st{stopped = True}
        where
            withExt f addr = do
                acc <- getAcc
                value <- getWord $ fromEnum addr
                let Ext{value = result, overflow, carry} = f acc value
                setAcc result
                setOverflowFlag overflow
                setCarryFlag carry
                nextPc
            withAcc f addr = do
                acc <- getAcc
                value <- getWord $ fromEnum addr
                setAcc $ f acc value
                nextPc
            condJmp p a = do
                acc <- getAcc
                if p acc
                    then setPc (fromEnum a)
                    else nextPc
