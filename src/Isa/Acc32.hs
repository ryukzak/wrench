{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Isa.Acc32 (
    Isa (..),
    MachineState (..),
    Register (..),
) where

import Data.Bits (Bits (..), complement, shiftL, shiftR, (.&.))
import Machine.Memory
import Machine.Types
import Relude
import Text.Megaparsec (choice)
import Text.Megaparsec.Char (hspace, hspace1, string)
import Translator.Parser.Misc
import Translator.Parser.Types
import Translator.Types

data Register = Acc
    deriving (Show, Generic, Eq, Read)

instance Hashable Register

-- | The 'Isa' type represents the instruction set architecture for the Acc32 machine.
-- Each constructor corresponds to a specific instruction.
data Isa w l
    = -- | Syntax: @load_imm <value>@ Load an immediate value into the accumulator.
      LoadImm l
    | -- | Syntax: @load_addr <address>@ Load a value from a specific address into the accumulator.
      LoadAddr l
    | -- | Syntax: @load_rel <offset>@ Load a value from a relative address into the accumulator.
      LoadRel l
    | -- | Syntax: @load_ind <address>@ Load a value from an indirect address into the accumulator.
      LoadInd l
    | -- | Syntax: @store_addr <address>@ Store the accumulator value into a specific address.
      StoreAddr l
    | -- | Syntax: @store_rel <offset>@ Store the accumulator value into a relative address.
      StoreRel l
    | -- | Syntax: @store_ind <address>@ Store the accumulator value into an indirect address.
      StoreInd l
    | -- | Syntax: @add <value>@ Add a value to the accumulator.
      Add l
    | -- | Syntax: @sub <value>@ Subtract a value from the accumulator.
      Sub l
    | -- | Syntax: @mul <value>@ Multiply the accumulator by a value.
      Mul l
    | -- | Syntax: @div <value>@ Divide the accumulator by a value.
      Div l
    | -- | Syntax: @rem <value>@ Compute the remainder of the accumulator divided by a value.
      Rem l
    | -- | Syntax: @shiftl <bits>@ Shift the accumulator left by a number of bits.
      ShiftL l
    | -- | Syntax: @shiftr <bits>@ Shift the accumulator right by a number of bits.
      ShiftR l
    | -- | Syntax: @and <value>@ Perform a bitwise AND on the accumulator with a value.
      And l
    | -- | Syntax: @or <value>@ Perform a bitwise OR on the accumulator with a value.
      Or l
    | -- | Syntax: @xor <value>@ Perform a bitwise XOR on the accumulator with a value.
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
    | -- | Syntax: @halt@ Halt the machine.
      Halt
    deriving (Show)

instance CommentStart (Isa w l) where
    commentStart = ";"

instance (MachineWord w) => MnemonicParser (Isa w (Ref w)) where
    mnemonic =
        hspace *> cmd <* (hspace1 <|> eol' "\\")
        where
            cmd =
                choice
                    [ LoadImm <$> (string "load_imm" *> hspace1 *> reference)
                    , LoadAddr <$> (string "load_addr" *> hspace1 *> reference)
                    , LoadRel <$> (string "load_rel" *> hspace1 *> reference)
                    , LoadInd <$> (string "load_ind" *> hspace1 *> reference)
                    , StoreAddr <$> (string "store_addr" *> hspace1 *> reference)
                    , StoreRel <$> (string "store_rel" *> hspace1 *> reference)
                    , StoreInd <$> (string "store_ind" *> hspace1 *> reference)
                    , Add <$> (string "add" *> hspace1 *> reference)
                    , Sub <$> (string "sub" *> hspace1 *> reference)
                    , Mul <$> (string "mul" *> hspace1 *> reference)
                    , Div <$> (string "div" *> hspace1 *> reference)
                    , Rem <$> (string "rem" *> hspace1 *> reference)
                    , ShiftL <$> (string "shiftl" *> hspace1 *> reference)
                    , ShiftR <$> (string "shiftr" *> hspace1 *> reference)
                    , And <$> (string "and" *> hspace1 *> reference)
                    , Or <$> (string "or" *> hspace1 *> reference)
                    , Xor <$> (string "xor" *> hspace1 *> reference)
                    , string "not" >> return Not
                    , Jmp <$> (string "jmp" *> hspace1 *> reference)
                    , Beqz <$> (string "beqz" *> hspace1 *> reference)
                    , Bnez <$> (string "bnez" *> hspace1 *> reference)
                    , Bgz <$> (string "bgt" *> hspace1 *> reference)
                    , Blz <$> (string "ble" *> hspace1 *> reference)
                    , string "halt" >> return Halt
                    ]

instance (MachineWord w) => DerefMnemonic (Isa w) w where
    derefMnemonic f offset i =
        let relF = fmap (\x -> x - offset) . f
         in case i of
                LoadImm l -> LoadImm (deref' f l)
                LoadAddr l -> LoadAddr (deref' f l)
                LoadRel l -> LoadRel (deref' relF l)
                LoadInd l -> LoadInd (deref' f l)
                StoreAddr l -> StoreAddr (deref' f l)
                StoreRel l -> StoreRel (deref' relF l)
                StoreInd l -> StoreInd (deref' f l)
                Add l -> Add (deref' f l)
                Sub l -> Sub (deref' f l)
                Mul l -> Mul (deref' f l)
                Div l -> Div (deref' f l)
                Rem l -> Rem (deref' f l)
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
                Jmp l -> Jmp (deref' f l)
                Halt -> Halt

-- FIXME: make size more real
instance ByteLength (Isa w l) where
    byteLength _ = 5

data MachineState mem w = State
    { pc :: Int
    , acc :: w
    , ram :: mem
    , stopped :: Bool
    }
    deriving (Show)

instance (MachineWord w) => InitState (IoMem (Isa w w) w) (MachineState (IoMem (Isa w w) w) w) where
    initState pc dump = State{acc = 0, ram = dump, stopped = False, pc = pc}

setPc :: forall w. Int -> State (MachineState (IoMem (Isa w w) w) w) ()
setPc addr = modify $ \st -> st{pc = addr}

nextPc :: (MachineWord w) => State (MachineState (IoMem (Isa w w) w) w) ()
nextPc = do
    (pc, instruction) <- fromMaybe (error "internal error") <$> instructionFetch
    setPc (pc + byteLength instruction)

getWord addr = do
    st@State{ram} <- get
    let (w, ram') = runState (readWord addr) ram
    put st{ram = ram'}
    return w

setWord addr w = do
    st@State{ram} <- get
    let ram' = execState (writeWord addr w) ram
    put st{ram = ram'}

setAcc w = modify $ \st -> st{acc = w}

getAcc :: State (MachineState (IoMem (Isa w w) w) w) w
getAcc = do
    State{acc} <- get
    return acc

instance StateInterspector (MachineState (IoMem (Isa w w) w) w) (Isa w w) w Register where
    registers State{acc} =
        fromList
            [ (Acc, acc)
            ]
    memoryDump State{ram = IoMem{mIoCells}} = mIoCells
    ioStreams State{ram = IoMem{mIoStreams}} = mIoStreams

instance ViewState (MachineState (IoMem (Isa w w) w) w) where
    viewState State{} _ = error "not supported"

instance (MachineWord w) => Machine (MachineState (IoMem (Isa w w) w) w) (Isa w w) w where
    instructionFetch =
        get
            <&> ( \case
                    State{stopped = True} -> Nothing
                    State{pc, ram} -> do
                        let instruction = evalState (readInstruction pc) ram
                        Just (pc, instruction)
                )
    instructionStep = do
        (tmp :: Maybe (Int, Isa w w)) <- instructionFetch
        let (pc, instruction) = fromMaybe (error "Can't fetch instruction.") tmp
        case instruction of
            LoadImm a -> setAcc a >> nextPc
            LoadAddr a -> do
                value <- getWord $ fromEnum a
                setAcc value
                nextPc
            LoadRel a -> getWord (pc + fromEnum a) >>= setAcc >> nextPc
            LoadInd a -> do
                addr <- getWord $ fromEnum a
                value <- getWord $ fromEnum addr
                setAcc value
                nextPc
            StoreAddr a -> getAcc >>= setWord (fromEnum a) >> nextPc
            StoreRel a -> getAcc >>= setWord (fromEnum (pc + fromEnum a)) >> nextPc
            StoreInd a -> do
                addr <- getWord $ fromEnum a
                acc <- getAcc
                setWord (fromEnum addr) acc
                nextPc
            Add a -> withAcc (+) a
            Sub a -> withAcc (-) a
            Mul a -> withAcc (*) a
            Div a -> withAcc div a
            Rem a -> withAcc rem a
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
            Halt -> modify $ \st -> st{stopped = True}
        where
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
