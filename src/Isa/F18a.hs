{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- \* Inspired by https://www.greenarraychips.com/home/documents/greg/DB001-221113-F18A.pdf
module Isa.F18a (
    Isa (..),
    MachineState (..),
    Register (..),
) where

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

data Register = T | S | A | P
    deriving (Show, Generic, Eq, Read)

instance Hashable Register

data Isa w l
    = -- | Pushes data stack, reads [P] into T, and increments P
      FetchP l
    | -- | Stores T into register A, popping the data stack
      AStore
    | -- | Pushes data stack and reads [A] into T.
      Fetch
    | -- | Writes T into [A] and pops the data stack
      Store
    | -- | Exclusive Or. Replaces T with the Boolean XOR of S and T. Pops data stack
      Xor
    | Halt
    deriving (Show)

instance (MachineWord w) => MnemonicParser (Isa w (Ref w)) where
    mnemonic =
        hspace *> cmd <* (hspace1 <|> eol')
        where
            cmd =
                choice
                    [ FetchP <$> (string "@p" *> hspace1 *> reference)
                    , string "a!" >> return AStore
                    , string "xor" >> return Xor
                    , string "@" >> return Fetch
                    , string "!" >> return Store
                    , string "halt" >> return Halt
                    ]

instance (MachineWord w) => DerefMnemonic (Isa w) w where
    derefMnemonic f _offset i =
        case i of
            FetchP l -> FetchP (deref' f l)
            AStore -> AStore
            Fetch -> Fetch
            Store -> Store
            Xor -> Xor
            Halt -> Halt

-- FIXME: make instruction size more real
instance ByteLength (Isa w l) where
    byteLength (FetchP _) = 8
    byteLength _ = 4

data MachineState mem w = State
    { p :: Int
    , a :: w
    , ram :: mem
    , dataStack :: [w]
    , returnStack :: [w]
    , stopped :: Bool
    }
    deriving (Show)

instance (MachineWord w) => InitState (IoMem (Isa w w) w) (MachineState (IoMem (Isa w w) w) w) where
    initState pc dump = State{p = pc, a = 0, dataStack = [], returnStack = [], ram = dump, stopped = False}

setPc :: forall w. Int -> State (MachineState (IoMem (Isa w w) w) w) ()
setPc addr = modify $ \st -> st{p = addr}

-- FIXME: not a constant, depends on current instruction
nextP :: forall w. (ByteLength w, Default w) => State (MachineState (IoMem (Isa w w) w) w) ()
nextP = do
    State{p} <- get
    setPc (p + byteLength (def :: w))

getWord addr = do
    st@State{ram} <- get
    let (w, ram') = runState (readWord addr) ram
    put st{ram = ram'}
    return w

setWord addr w = do
    st@State{ram} <- get
    let ram' = execState (writeWord addr w) ram
    put st{ram = ram'}

dataPush w = do
    st@State{dataStack} <- get
    put st{dataStack = w : dataStack}

dataPop :: (Num w) => State (MachineState (IoMem (Isa w w) w) w) w
dataPop = do
    st@State{dataStack} <- get
    case dataStack of
        [] -> return 0
        (x : xs) -> do
            put st{dataStack = xs}
            return x

setA w = modify $ \st -> st{a = w}

getA :: State (MachineState (IoMem (Isa w w) w) w) w
getA = do
    State{a} <- get
    return a

instance (Num w) => StateInterspector (MachineState (IoMem (Isa w w) w) w) (Isa w w) w Register where
    registers State{a, dataStack = []} = fromList [(A, a), (T, 0), (S, 0)]
    registers State{a, dataStack = [t]} = fromList [(A, a), (T, t), (S, 0)]
    registers State{a, dataStack = (t : s : _)} = fromList [(A, a), (T, t), (S, s)]
    memoryDump State{ram = IoMem{mIoCells}} = mIoCells
    ioStreams State{ram = IoMem{mIoStreams}} = mIoStreams

instance (MachineWord w) => Machine (MachineState (IoMem (Isa w w) w) w) (Isa w w) w where
    instructionFetch =
        get
            <&> ( \case
                    State{stopped = True} -> Nothing
                    State{p, ram} -> do
                        let instruction = evalState (readInstruction p) ram
                        Just (p, instruction)
                )
    instructionStep = do
        (tmp :: Maybe (Int, Isa w w)) <- instructionFetch
        let (_pc, instruction) = fromMaybe (error "Can't fetch instruction.") tmp
        case instruction of
            FetchP l -> do
                -- FIXME: and it is not word, word we need to read from memory
                w <- getWord $ fromEnum l -- actually it is p
                dataPush w
                nextP
                nextP
            AStore -> do
                w <- dataPop
                setA w
                nextP
            Fetch -> do
                a <- getA
                w <- getWord $ fromEnum a
                dataPush w
                nextP
            Store -> do
                a <- getA
                w <- dataPop
                setWord (fromEnum a) w
                nextP
            Xor -> do
                t <- dataPop
                s <- dataPop
                dataPush (s `xor` t)
                nextP
            Halt -> modify $ \st -> st{stopped = True}
