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
import Text.Megaparsec (choice, try)
import Text.Megaparsec.Char (hspace, hspace1, string)
import Translator.Parser.Misc
import Translator.Parser.Types
import Translator.Types

data Register = T | S | A | B | P
    deriving (Show, Generic, Eq, Read)

instance Hashable Register

data Isa w l
    = Lit l
    | Jump l
    | If l -- jump if T=0
    | MinusIf l -- jump if T>=0
    | AStore -- Stores T into register A, popping the data stack
    | BStore -- B-Store. Stores T into register B, popping the data stack.
    | FetchP l -- @p Pushes data stack, reads [P] into T, and increments P
    | FetchPlus -- @+ Fetch-plus. Pushes data stack, reads [A] into T, and increments A
    | FetchB -- @b Fetch-B. Pushes data stack and reads [B] into T
    | Fetch -- @ Fetch. Pushes data stack and reads [A] into T.
    | StoreP l -- !p Store-P. Writes T into [P], pops the data stack, and increments P
    | StoreB -- !b Store-B. Writes T into [B] and pops the data stack
    | StorePlus -- !+ Store-plus. Writes T into [A], pops the data stack, and increments A
    | Store -- ! Writes T into [A] and pops the data stack
    | Add
    | And
    | Xor -- Exclusive Or. Replaces T with the Boolean XOR of S and T. Pops data stack
    | Drop -- Drop. Pops the data stack
    | Dup -- Dup. Duplicates T on the data stack
    | Halt
    deriving (Show)

instance CommentStart (Isa w l) where
    commentStart = "\\"

instance (MachineWord w) => MnemonicParser (Isa w (Ref w)) where
    mnemonic =
        hspace *> cmd <* (hspace1 <|> eol' ";") -- FIXME: should be \
        where
            cmd =
                choice
                    [ Lit <$> (string "lit" *> hspace1 *> reference)
                    , If <$> (string "if" *> hspace1 *> reference)
                    , MinusIf <$> (string "-if" *> hspace1 *> reference)
                    , string "a!" >> return AStore
                    , string "b!" >> return BStore
                    , string "+" >> return Add
                    , string "and" >> return And
                    , string "xor" >> return Xor
                    , string "drop" >> return Drop
                    , string "dup" >> return Dup
                    , FetchP <$> (string "@p" *> hspace1 *> reference)
                    , string "@+" >> return FetchPlus
                    , string "@b" >> return FetchB
                    , string "@" >> return Fetch
                    , StoreP <$> (string "!p" *> hspace1 *> reference)
                    , string "!b" >> return StoreB
                    , string "!+" >> return StorePlus
                    , string "!" >> return Store
                    , string "halt" >> return Halt
                    , try $ do
                        label <- reference
                        hspace1
                        void $ string ";"
                        return $ Jump label
                    ]

instance (MachineWord w) => DerefMnemonic (Isa w) w where
    derefMnemonic f _offset i =
        case i of
            Lit l -> Lit (deref' f l)
            Jump l -> Jump (deref' f l)
            If l -> If (deref' f l)
            MinusIf l -> MinusIf (deref' f l)
            AStore -> AStore
            BStore -> BStore
            FetchP l -> FetchP (deref' f l)
            Fetch -> Fetch
            FetchPlus -> FetchPlus
            FetchB -> FetchB
            StoreP l -> StoreP (deref' f l)
            StoreB -> StoreB
            StorePlus -> StorePlus
            Store -> Store
            Add -> Add
            And -> And
            Xor -> Xor
            Drop -> Drop
            Dup -> Dup
            Halt -> Halt

-- FIXME: make instruction size more real
instance ByteLength (Isa w l) where
    byteLength (FetchP _) = 8
    byteLength (StoreP _) = 8
    byteLength _ = 4

data MachineState mem w = State
    { p :: Int
    , a :: w
    , b :: w
    , ram :: mem
    , dataStack :: [w]
    , returnStack :: [w]
    , stopped :: Bool
    }
    deriving (Show)

instance (MachineWord w) => InitState (IoMem (Isa w w) w) (MachineState (IoMem (Isa w w) w) w) where
    initState pc dump = State{p = pc, a = 0, b = 0, dataStack = [], returnStack = [], ram = dump, stopped = False}

setPc :: forall w. Int -> State (MachineState (IoMem (Isa w w) w) w) ()
setPc addr = modify $ \st -> st{p = addr}

getP :: State (MachineState (IoMem (Isa w w) w) w) Int
getP = get <&> p

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

setB w = modify $ \st -> st{b = w}

getA :: State (MachineState (IoMem (Isa w w) w) w) w
getA = do
    State{a} <- get
    return a

getB :: State (MachineState (IoMem (Isa w w) w) w) w
getB = get <&> b

instance (Num w) => StateInterspector (MachineState (IoMem (Isa w w) w) w) (Isa w w) w Register where
    registers State{a, b, dataStack = []} = fromList [(A, a), (B, b), (T, 0), (S, 0)]
    registers State{a, b, dataStack = [t]} = fromList [(A, a), (B, b), (T, t), (S, 0)]
    registers State{a, b, dataStack = (t : s : _)} = fromList [(A, a), (B, b), (T, t), (S, s)]
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
            Lit l -> do
                dataPush l
                nextP
            Jump l -> do
                setPc (fromEnum l)
            If l -> do
                w <- dataPop
                if w == 0
                    then setPc (fromEnum l)
                    else nextP
            MinusIf l -> do
                w <- dataPop
                if w >= 0
                    then setPc (fromEnum l)
                    else nextP
            AStore -> dataPop >>= setA >> nextP
            BStore -> dataPop >>= setB >> nextP
            FetchPlus -> do
                a <- getA
                w <- getWord $ fromEnum a
                dataPush w
                setA (a + 1)
                nextP
            FetchB -> do
                b <- getB
                w <- getWord $ fromEnum b
                dataPush w
                nextP
            FetchP l -> do
                -- FIXME: and it is not word, word we need to read from memory
                w <- getWord $ fromEnum l -- actually it is p
                dataPush w
                nextP
                nextP
            Fetch -> do
                a <- getA
                w <- getWord $ fromEnum a
                dataPush w
                nextP
            StoreP l -> do
                -- FIXME: and it is not word, word we need to read from memory
                w <- dataPop
                setWord (fromEnum l) w
                nextP
                nextP
            StorePlus -> do
                w <- dataPop
                a <- getA
                setWord (fromEnum a) w
                setA (a + 1)
                nextP
            StoreB -> do
                w <- dataPop
                b <- getB
                setWord (fromEnum b) w
                nextP
            Store -> do
                a <- getA
                w <- dataPop
                setWord (fromEnum a) w
                nextP
            Add -> do
                t <- dataPop
                s <- dataPop
                dataPush (s + t)
                nextP
            And -> do
                t <- dataPop
                s <- dataPop
                dataPush (s .&. t)
                nextP
            Xor -> do
                t <- dataPop
                s <- dataPop
                dataPush (s `xor` t)
                nextP
            Drop -> do
                void dataPop
                nextP
            Dup -> do
                w <- dataPop
                dataPush w
                dataPush w
                nextP
            Halt -> modify $ \st -> st{stopped = True}
