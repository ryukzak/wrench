{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- TODO: ; ex call

-- | Inspired by https://www.greenarraychips.com/home/documents/greg/DB001-221113-F18a.pdf
module Isa.F32a (
    Isa (..),
    MachineState (..),
    Register (..),
) where

import Data.Bits (Bits (..), clearBit, complement, setBit, shiftL, shiftR, testBit, (.&.))
import Machine.Memory
import Machine.Types
import Relude
import Text.Megaparsec (choice, try)
import Text.Megaparsec.Char (hspace, hspace1, string)
import Translator.Parser.Misc
import Translator.Parser.Types
import Translator.Types

data Register = T | S | A | B | P | R
    deriving (Show, Generic, Eq, Read)

instance Hashable Register

data Isa w l
    = -- | __lit REF__
      -- ; return
      -- ex execute (swap P and R)
      -- call to name
      Lit l
    | -- | __next REF__ loop to address (decrement R)
      Next l
    | -- | __REF ;__
      Jump l
    | -- | __if__   If T is nonzero, continues with the next instruction word addressed by P. If T is zero, jumps
      If l
    | -- | __-if__  Minus-if. If T is negative (T17 set), continues with the next instruction word addressed by P. If T is positive, jumps
      MinusIf l
    | -- | __a!__   A-Store. Stores T into register A, popping the data stack
      AStore
    | -- | __b!__   B-Store. Stores T into register B, popping the data stack.
      BStore
    | -- | __@p__   Pushes data stack, reads [P] into T, and increments P
      FetchP l
    | -- | __@+__   Fetch-plus. Pushes data stack, reads [A] into T, and increments A
      FetchPlus
    | -- | __@b__   Fetch-B. Pushes data stack and reads [B] into T
      FetchB
    | -- | __@__    Fetch. Pushes data stack and reads [A] into T.
      Fetch
    | -- | __!p__   Store-P. Writes T into [P], pops the data stack, and increments P
      StoreP l
    | -- | __!b__   Store-B. Writes T into [B] and pops the data stack
      StoreB
    | -- | __!+__   Store-plus. Writes T into [A], pops the data stack, and increments A
      StorePlus
    | -- | __!__    Writes T into [A] and pops the data stack
      Store
    | -- | __+*__   multiply step
      MulStep
    | -- | __+/__   divide step (details: example/step-by-step-div.py)
      DivStep
    | -- | __2*__   11 left shift
      LShift
    | -- | __2/__   right shift (signed)
      RShift
    | -- | __inv__  invert all bits (was ~)
      Inv
    | -- | __+__    add (or add with carry)
      Add
    | -- | __and__
      And
    | -- | __xor__  Exclusive Or. Replaces T with the Boolean XOR of S and T. Pops data stack
      Xor
    | -- | __drop__ Drop. Pops the data stack
      Drop
    | -- | __dup__  Dup. Duplicates T on the data stack
      Dup
    | -- | __>r__   Moves R into T, popping the return stack and pushing the data stack
      RPop
    | -- | __r>__   Moves T into R, pushing the return stack and popping the data stack
      RPush
    | -- | __over__
      Over
    | -- | __a__    Fetches the contents of register A into T, pushing the data stack
      -- . nop
      AFetch
    | -- | __halt__
      Halt
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
                    , Next <$> (string "next" *> hspace1 *> reference)
                    , If <$> (string "if" *> hspace1 *> reference)
                    , MinusIf <$> (string "-if" *> hspace1 *> reference)
                    , string "a!" >> return AStore
                    , string "b!" >> return BStore
                    , string "+*" >> return MulStep
                    , string "+/" >> return DivStep
                    , string "2*" >> return LShift
                    , string "2/" >> return RShift
                    , string "inv" >> return Inv
                    , string "+" >> return Add
                    , string "and" >> return And
                    , string "xor" >> return Xor
                    , string "drop" >> return Drop
                    , string "dup" >> return Dup
                    , string "over" >> return Over
                    , string "a" >> return AFetch
                    , FetchP <$> (string "@p" *> hspace1 *> reference)
                    , string "@+" >> return FetchPlus
                    , string "@b" >> return FetchB
                    , string "@" >> return Fetch
                    , StoreP <$> (string "!p" *> hspace1 *> reference)
                    , string "!b" >> return StoreB
                    , string "!+" >> return StorePlus
                    , string "!" >> return Store
                    , string "r>" >> return RPush
                    , string ">r" >> return RPop
                    , string "halt" >> return Halt
                    , try $ do
                        label <- reference
                        hspace1
                        void $ string ";"
                        return $ Jump label
                    ]

instance DerefMnemonic (Isa w) w where
    derefMnemonic f _offset i =
        case i of
            Lit l -> Lit (deref' f l)
            Jump l -> Jump (deref' f l)
            Next l -> Next (deref' f l)
            If l -> If (deref' f l)
            MinusIf l -> MinusIf (deref' f l)
            AStore -> AStore
            BStore -> BStore
            AFetch -> AFetch
            FetchP l -> FetchP (deref' f l)
            Fetch -> Fetch
            FetchPlus -> FetchPlus
            FetchB -> FetchB
            StoreP l -> StoreP (deref' f l)
            StoreB -> StoreB
            StorePlus -> StorePlus
            Store -> Store
            RPush -> RPush
            RPop -> RPop
            MulStep -> MulStep
            DivStep -> DivStep
            LShift -> LShift
            RShift -> RShift
            Inv -> Inv
            Add -> Add
            And -> And
            Xor -> Xor
            Drop -> Drop
            Dup -> Dup
            Over -> Over
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

nextP :: (MachineWord w) => State (MachineState (IoMem (Isa w w) w) w) ()
nextP = do
    (p, instruction) <- fromMaybe (error "internal error") <$> instructionFetch
    setPc (p + byteLength instruction)

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

returnPush w = do
    st@State{returnStack} <- get
    put st{returnStack = w : returnStack}

returnPop :: (Num w) => State (MachineState (IoMem (Isa w w) w) w) w
returnPop = do
    st@State{returnStack} <- get
    case returnStack of
        [] -> return 0
        (x : xs) -> do
            put st{returnStack = xs}
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
    registers State{a, b, dataStack, returnStack} =
        fromList
            [ (A, a)
            , (B, b)
            , (T, fromMaybe 0 $ dataStack !!? 0)
            , (S, fromMaybe 0 $ dataStack !!? 1)
            , (R, fromMaybe 0 $ returnStack !!? 0)
            ]
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
            Next l -> do
                r <- returnPop
                if r == 0
                    then nextP
                    else do
                        returnPush (r - 1)
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
            AFetch -> getA >>= dataPush >> nextP
            -- [    T   ][    A   ] >> 1
            --      ^            |
            --      |            |
            --      +?<----------+ lower bit
            --      ^
            --      |
            -- [    S   ]
            MulStep -> do
                a <- getA
                t <- dataPop
                s <- dataPop
                let t' = t + if testBit a 0 then s else 0
                    a' = a `shiftR` 1
                    t'' = t' `shiftR` 1
                    a'' = if testBit t' 0 then setBit a' 31 else clearBit a' 31
                dataPush s
                dataPush t''
                setA a''
                nextP
            -- A <- dividend
            -- [B] <- divisor
            -- T -> quotient
            -- S -> remainder
            DivStep -> do
                quotient1 <- dataPop
                remainder1 <- dataPop
                dividend1 <- getA
                divisor <- getB >>= getWord . fromEnum
                let remainder2 = remainder1 `shiftL` 1
                    dividenUpperBit = if testBit dividend1 31 then 1 else 0
                    dividend2 = dividend1 `shiftL` 1
                    remainder3 = remainder2 .|. dividenUpperBit
                    quotient2 = quotient1 `shiftL` 1
                    (remainder4, quotient3) =
                        if remainder3 >= divisor
                            then (remainder3 - divisor, quotient2 .|. 1)
                            else (remainder3, quotient2)
                setA dividend2
                dataPush remainder4
                dataPush quotient3
                nextP
            LShift -> do
                w <- dataPop
                dataPush (w `shiftL` 1)
                nextP
            RShift -> do
                w <- dataPop
                dataPush (w `shiftR` 1)
                nextP
            Inv -> do
                w <- dataPop
                dataPush (complement w)
                nextP
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
            FetchP l -> getWord (fromEnum l) >>= dataPush >> nextP
            Fetch -> do
                a <- getA
                w <- getWord $ fromEnum a
                dataPush w
                nextP
            StoreP l -> dataPop >>= setWord (fromEnum l) >> nextP
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
            RPop -> returnPop >>= dataPush >> nextP
            RPush -> dataPop >>= returnPush >> nextP
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
            Over -> do
                t <- dataPop
                s <- dataPop
                dataPush t
                dataPush s
                nextP
            Halt -> modify $ \st -> st{stopped = True}
