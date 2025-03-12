{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Inspired by https://www.greenarraychips.com/home/documents/greg/DB001-221113-F18a.pdf
module Isa.F32a (
    Isa (..),
    MachineState (..),
) where

import Data.Bits (Bits (..), clearBit, complement, setBit, shiftL, shiftR, testBit, (.&.))
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

data Isa w l
    = -- | __;__ return
      Return
    | -- | __call__ to name
      Call l
    | -- | __lit REF__
      Lit l
    | -- | __next REF__ loop to address (decrement R)
      Next l
    | -- | __REF ;__
      Jump l
    | -- | __if__   If T is nonzero, continues with the next instruction word addressed by P. If T is zero, jumps
      If l
    | -- | __-if__  Minus-if. If T is negative (T[31] set), continues with the next instruction word addressed by P. If T is nonnegative, jumps
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
    | -- | __eam__  set arithmetic mode, pop T, if T == 0 then 0 else 1
      Eam
    | -- | __and__
      And
    | -- | __xor__  Exclusive Or. Replaces T with the Boolean XOR of S and T. Pops data stack
      Xor
    | -- | __drop__ Drop. Pops the data stack
      Drop
    | -- | __dup__  Dup. Duplicates T on the data stack
      Dup
    | -- | __r>__   Moves R into T, popping the return stack and pushing the data stack
      RIntoT
    | -- | __>r__   Moves T into R, pushing the return stack and popping the data stack
      TIntoR
    | -- | __over__
      Over
    | -- | __a__    Fetches the contents of register A into T, pushing the data stack
      AFetch
    | -- | __halt__
      Halt
    deriving (Show)

instance CommentStart (Isa w l) where
    commentStart = "\\"

instance (MachineWord w) => MnemonicParser (Isa w (Ref w)) where
    mnemonic =
        hspace *> cmd <* (hspace1 <|> eol' "\\")
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
                    , string "eam" >> return Eam
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
                    , string "r>" >> return RIntoT
                    , string ">r" >> return TIntoR
                    , string "halt" >> return Halt
                    , string ";" >> return Return
                    , try $ do
                        label <- reference
                        hspace1
                        void $ string ";"
                        return $ Jump label
                    , try $ do
                        label <- reference
                        hspace1 <|> eol' "\\"
                        return $ Call label
                    ]

instance DerefMnemonic (Isa w) w where
    derefMnemonic f _offset i =
        case i of
            Lit l -> Lit (deref' f l)
            Call l -> Call (deref' f l)
            Return -> Return
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
            RIntoT -> RIntoT
            TIntoR -> TIntoR
            MulStep -> MulStep
            DivStep -> DivStep
            LShift -> LShift
            RShift -> RShift
            Inv -> Inv
            Add -> Add
            Eam -> Eam
            And -> And
            Xor -> Xor
            Drop -> Drop
            Dup -> Dup
            Over -> Over
            Halt -> Halt

instance ByteLength (Isa w l) where
    -- NOTE: in f18a multiple instructions can be fitted in one machine word.
    -- Here we simplify it: args -- is just a muchine word. Opcode -- one byte.
    byteLength (Lit _) = 5
    byteLength (Call _) = 5
    byteLength (Jump _) = 5
    byteLength (Next _) = 5
    byteLength (If _) = 5
    byteLength (MinusIf _) = 5
    byteLength (FetchP _) = 5
    byteLength (StoreP _) = 5
    byteLength _ = 1

data MachineState mem w = State
    { p :: Int
    , a :: w
    , b :: w
    , ram :: mem
    , dataStack :: [w]
    , returnStack :: [w]
    , stopped :: Bool
    , extendedArithmeticMode :: Bool
    , carryFlag :: Bool
    , internalError :: Maybe Text
    }
    deriving (Show)

instance (MachineWord w) => InitState (IoMem (Isa w w) w) (MachineState (IoMem (Isa w w) w) w) where
    initState pc dump =
        State
            { p = pc
            , a = def
            , b = def
            , dataStack = []
            , returnStack = []
            , ram = dump
            , stopped = False
            , extendedArithmeticMode = False
            , carryFlag = False
            , internalError = Nothing
            }

setP :: forall w. Int -> State (MachineState (IoMem (Isa w w) w) w) ()
setP addr = modify $ \st -> st{p = addr}

getP :: State (MachineState (IoMem (Isa w w) w) w) Int
getP = get <&> (fromEnum . p)

nextP :: (MachineWord w) => State (MachineState (IoMem (Isa w w) w) w) ()
nextP = do
    instructionFetch >>= \case
        Right (p, instruction) -> setP (p + byteLength instruction)
        Left err -> raiseInternalError $ "nextPc: " <> err

raiseInternalError :: Text -> State (MachineState (IoMem (Isa w w) w) w) ()
raiseInternalError msg = modify $ \st -> st{internalError = Just msg}

getWord addr = do
    st@State{ram} <- get
    case readWord ram addr of
        Right (ram', w) -> do
            put st{ram = ram'}
            return w
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return def

setWord addr w = do
    st@State{ram} <- get
    case writeWord ram addr w of
        Right ram' -> put st{ram = ram'}
        Left err -> raiseInternalError $ "memory access error: " <> err

dataPush w = do
    setCarryFlag False
    st@State{dataStack} <- get
    put st{dataStack = w : dataStack}

dataPop :: (MachineWord w) => State (MachineState (IoMem (Isa w w) w) w) w
dataPop = do
    st@State{dataStack} <- get
    case dataStack of
        [] -> do
            raiseInternalError "empty data stack"
            return def
        (x : xs) -> do
            put st{dataStack = xs}
            return x

returnPush w = do
    st@State{returnStack} <- get
    put st{returnStack = w : returnStack}

returnPop :: (MachineWord w) => State (MachineState (IoMem (Isa w w) w) w) w
returnPop = do
    st@State{returnStack} <- get
    case returnStack of
        [] -> do
            raiseInternalError "empty return stack"
            return def
        (x : xs) -> do
            put st{returnStack = xs}
            return x

setExtendedArithmeticMode :: Bool -> State (MachineState (IoMem (Isa w w) w) w) ()
setExtendedArithmeticMode flag = modify $ \st -> st{extendedArithmeticMode = flag}

setCarryFlag :: Bool -> State (MachineState (IoMem (Isa w w) w) w) ()
setCarryFlag flag = modify $ \st -> st{carryFlag = flag}

getCarryFlag :: State (MachineState (IoMem (Isa w w) w) w) Bool
getCarryFlag = get <&> carryFlag

setA w = modify $ \st -> st{a = w}

setB w = modify $ \st -> st{b = w}

getA :: State (MachineState (IoMem (Isa w w) w) w) w
getA = do
    State{a} <- get
    return a

getB :: State (MachineState (IoMem (Isa w w) w) w) w
getB = get <&> b

instance (MachineWord w) => StateInterspector (MachineState (IoMem (Isa w w) w) w) (Isa w w) w where
    programCounter State{p} = p
    memoryDump State{ram = IoMem{mIoCells}} = mIoCells
    ioStreams State{ram = IoMem{mIoStreams}} = mIoStreams
    reprState labels st v
        | Just v' <- defaultView labels st v = v'
    reprState labels st@State{a, b, dataStack, returnStack, extendedArithmeticMode, carryFlag} v =
        case T.splitOn ":" v of
            ["EAM"] -> if extendedArithmeticMode then "1" else "0"
            ["C"] -> if carryFlag then "1" else "0"
            [r] -> reprState labels st (r <> ":dec")
            ["A", f] -> viewRegister f a
            ["B", f] -> viewRegister f b
            ["T", f] -> viewRegister f $ fromMaybe 0 $ dataStack !!? 0
            ["S", f] -> viewRegister f $ fromMaybe 0 $ dataStack !!? 1
            ["R", f] -> viewRegister f $ fromMaybe 0 $ returnStack !!? 0
            ["stack", f] -> stack f dataStack
            ["rstack", f] -> stack f returnStack
            [r, _] -> unknownView r
            _ -> errorView v
        where
            stack "dec" dt = toText $ intercalate ":" $ map show dt
            stack "hex" dt = T.intercalate ":" $ map (toText . word32ToHex) dt
            stack f _ = unknownFormat f

instance (MachineWord w) => Machine (MachineState (IoMem (Isa w w) w) w) (Isa w w) w where
    instructionFetch =
        get
            <&> ( \case
                    State{stopped = True} -> Left halted
                    State{internalError = Just err} -> Left err
                    State{p, ram} -> do
                        instruction <- readInstruction ram p
                        return (p, instruction)
                )
    instructionStep = do
        (_pc, instruction) :: (Int, Isa w w) <- either (error . ("Can't fetch instruction." <>)) id <$> instructionFetch
        case instruction of
            Lit l -> do
                dataPush l
                nextP
            Return -> returnPop >>= setP . fromEnum >> nextP
            Call l -> do
                getP >>= returnPush . toEnum
                setP (fromEnum l)
            Jump l -> do
                setP (fromEnum l)
            Next l -> do
                r <- returnPop
                if r == 0
                    then nextP
                    else do
                        returnPush (r - 1)
                        setP (fromEnum l)
            If l -> do
                w <- dataPop
                if w == 0
                    then setP (fromEnum l)
                    else nextP
            MinusIf l -> do
                w <- dataPop
                if w >= 0
                    then setP (fromEnum l)
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
            RIntoT -> returnPop >>= dataPush >> nextP
            TIntoR -> dataPop >>= returnPush >> nextP
            Add -> do
                t <- dataPop
                s <- dataPop
                State{extendedArithmeticMode, carryFlag} <- get
                let Ext{value = v1, carry = c1} = addExt s t
                    (result, carry) =
                        if extendedArithmeticMode && carryFlag
                            then
                                let Ext{value = v2, carry = c2} = addExt v1 1
                                 in (v2, c1 || c2)
                            else (v1, c1)
                dataPush result
                setCarryFlag carry
                nextP
            Eam -> do
                t <- dataPop
                setExtendedArithmeticMode (t /= 0)
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
                setCarryFlag False
                nextP
            Dup -> do
                carryFlag <- getCarryFlag
                w <- dataPop
                dataPush w
                dataPush w
                setCarryFlag carryFlag
                nextP
            Over -> do
                t <- dataPop
                s <- dataPop
                dataPush t
                dataPush s
                nextP
            Halt -> modify $ \st -> st{stopped = True}
