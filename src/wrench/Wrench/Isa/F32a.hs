{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Inspired by https://www.greenarraychips.com/home/documents/greg/DB001-221113-F18a.pdf
module Wrench.Isa.F32a (
    F32aIsa (..),
    F32aSt (..),
    f32aMnemonics,
) where

import Data.Bits (Bits (..), clearBit, complement, setBit, shiftL, shiftR, testBit, (.&.))
import Data.Default (def)
import Data.Text qualified as T
import Relude
import Text.Megaparsec (anySingleBut, choice, getSourcePos, try)
import Text.Megaparsec.Char (char, hspace, hspace1, string)
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Machine.Word
import Wrench.Report
import Wrench.Report.Keyword (flagKey, valueKey)
import Wrench.Translator.Parser.Misc
import Wrench.Translator.Parser.Types
import Wrench.Translator.Types

data F32aIsa w l
    = -- | > ; ( -- )
      --
      -- Return: pops the return stack into P.
      Return
    | -- | > <address: i32> ( -- )
      --
      -- Call, written as a bare label: pushes P onto the return stack and
      -- jumps.
      Call l
    | -- | > lit <value: i32> ( -- value: i32 )
      --
      -- Push a literal. A bare number is the same instruction.
      Lit l
    | -- | > next <address: i32> ( -- )
      --
      -- Loop: pops R; falls through when it was @0@, otherwise pushes
      -- @R-1@ back and jumps.
      Next l
    | -- | > <address: i32> ; ( -- )
      --
      -- Jump, written as a label followed by @;@.
      Jump l
    | -- | > if <address: i32> ( flag: i32 -- )
      --
      -- Pops T: continues with the next instruction word if it was
      -- non-zero, jumps if it was zero.
      If l
    | -- | > -if <address: i32> ( value: i32 -- )
      --
      -- Minus-if. Pops T: continues with the next instruction word if it
      -- was negative (T[31] set), jumps if it was non-negative.
      MinusIf l
    | -- | > a! ( value: i32 -- )
      --
      -- A-Store. Stores T into register A, popping the data stack.
      AStore
    | -- | > b! ( value: i32 -- )
      --
      -- B-Store. Stores T into register B, popping the data stack.
      BStore
    | -- | > @p <address: i32> ( -- value: i32 )
      --
      -- Pushes the data stack, reads [P] into T, and increments P.
      FetchP l
    | -- | > @+ ( -- value: i32 )
      --
      -- Fetch-plus. Pushes the data stack, reads [A] into T, and
      -- increments A.
      FetchPlus
    | -- | > @b ( -- value: i32 )
      --
      -- Fetch-B. Pushes the data stack and reads [B] into T.
      FetchB
    | -- | > @ ( -- value: i32 )
      --
      -- Fetch. Pushes the data stack and reads [A] into T.
      Fetch
    | -- | > !p <address: i32> ( value: i32 -- )
      --
      -- Store-P. Writes T into [P], pops the data stack, and increments P.
      StoreP l
    | -- | > !b ( value: i32 -- )
      --
      -- Store-B. Writes T into [B] and pops the data stack.
      StoreB
    | -- | > !+ ( value: i32 -- )
      --
      -- Store-plus. Writes T into [A], pops the data stack, and increments
      -- A.
      StorePlus
    | -- | > ! ( value: i32 -- )
      --
      -- Writes T into [A] and pops the data stack.
      Store
    | -- | > +* ( s: i32, t: i32 -- s: i32, t: i32 )
      --
      -- Multiply step. Leaves S alone, replaces T, and shifts A: one
      -- iteration of a shift-and-add multiply.
      MulStep
    | -- | > +/ ( remainder: i32, quotient: i32 -- remainder: i32, quotient: i32 )
      --
      -- Divide step: A holds the dividend, [B] the divisor, T the quotient
      -- so far and S the remainder (details:
      -- example\/step-by-step-div.py).
      DivStep
    | -- | > 2* ( value: i32 -- shifted: i32 )
      --
      -- Left shift.
      LShift
    | -- | > 2/ ( value: i32 -- shifted: i32 )
      --
      -- Right shift (signed).
      RShift
    | -- | > inv ( value: i32 -- inverted: i32 )
      --
      -- Invert all bits (was @~@).
      Inv
    | -- | > + ( s: i32, t: i32 -- sum: i32 )
      --
      -- Add, or add with carry in extended arithmetic mode.
      Add
    | -- | > eam ( flag: i32 -- )
      --
      -- Set arithmetic mode: pops T, extended iff it was non-zero.
      Eam
    | -- | > and ( s: i32, t: i32 -- bits: i32 )
      And
    | -- | > xor ( s: i32, t: i32 -- bits: i32 )
      --
      -- Exclusive Or. Replaces T with the Boolean XOR of S and T. Pops the
      -- data stack.
      Xor
    | -- | > drop ( value: i32 -- )
      --
      -- Drop. Pops the data stack.
      Drop
    | -- | > dup ( value: i32 -- value: i32, value: i32 )
      --
      -- Dup. Duplicates T on the data stack.
      Dup
    | -- | > r> ( -- value: i32 )
      --
      -- Moves R into T, popping the return stack and pushing the data
      -- stack.
      RIntoT
    | -- | > >r ( value: i32 -- )
      --
      -- Moves T into R, pushing the return stack and popping the data
      -- stack.
      TIntoR
    | -- | > over ( s: i32, t: i32 -- s: i32, t: i32, s: i32 )
      --
      -- Copies S over the top of the data stack.
      Over
    | -- | > a ( -- value: i32 )
      --
      -- Fetches the contents of register A into T, pushing the data stack.
      AFetch
    | -- | > halt ( -- )
      Halt
    deriving (Show)

-- | Every word the 'MnemonicParser' instance accepts as an instruction.
f32aMnemonics :: [Text]
f32aMnemonics =
    [ "lit"
    , "next"
    , "if"
    , "-if"
    , "a!"
    , "b!"
    , "+*"
    , "+/"
    , "2*"
    , "2/"
    , "inv"
    , "+"
    , "eam"
    , "and"
    , "xor"
    , "drop"
    , "dup"
    , "over"
    , "a"
    , "@p"
    , "@+"
    , "@b"
    , "@"
    , "!p"
    , "!b"
    , "!+"
    , "!"
    , "r>"
    , ">r"
    , "halt"
    , ";"
    ]

instance CommentStart (F32aIsa w l) where
    commentStart = "\\"

instance (IsWord w) => MnemonicParser (F32aIsa w (Ref w)) where
    mnemonic =
        choice
            [ Lit <$> cmdMnemonic1 "lit"
            , Next <$> cmdMnemonic1 "next"
            , If <$> cmdMnemonic1 "if"
            , MinusIf <$> cmdMnemonic1 "-if"
            , cmdMnemonic0 "a!" >> return AStore
            , cmdMnemonic0 "b!" >> return BStore
            , cmdMnemonic0 "+*" >> return MulStep
            , cmdMnemonic0 "+/" >> return DivStep
            , cmdMnemonic0 "2*" >> return LShift
            , cmdMnemonic0 "2/" >> return RShift
            , cmdMnemonic0 "inv" >> return Inv
            , cmdMnemonic0 "+" >> return Add
            , cmdMnemonic0 "eam" >> return Eam
            , cmdMnemonic0 "and" >> return And
            , cmdMnemonic0 "xor" >> return Xor
            , cmdMnemonic0 "drop" >> return Drop
            , cmdMnemonic0 "dup" >> return Dup
            , cmdMnemonic0 "over" >> return Over
            , cmdMnemonic0 "a" >> return AFetch
            , FetchP <$> cmdMnemonic1 "@p"
            , cmdMnemonic0 "@+" >> return FetchPlus
            , cmdMnemonic0 "@b" >> return FetchB
            , cmdMnemonic0 "@" >> return Fetch
            , StoreP <$> cmdMnemonic1 "!p"
            , cmdMnemonic0 "!b" >> return StoreB
            , cmdMnemonic0 "!+" >> return StorePlus
            , cmdMnemonic0 "!" >> return Store
            , cmdMnemonic0 "r>" >> return RIntoT
            , cmdMnemonic0 ">r" >> return TIntoR
            , cmdMnemonic0 "halt" >> return Halt
            , cmdMnemonic0 ";" >> return Return
            , try $ do
                label <- reference
                hspace1
                void $ string ";"
                return $ Jump label
            , Lit <$> bareLiteral
            , try $ do
                label <- reference
                hspace1 <|> eol' "\\"
                return $ Call label
            ]

bareLiteral :: (IsWord w) => Parser (Ref w)
bareLiteral = try $ do
    hspace
    pos <- getSourcePos
    ref <-
        choice
            [ do
                void $ char '\''
                c <- anySingleBut '\''
                void $ char '\''
                return $ ValueR id pos $ fromIntegral $ ord c
            , ValueR id pos <$> wordLiteral
            ]
    hspace1 <|> eol' "\\"
    return ref

cmdMnemonic0 :: String -> Parser ()
cmdMnemonic0 mnemonic = try $ do
    hspace
    void (string mnemonic)
    hspace1 <|> eol' "\\"

cmdMnemonic1 :: (IsWord w) => String -> Parser (Ref w)
cmdMnemonic1 mnemonic = try $ do
    void hspace
    void (string mnemonic)
    hspace1
    ref <- reference
    hspace1 <|> eol' "\\"
    return ref

instance DerefMnemonic (F32aIsa w) w where
    derefMnemonic f _offset i =
        case i of
            Lit l -> Lit (resolveRef Right f l)
            Call l -> Call (resolveRef Right f l)
            Return -> Return
            Jump l -> Jump (resolveRef Right f l)
            Next l -> Next (resolveRef Right f l)
            If l -> If (resolveRef Right f l)
            MinusIf l -> MinusIf (resolveRef Right f l)
            AStore -> AStore
            BStore -> BStore
            AFetch -> AFetch
            FetchP l -> FetchP (resolveRef Right f l)
            Fetch -> Fetch
            FetchPlus -> FetchPlus
            FetchB -> FetchB
            StoreP l -> StoreP (resolveRef Right f l)
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

instance ByteSize (F32aIsa w l) where
    -- NOTE: in f18a multiple instructions can be fitted in one machine word.
    -- Here we simplify it: args -- is just a muchine word. Opcode -- one byte.
    byteSize (Lit _) = 5
    byteSize (Call _) = 5
    byteSize (Jump _) = 5
    byteSize (Next _) = 5
    byteSize (If _) = 5
    byteSize (MinusIf _) = 5
    byteSize (FetchP _) = 5
    byteSize (StoreP _) = 5
    byteSize _ = 1

data F32aSt w = F32aSt
    { p :: Int
    , a :: w
    , b :: w
    , ram :: IoMem (F32aIsa w w) w
    , dataStack :: [w]
    , returnStack :: [w]
    , dataStackMax :: !Int
    -- ^ High-water mark of the data stack depth over the run (for f32a:data-stack-max).
    , returnStackMax :: !Int
    -- ^ High-water mark of the return stack depth over the run (for f32a:return-stack-max).
    , stopped :: Bool
    , extendedArithmeticMode :: Bool
    , carryFlag :: Bool
    , internalError :: Maybe Text
    }
    deriving (Show)

type instance MemOf (F32aSt w) = IoMem (F32aIsa w w) w

instance (IsWord w) => InitState (F32aSt w) where
    initState pc dump _randomStream =
        F32aSt
            { p = pc
            , a = def
            , b = def
            , dataStack = []
            , returnStack = []
            , dataStackMax = 0
            , returnStackMax = 0
            , ram = dump
            , stopped = False
            , extendedArithmeticMode = False
            , carryFlag = False
            , internalError = Nothing
            }

setP :: forall w. Int -> State (F32aSt w) ()
setP addr = modify $ \st -> st{p = addr}

getP :: State (F32aSt w) Int
getP = get <&> (fromEnum . p)

nextP :: (IsWord w) => State (F32aSt w) ()
nextP = do
    instructionFetch >>= \case
        Right (p, instruction) -> setP (p + byteSize instruction)
        Left err -> raiseInternalError $ "nextPc: " <> err

raiseInternalError :: Text -> State (F32aSt w) ()
raiseInternalError msg = modify $ \st -> st{internalError = Just msg}

getWord addr = do
    st@F32aSt{ram} <- get
    case readWord ram addr of
        Right (ram', w) -> do
            put st{ram = ram'}
            return w
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return def

setWord addr w = do
    st@F32aSt{ram} <- get
    case writeWord ram addr w of
        Right ram' -> put st{ram = ram'}
        Left err -> raiseInternalError $ "memory access error: " <> err

dataPush w = do
    setCarryFlag False
    st@F32aSt{dataStack, dataStackMax} <- get
    let dataStack' = w : dataStack
    put st{dataStack = dataStack', dataStackMax = max dataStackMax (length dataStack')}

dataPop :: (IsWord w) => State (F32aSt w) w
dataPop = do
    st@F32aSt{dataStack} <- get
    case dataStack of
        [] -> do
            raiseInternalError "empty data stack"
            return def
        (x : xs) -> do
            put st{dataStack = xs}
            return x

returnPush w = do
    st@F32aSt{returnStack, returnStackMax} <- get
    let returnStack' = w : returnStack
    put st{returnStack = returnStack', returnStackMax = max returnStackMax (length returnStack')}

returnPop :: (IsWord w) => State (F32aSt w) w
returnPop = do
    st@F32aSt{returnStack} <- get
    case returnStack of
        [] -> do
            raiseInternalError "empty return stack"
            return def
        (x : xs) -> do
            put st{returnStack = xs}
            return x

setExtendedArithmeticMode :: Bool -> State (F32aSt w) ()
setExtendedArithmeticMode flag = modify $ \st -> st{extendedArithmeticMode = flag}

setCarryFlag :: Bool -> State (F32aSt w) ()
setCarryFlag flag = modify $ \st -> st{carryFlag = flag}

getCarryFlag :: State (F32aSt w) Bool
getCarryFlag = get <&> carryFlag

setA w = modify $ \st -> st{a = w}

setB w = modify $ \st -> st{b = w}

getA :: State (F32aSt w) w
getA = do
    F32aSt{a} <- get
    return a

getB :: State (F32aSt w) w
getB = get <&> b

instance (IsWord w) => Inspectable (F32aSt w) where
    type WordOf (F32aSt w) = w
    type IsaOf (F32aSt w) = F32aIsa w w

    programCounter F32aSt{p} = p
    memoryDump F32aSt{ram} = ram
    ioStreams F32aSt{ram = IoMem{mIoStreams}} = mIoStreams
    isHalted F32aSt{stopped} = stopped
    reprState labels st v
        | Just v' <- defaultView labels st v = v'
    reprState _labels F32aSt{a, b, dataStack, returnStack, extendedArithmeticMode, carryFlag} v =
        case T.splitOn ":" v of
            ["EAM"] -> if extendedArithmeticMode then "1" else "0"
            ["C"] -> if carryFlag then "1" else "0"
            ["A", f] -> viewRegister f a
            ["B", f] -> viewRegister f b
            ["T", f] -> viewRegister f $ fromMaybe 0 $ dataStack !!? 0
            ["S", f] -> viewRegister f $ fromMaybe 0 $ dataStack !!? 1
            ["R", f] -> viewRegister f $ fromMaybe 0 $ returnStack !!? 0
            ["stack", f] -> stack f dataStack
            ["rstack", f] -> stack f returnStack
            _ -> errorView v
        where
            stack "dec" dt = toText $ intercalate ":" $ map show dt
            stack "hex" dt = T.intercalate ":" $ map (toText . word32ToHex) dt
            stack f _ = unknownFormat f

    viewKeywords _ =
        map flagKey ["EAM", "C", "f32a:data-stack-max", "f32a:return-stack-max"]
            <> map valueKey ["A", "B", "T", "S", "R", "stack", "rstack"]

    summaryView _labels F32aSt{dataStackMax, returnStackMax} v = case T.splitOn ":" v of
        ["f32a", "data-stack-max"] -> Just $ show dataStackMax
        ["f32a", "return-stack-max"] -> Just $ show returnStackMax
        ["isa-specific"] ->
            Just $
                "f32a:data-stack-max:   "
                    <> show dataStackMax
                    <> "\n"
                    <> "f32a:return-stack-max: "
                    <> show returnStackMax
        _ -> Nothing

instance (IsWord w) => Machine (F32aSt w) (F32aIsa w w) w where
    instructionFetch = do
        st <- get
        case st of
            F32aSt{stopped = True} -> return $ Left halted
            F32aSt{internalError = Just err} -> return $ Left err
            F32aSt{p, ram} ->
                case readInstruction ram p of
                    Left err -> return $ Left err
                    Right (ram', instruction) -> do
                        put st{ram = ram'}
                        return $ Right (p, instruction)
    instructionExecute _pc instruction =
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
                F32aSt{extendedArithmeticMode, carryFlag} <- get
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
                dataPush s
                dataPush t
                dataPush s
                nextP
            Halt -> modify $ \st -> st{stopped = True}
