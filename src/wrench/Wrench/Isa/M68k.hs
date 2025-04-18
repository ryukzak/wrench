{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Wrench.Isa.M68k (
    Isa (..),
    MachineState (..),
) where

import Data.Bits (Bits (..), complement, shiftL, shiftR, (.&.), (.|.))
import Data.Default (Default, def)
import Data.Text qualified as T
import Relude
import Relude.Extra
import Relude.Unsafe qualified as Unsafe
import Text.Megaparsec (choice, lookAhead, oneOf, try)
import Text.Megaparsec.Char (hspace, hspace1, string)
import Text.Megaparsec.Debug (dbg, dbg')
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Report
import Wrench.Translator.Parser.Misc
import Wrench.Translator.Parser.Types
import Wrench.Translator.Types

data Mode
    = -- | 32-bit mode
      Long
    deriving (Show)

data DataReg = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7
    deriving (Eq, Generic, Hashable, Read, Show)

instance (Default w) => Default (HashMap DataReg w) where
    def = fromList $ map (,def) [D0, D1, D2, D3, D4, D5, D6, D7]

data AddrReg = A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7
    deriving (Eq, Generic, Hashable, Read, Show)

instance (Default w) => Default (HashMap AddrReg w) where
    def = fromList $ map (,def) [A0, A1, A2, A3, A4, A5, A6, A7]

data Argument w l
    = DirectDataReg DataReg
    | DirectAddrReg AddrReg
    | IndirectAddrReg AddrReg
    | Immediate l
    deriving (Show)

-- | The 'Isa' type represents the instruction set architecture for the M68k machine.
-- Each constructor corresponds to a specific instruction.
data Isa w l
    = Move {mode :: Mode, src, dst :: Argument w l}
    -- | MoveA
    | Not {mode :: Mode, dst :: Argument w l}
    | Halt
    deriving (Show)

instance CommentStart (Isa w l) where
    commentStart = ";"

instance (MachineWord w) => MnemonicParser (Isa w (Ref w)) where
    mnemonic =
        choice
            [ cmd2args "move" Move
            , cmd1args "not" Not
            , cmd0args "halt" Halt
            ]

cmd0args :: String -> Isa w (Ref w) -> Parser (Isa w (Ref w))
cmd0args mnemonic constructor = try $ do
    void $ string mnemonic
    eol' ";"
    return constructor

cmd1args :: (MachineWord w) => String -> (Mode -> Argument w (Ref w) -> Isa w (Ref w)) -> Parser (Isa w (Ref w))
cmd1args mnemonic constructor = try $ do
    void $ string mnemonic
    m <- cmdMode
    hspace1
    a <- argument
    eol' ";"
    return $ constructor m a

cmd2args ::
    (MachineWord w) =>
    String -> (Mode -> Argument w (Ref w) -> Argument w (Ref w) -> Isa w (Ref w)) -> Parser (Isa w (Ref w))
cmd2args mnemonic constructor = try $ do
    void $ string mnemonic
    m <- cmdMode
    hspace1
    a <- argument
    comma
    b <- argument
    eol' ";"
    return $ constructor m a b

cmdMode = void (string ".l") >> return Long

comma :: Parser ()
comma = hspace >> void (string ",") >> hspace

dataRegister = try $ do
    void (string "D")
    n <- oneOf ['0' .. '7']
    -- lookAhead (comma <|> hspace1 <|> eol' ";")
    return $ DirectDataReg $ Unsafe.read ['D', n]

addrRegister = try $ do
    void (string "A")
    n <- oneOf ['0' .. '7']
    -- lookAhead (comma <|> hspace1 <|> eol' ";")
    return $ DirectAddrReg $ Unsafe.read ['A', n]

indirectAddrRegister = try $ do
    void (string "(")
    hspace
    void (string "A")
    n <- oneOf ['0' .. '7']
    hspace
    void (string ")")
    -- lookAhead (comma <|> hspace1 <|> eol' ";")
    return $ IndirectAddrReg $ Unsafe.read ['A', n]

immidiate :: (MachineWord w) => Parser (Argument w (Ref w))
immidiate = Immediate <$> reference

argument :: (MachineWord w) => Parser (Argument w (Ref w))
argument = choice [dataRegister, addrRegister, indirectAddrRegister, immidiate]

instance DerefMnemonic (Isa w) w where
    derefMnemonic f _offset i =
        let derefArg (DirectDataReg r) = DirectDataReg r
            derefArg (DirectAddrReg r) = DirectAddrReg r
            derefArg (IndirectAddrReg r) = IndirectAddrReg r
            derefArg (Immediate l) = Immediate $ deref' f l
         in case i of
                Move{mode, src, dst} -> Move mode (derefArg src) (derefArg dst)
                Not{mode, dst} -> Not mode (derefArg dst)
                Halt -> Halt

instance ByteLength (Isa w l) where
    byteLength _ = 4 -- Simplified assumption: all instructions are 2 bytes.

data MachineState mem w = State
    { pc :: Int
    , dr :: HashMap DataReg w
    , ar :: HashMap AddrReg w
    , mem :: mem
    , stopped :: Bool
    , internalError :: Maybe Text
    }
    deriving (Show)

setPc :: forall w. Int -> State (MachineState (IoMem (Isa w w) w) w) ()
setPc addr = modify $ \st -> st{pc = addr}

nextPc :: (MachineWord w) => State (MachineState (IoMem (Isa w w) w) w) ()
nextPc = do
    instructionFetch >>= \case
        Right (pc, instruction) -> setPc (pc + byteLength instruction)
        Left err -> raiseInternalError $ "nextPc: " <> err

raiseInternalError :: Text -> State (MachineState (IoMem (Isa w w) w) w) ()
raiseInternalError msg = modify $ \st -> st{internalError = Just msg}

instance (MachineWord w) => InitState (IoMem (Isa w w) w) (MachineState (IoMem (Isa w w) w) w) where
    initState pc dump =
        State
            { pc
            , dr = def
            , ar = def
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
    reprState labels st@State{ar, dr} v =
        errorView v

fetch :: (MachineWord w) => Mode -> Argument w w -> State (MachineState (IoMem (Isa w w) w) w) w
fetch _ (DirectDataReg r) = do
    State{dr} <- get
    return $ fromMaybe (error "Invalid register") (dr !? r)
fetch _ (IndirectAddrReg r) = do
    st@State{ar, mem} <- get
    let addr = maybe (error "Invalid register") fromEnum (ar !? r)
    case readWord mem addr of
        Right (mem', w) -> do
            put st{mem = mem'}
            return w
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return def
fetch _ (Immediate v) = return v
fetch _ _ = raiseInternalError "internal error" >> return def

store :: (MachineWord w) => Mode -> Argument w w -> w -> State (MachineState (IoMem (Isa w w) w) w) ()
store _ (DirectDataReg r) v = modify $ \st@State{dr} -> st{dr = insert r v dr}
store _ (DirectAddrReg r) v = modify $ \st@State{ar} -> st{ar = insert r v ar}
store _ (IndirectAddrReg r) v = do
    st@State{ar, mem} <- get
    let addr = maybe (error "Invalid register") fromEnum (ar !? r)
    case writeWord mem addr v of
        Right mem' -> do
            put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err
store _ _ _ = raiseInternalError "internal error"

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
            Move{mode, src, dst} -> do
                v <- fetch mode src
                store mode dst v
                nextPc
            Not{mode, dst} -> do
                v <- fetch mode dst
                store mode dst $ complement v
                nextPc
            Halt -> modify $ \st -> st{stopped = True}
