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

data DR = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7
    deriving (Eq, Generic, Hashable, Read, Show)

instance (Default w) => Default (HashMap DR w) where
    def = fromList $ map (,def) [D0, D1, D2, D3, D4, D5, D6, D7]

data AR = A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7
    deriving (Eq, Generic, Hashable, Read, Show)

instance (Default w) => Default (HashMap AR w) where
    def = fromList $ map (,def) [A0, A1, A2, A3, A4, A5, A6, A7]

data Argument w l
    = Dn DR
    | An AR
    | Imm l
    deriving (Show)

-- | The 'Isa' type represents the instruction set architecture for the M68k machine.
-- Each constructor corresponds to a specific instruction.
data Isa w l
    = Move {mode :: Mode, src, dst :: Argument w l}
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

argument :: (MachineWord w) => Parser (Argument w (Ref w))
argument =
    choice
        [ try $ do
            void (string "D")
            n <- oneOf ['0' .. '7']
            lookAhead (hspace1 <|> eol' "\\")
            return $ Dn $ Unsafe.read ['D', n]
        , try $ do
            void (string "(")
            hspace
            void (string "A")
            n <- oneOf ['0' .. '7']
            hspace
            void (string ")")
            lookAhead (hspace1 <|> eol' "\\")
            return $ An $ Unsafe.read ['A', n]
        , Imm <$> reference
        ]

instance (MachineWord w) => DerefMnemonic (Isa w) w where
    derefMnemonic f _offset i =
        let derefArg (Dn r) = Dn r
            derefArg (An r) = An r
            derefArg (Imm l) = Imm $ deref' f l
         in case i of
                Move{mode, src, dst} -> Move mode (derefArg src) (derefArg dst)
                Not{mode, dst} -> Not mode (derefArg dst)
                Halt -> Halt

instance ByteLength (Isa w l) where
    byteLength _ = 4 -- Simplified assumption: all instructions are 2 bytes.

data MachineState mem w = State
    { pc :: Int
    , dr :: HashMap DR w
    , ar :: HashMap AR w
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
                nextPc
            Halt -> modify $ \st -> st{stopped = True}
