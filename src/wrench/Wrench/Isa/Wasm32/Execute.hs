{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Executing one instruction at a time.
-}
module Wrench.Isa.Wasm32.Execute () where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.String.Interpolate (i)
import Relude
import Wrench.Isa.Wasm32.Instruction
import Wrench.Isa.Wasm32.State
import Wrench.Machine.Memory
import Wrench.Machine.Types

instance (IsWord w) => InitState (Wasm32St w) where
    initState pc dump _randomStream =
        Wasm32St
            { pc
            , sp = defaultStackRoot dump
            , stackRoot = defaultStackRoot dump
            , scanCache = []
            , ctrlSp = defaultStackRoot dump
            , frameBase = defaultStackRoot dump - byteSizeT @w
            , localCount = 0
            , mem = dump
            , stopped = False
            , internalError = Nothing
            }

afterMatchingEnd ::
    (IsWord w) => Int -> Wasm32Isa w w -> (Int -> State (Wasm32St w) ()) -> State (Wasm32St w) ()
afterMatchingEnd pc opener f =
    matchElseOrEnd False pc opener >>= \case
        Left err -> raiseInternalError err
        Right afterEnd -> f afterEnd

afterMatchingElseOrEnd ::
    (IsWord w) => Int -> Wasm32Isa w w -> (Int -> State (Wasm32St w) ()) -> State (Wasm32St w) ()
afterMatchingElseOrEnd pc opener f =
    matchElseOrEnd True pc opener >>= \case
        Left err -> raiseInternalError err
        Right afterEnd -> f afterEnd

matchElseOrEnd :: (IsWord w) => Bool -> Int -> Wasm32Isa w w -> State (Wasm32St w) (Either Text Int)
matchElseOrEnd stopAtElse addr opener = cachedScan addr $ go (0 :: Int) (addr + byteSize opener)
    where
        go depth at =
            fetchInstructionAt at >>= \case
                Left err ->
                    return $ Left [i|scope at #{hexAddr 2 addr} has no matching end: #{err}|]
                Right instruction ->
                    let next = at + byteSize instruction
                     in case instruction of
                            Block -> go (depth + 1) next
                            Loop -> go (depth + 1) next
                            If -> go (depth + 1) next
                            Else | stopAtElse && depth == 0 -> return $ Right next
                            End
                                | depth == 0 -> return $ Right next
                                | otherwise -> go (depth - 1) next
                            _ -> go depth next

returnFromCall :: forall w. (IsWord w) => State (Wasm32St w) ()
returnFromCall =
    innermostCall >>= \case
        Left err -> raiseInternalError err
        Right (addr, CallScope{csCallerFrameBase, csCallerLocalCount, csReturnPc, csResultCount}) -> do
            results <- reverse <$> replicateM csResultCount popValue
            Wasm32St{frameBase} <- get
            modify $ \st ->
                st
                    { sp = frameBase + byteSizeT @w
                    , ctrlSp = addr
                    , frameBase = csCallerFrameBase
                    , localCount = csCallerLocalCount
                    }
            mapM_ pushValue results
            setPc csReturnPc
        Right (addr, other) ->
            raiseInternalError [i|control stack corrupted at #{hexAddr 2 addr}: expected a call record, found #{other}|]

branchTo :: (IsWord w) => Int -> State (Wasm32St w) ()
branchTo depth = do
    scope <- scopeAtDepth depth
    case scope of
        Nothing -> raiseInternalError "br/br_if: the scope it names went missing"
        Just (Left err) -> raiseInternalError err
        Just (Right LoopScope{csStart}) -> do
            retireScopes depth
            setPc csStart
        Just (Right BlockScope{csEnd}) -> do
            retireScopes (depth + 1)
            setPc (csEnd + byteSize End)
        Just (Right CallScope{}) ->
            raiseInternalError "br/br_if: depth reaches past the enclosing function; use return to leave it"

instance (IsWord w) => Machine (Wasm32St w) (Wasm32Isa w w) w where
    instructionFetch = do
        st <- get
        case st of
            Wasm32St{stopped = True} -> return $ Left halted
            Wasm32St{internalError = Just err} -> return $ Left err
            Wasm32St{pc} ->
                fetchInstructionAt pc
                    >>= ( \case
                            Left err -> return $ Left err
                            Right instruction -> do
                                return $ Right (pc, instruction)
                        )

    instructionStep = do
        (pc, instruction) <- either (\err -> error [i|internal error: #{err}|]) id <$> instructionFetch
        instructionExecute pc instruction

    instructionExecute pc instruction =
        case instruction of
            I32Const value -> pushValue value >> nextPc instruction
            I32Add -> binary (+) >> nextPc instruction
            I32Sub -> binary (-) >> nextPc instruction
            I32Mul -> binary (*) >> nextPc instruction
            I32DivS -> signedDiv >> nextPc instruction
            I32RemS -> signedRem >> nextPc instruction
            I32And -> binary (.&.) >> nextPc instruction
            I32Or -> binary (.|.) >> nextPc instruction
            I32Xor -> binary xor >> nextPc instruction
            I32Shl -> binary (\x y -> x `shiftL` (fromEnum y .&. 0x1F)) >> nextPc instruction
            I32ShrS -> binary (\x y -> x `shiftR` (fromEnum y .&. 0x1F)) >> nextPc instruction
            I32ShrU -> unsignedBinary (\x y -> toSign $ x `shiftR` (fromEnum y .&. 0x1F)) >> nextPc instruction
            I32Eqz -> do
                value <- popValue
                pushValue $ if value == 0 then 1 else 0
                nextPc instruction
            I32Eq -> compareS (==) >> nextPc instruction
            I32LtS -> compareS (<) >> nextPc instruction
            I32LeS -> compareS (<=) >> nextPc instruction
            I32GtS -> compareS (>) >> nextPc instruction
            I32GeS -> compareS (>=) >> nextPc instruction
            I32LtU -> compareU (<) >> nextPc instruction
            I32LeU -> compareU (<=) >> nextPc instruction
            I32GtU -> compareU (>) >> nextPc instruction
            I32GeU -> compareU (>=) >> nextPc instruction
            I32Load offset -> load (fromEnum offset) getWord
            I32Load8U offset -> load (fromEnum offset) (fmap fromIntegral . getByte)
            I32Load8S offset -> load (fromEnum offset) (fmap (fromIntegral . (fromIntegral :: Word8 -> Int8)) . getByte)
            I32Store offset -> store (fromEnum offset) setWord
            I32Store8 offset -> store (fromEnum offset) (\addr -> setByte addr . fromIntegral)
            If -> do
                condition <- popValue
                if condition == 0
                    then afterMatchingElseOrEnd pc instruction setPc
                    else nextPc instruction
            Else -> afterMatchingEnd pc instruction setPc
            End ->
                scopeAtDepth 0 >>= \case
                    Nothing -> nextPc instruction
                    Just (Left err) -> raiseInternalError err
                    Just (Right BlockScope{csEnd})
                        | csEnd == pc -> do
                            retireScopes 1
                            nextPc instruction
                    Just (Right LoopScope{csEnd})
                        | csEnd == pc -> do
                            retireScopes 1
                            nextPc instruction
                    _ -> nextPc instruction
            Loop ->
                afterMatchingEnd pc instruction $ \afterEnd -> do
                    pushScope LoopScope{csStart = pc + byteSize instruction, csEnd = afterEnd - byteSize End}
                    nextPc instruction
            Block ->
                afterMatchingEnd pc instruction $ \afterEnd -> do
                    pushScope BlockScope{csEnd = afterEnd - byteSize End}
                    nextPc instruction
            Br depth -> branchTo (fromEnum depth)
            BrIf depth -> do
                condition <- popValue
                if condition == 0
                    then nextPc instruction
                    else branchTo (fromEnum depth)
            Drop -> popValue >> nextPc instruction
            Select -> do
                condition <- popValue
                onFalse <- popValue
                onTrue <- popValue
                pushValue (if condition /= 0 then onTrue else onFalse)
                nextPc instruction
            Unreachable -> raiseInternalError "unreachable"
            LocalGet idx -> do
                addr <- localAddr (fromEnum idx)
                value <- getWord addr
                pushValue value
                nextPc instruction
            LocalSet idx -> do
                addr <- localAddr (fromEnum idx)
                value <- popValue
                setWord addr value
                nextPc instruction
            LocalTee idx -> do
                addr <- localAddr (fromEnum idx)
                value <- popValue
                setWord addr value
                pushValue value
                nextPc instruction
            LocalsReserve n -> do
                reserveLocals (fromEnum n)
                nextPc instruction
            Call{callParamCount, callResultCount} -> do
                callTarget <- popValue
                Wasm32St{sp, frameBase, localCount} <- get
                let paramCount = fromEnum callParamCount
                    calleeFrameBase = sp + (paramCount - 1) * byteSizeT @w
                pushScope
                    CallScope
                        { csCallerFrameBase = frameBase
                        , csCallerLocalCount = localCount
                        , csReturnPc = pc + byteSize instruction
                        , csResultCount = fromEnum callResultCount
                        }
                modify $ \st ->
                    st
                        { frameBase = calleeFrameBase
                        , localCount = paramCount
                        }
                setPc (fromEnum callTarget)
            Return -> returnFromCall
            SpInit -> do
                addr <- fromEnum <$> popValue
                modify $ \st ->
                    st
                        { sp = addr
                        , stackRoot = addr
                        , ctrlSp = addr
                        , frameBase = addr - byteSizeT @w
                        , localCount = 0
                        }
                nextPc instruction
            Halt -> modify $ \st -> st{stopped = True}
        where
            load offset readAt = do
                addr <- popValue
                readAt (fromEnum addr + offset) >>= pushValue
                nextPc instruction
            store offset writeAt = do
                value <- popValue
                addr <- popValue
                writeAt (fromEnum addr + offset) value
                nextPc instruction
            binary op = do
                y <- popValue
                x <- popValue
                pushValue (x `op` y)
            unsignedBinary op = do
                y <- fromSign <$> popValue
                x <- fromSign <$> popValue
                pushValue (op x y)
            signedDiv = do
                y <- popValue
                x <- popValue
                if
                    | y == 0 -> raiseInternalError "integer divide by zero"
                    | x == minBound && y == -1 -> raiseInternalError "integer overflow"
                    | otherwise -> pushValue (x `quot` y)
            signedRem = do
                y <- popValue
                x <- popValue
                if y == 0
                    then raiseInternalError "integer divide by zero"
                    else pushValue (x `rem` y)
            compareS op = do
                y <- popValue
                x <- popValue
                pushValue $ if x `op` y then 1 else 0
            compareU op = do
                y <- fromSign <$> popValue
                x <- fromSign <$> popValue
                pushValue $ if x `op` y then 1 else 0
