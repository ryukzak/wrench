{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Wrench.Machine (powerOn) where

import Relude
import Relude.Extra
import Wrench.Machine.Types

data Simulation st isa = Simulation
    { log :: [Trace st isa]
    , machineState :: st
    , pc2label :: HashMap Int String
    , instructionCount :: Int
    , instructionLimits :: Int
    , stateRecordCount :: Int
    , stateRecordLimits :: Int
    , takePartOnStateRecordLimit :: Int
    }

fetchNextInstruction :: (Machine st isa w) => st -> Maybe isa
fetchNextInstruction st =
    case evalState instructionFetch st of
        Right (_, instruction) -> Just instruction
        Left _ -> Nothing

tellState :: (Machine st isa w) => Maybe isa -> State (Simulation st isa) ()
tellState lastInstruction = do
    sim@Simulation{machineState} <- get
    let nextInstruction = fetchNextInstruction machineState

    modify
        $ \sim'@Simulation{log, stateRecordCount, stateRecordLimits, takePartOnStateRecordLimit, instructionCount} ->
            if stateRecordCount >= stateRecordLimits
                then
                    let n = stateRecordLimits `div` takePartOnStateRecordLimit
                        rest = drop n log
                        rest' =
                            filter
                                ( \case
                                    TState{} -> False
                                    _ -> True
                                )
                                rest
                        dropped = length rest - length rest'
                        warn = "Dropped " <> show dropped <> " states"
                     in sim'
                            { log = take n log <> rest' <> [TWarn warn]
                            , stateRecordCount = stateRecordCount - dropped
                            }
                else
                    sim'
                        { log =
                            TState
                                { tInstructionCount = instructionCount
                                , tInstructionNext = nextInstruction
                                , tLastInstruction = lastInstruction
                                , tState = machineState
                                }
                                : log
                        , stateRecordCount = stateRecordCount + 1
                        }

tellError msg = modify $ \sim@Simulation{log} ->
    sim{log = TError msg : log}

simulate :: (Machine st isa w) => Simulation st isa -> ([Trace st isa], st)
simulate sim =
    let Simulation{log, machineState} = execState simulate' sim
     in (reverse log, machineState)

simulateOneInstruction :: (Machine st isa w) => State (Simulation st isa) (Either Text isa)
simulateOneInstruction = do
    sim@Simulation{machineState, instructionCount} <- get

    case evalState instructionFetch machineState of
        Left err ->
            return $ Left err

        Right (pc, instruction) -> do
            let machineState' =
                    execState
                        (instructionExecute pc instruction)
                        machineState

            put sim
                { machineState = machineState'
                , instructionCount = instructionCount + 1
                }

            return $ Right instruction

simulate' :: (Machine st isa w) => State (Simulation st isa) ()
simulate' = do
    tellState Nothing
    simulateLoop

simulateLoop :: (Machine st isa w) => State (Simulation st isa) ()
simulateLoop = do
    Simulation{instructionCount, instructionLimits} <- get

    if instructionCount >= instructionLimits
        then tellError "Simulation limit reached"
        else do
            result <- simulateOneInstruction

            case result of
                Right instruction -> do
                    tellState (Just instruction)
                    simulateLoop

                Left err
                    | err == halted -> return ()
                    | otherwise -> tellError err

powerOn ::
    (Machine st isa w, MachineWord w) =>
    Int
    -> Int
    -> HashMap String w
    -> st
    -> Either Text ([Trace st isa], st)
powerOn instructionLimits stateRecordLimits labels machineInitState = do
    let pc2label = fromList $ map (\(a, b) -> (fromEnum b, a)) $ toPairs labels
    Right
        $ simulate
            Simulation
                { log = []
                , machineState = machineInitState
                , pc2label = pc2label
                , instructionCount = 0
                , instructionLimits
                , stateRecordCount = 0
                , stateRecordLimits
                , takePartOnStateRecordLimit = 4
                }