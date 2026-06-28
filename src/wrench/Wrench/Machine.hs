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

tellState :: st -> State (Simulation st isa) ()
tellState machineState = modify
    $ \sim@Simulation{log, stateRecordCount, stateRecordLimits, takePartOnStateRecordLimit, instructionCount} ->
        if stateRecordCount >= stateRecordLimits
            then
                let n = (stateRecordLimits `div` takePartOnStateRecordLimit)
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
                 in sim
                        { log = take n log <> rest' <> [TWarn warn]
                        , stateRecordCount = stateRecordCount - dropped
                        }
            else
                sim
                    { log = TState{tInstructionCount = instructionCount + 1, tState = machineState} : log
                    , stateRecordCount = stateRecordCount + 1
                    }

tellError msg = modify $ \sim@Simulation{log} ->
    sim{log = TError msg : log}

-- | Run the simulation and return both the recorded trace log and the final
--   machine state. The final state carries the complete runtime accumulators
--   (e.g. 'AccessLog' in 'IoMem'); per-state trace entries are recorded
--   pre-step and therefore don't include the last instruction's accesses.
simulate :: (Machine st isa w) => Simulation st isa -> ([Trace st isa], st)
simulate sim =
    let Simulation{log, machineState} = execState simulate' sim
     in (reverse log, machineState)

simulateInstructionStep :: (Machine st isa w) => State (Simulation st isa) ()
simulateInstructionStep =
    modify $ \sim@Simulation{machineState, instructionCount} ->
        sim
            { machineState = execState instructionStep machineState
            , instructionCount = instructionCount + 1
            }

simulate' :: (Machine st isa w) => State (Simulation st isa) ()
simulate' = do
    Simulation{machineState, instructionCount, instructionLimits} <- get
    if instructionCount >= instructionLimits
        then tellError "Simulation limit reached"
        else case evalState instructionFetch machineState of
            Right _ -> do
                tellState machineState
                simulateInstructionStep
                simulate'
            Left err | err == halted -> return ()
            Left err -> tellError err

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
