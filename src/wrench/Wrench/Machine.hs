{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Wrench.Machine (powerOn, TracePlan (..)) where

import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Relude
import Relude.Extra
import Wrench.Machine.Types

-- | How many states the trace keeps from each end of the run.
data TracePlan = TracePlan
    { tpHead :: !Int
    , tpTail :: !Int
    }
    deriving (Eq, Show)

data Simulation st isa acc = Simulation
    { headLog :: ![Trace st isa]
    , headCount :: !Int
    , tailLog :: !(Seq (Trace st isa))
    , closing :: ![Trace st isa]
    , droppedStates :: !Int
    , prevAccesses :: !AccessCount
    , accessCount :: st -> AccessCount
    , observed :: !acc
    , observe :: acc -> Trace st isa -> acc
    , machineState :: st
    , pc2label :: HashMap Int Text
    , instructionCount :: Int
    , instructionLimits :: Int
    , plan :: !TracePlan
    }

tellState :: Maybe isa -> st -> State (Simulation st isa acc) ()
tellState prevInstr machineState =
    modify $
        \sim@Simulation
            { headLog
            , headCount
            , tailLog
            , droppedStates
            , prevAccesses
            , accessCount
            , observed
            , observe
            , instructionCount
            , plan = TracePlan{tpHead, tpTail}
            } ->
                let accesses = accessCount machineState
                    !record =
                        TState
                            { tInstructionCount = instructionCount
                            , tPrevInstruction = prevInstr
                            , tAccesses = acSince prevAccesses accesses
                            , tState = machineState
                            }
                    sim' = sim{prevAccesses = accesses, observed = observe observed record}
                 in if headCount < tpHead
                        then sim'{headLog = record : headLog, headCount = headCount + 1}
                        else case tailLog of
                            _ :<| rest
                                | Seq.length tailLog >= tpTail ->
                                    sim'{tailLog = rest :|> record, droppedStates = droppedStates + 1}
                            Empty | tpTail <= 0 -> sim'{droppedStates = droppedStates + 1}
                            _ -> sim'{tailLog = tailLog :|> record}

tellError msg = modify $ \sim -> sim{closing = [TError msg]}

-- | Run the simulation and return the kept states, a warning where states
--   were dropped, the final machine state and the fold over every state.
--   The final state carries the complete runtime accumulators (e.g.
--   'AccessLog' in 'IoMem'); per-state trace entries are recorded after each
--   step, so the last entry captures the result of the halt instruction too.
simulate :: (Machine st isa w) => Simulation st isa acc -> ([Trace st isa], st, acc)
simulate sim =
    let Simulation{headLog, tailLog, closing, droppedStates, machineState, observed} = execState (simulate' Nothing) sim
        gap = [TWarn ("Dropped " <> show droppedStates <> " states") | droppedStates > 0]
     in (reverse headLog <> gap <> toList tailLog <> closing, machineState, observed)

simulateInstructionStep :: (Machine st isa w) => State (Simulation st isa acc) ()
simulateInstructionStep =
    modify $ \sim@Simulation{machineState, instructionCount} ->
        sim
            { machineState = execState instructionStep machineState
            , instructionCount = instructionCount + 1
            }

simulate' :: (Machine st isa w) => Maybe isa -> State (Simulation st isa acc) ()
simulate' prevInstr = do
    Simulation{machineState, instructionCount, instructionLimits} <- get
    if instructionCount >= instructionLimits
        then tellError "Simulation limit reached"
        else case evalState instructionFetch machineState of
            Right (_, instr) -> do
                tellState prevInstr machineState
                simulateInstructionStep
                simulate' (Just instr)
            Left err | err == halted -> do
                Simulation{machineState = stateAfter} <- get
                tellState prevInstr stateAfter
            Left err -> tellError err

powerOn ::
    (IsWord w, Machine st isa w) =>
    Int
    -> TracePlan
    -> (st -> AccessCount)
    -> (acc -> Trace st isa -> acc)
    -> acc
    -> HashMap Text w
    -> st
    -> Either Text ([Trace st isa], st, acc)
powerOn instructionLimits plan accessCount observe observed labels machineInitState = do
    let pc2label = fromList $ map (\(a, b) -> (fromEnum b, a)) $ toPairs labels
    Right $
        simulate
            Simulation
                { headLog = []
                , headCount = 0
                , tailLog = Seq.empty
                , closing = []
                , droppedStates = 0
                , prevAccesses = emptyAccessCount
                , accessCount
                , observed
                , observe
                , machineState = machineInitState
                , pc2label = pc2label
                , instructionCount = 0
                , instructionLimits
                , plan
                }
