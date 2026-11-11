module Wrench.Machine (powerOn, Run (..)) where

import Relude
import Wrench.Machine.Types

-- | Every record of a run in order, then the state it ended in, which
--   carries the run's accumulators (e.g. 'AccessLog' in 'IoMem').
data Run st isa
    = Step !(Trace st isa) (Run st isa)
    | End st

-- | Run the simulation as a lazy stream, so a consumer keeps alive only
--   what it keeps.
powerOn :: (Machine st isa w) => Int -> st -> Run st isa
powerOn instructionLimits = go Nothing 0
    where
        go prevInstr !count st
            | count >= instructionLimits = Step (TError "Simulation limit reached") (End st)
            | otherwise =
                let record = TState{tInstructionCount = count, tPrevInstruction = prevInstr, tState = st}
                 in case runState instructionFetch st of
                        (Right (pc, instr), fetched) ->
                            Step record $ go (Just instr) (count + 1) (execState (instructionExecute pc instr) fetched)
                        (Left err, _) | err == halted -> Step record (End st)
                        (Left err, _) -> Step (TError err) (End st)
