{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Wrench.Machine (
    powerOn,
    RuntimeStats (..),
    StackStats (..),
) where

import Relude
import Relude.Extra
import Wrench.Machine.Types

-- | Stats accumulated while simulating, classified by stack architecture.
--
-- For SP-based stacks the "top" is taken as the SP value observed just before
-- the first decrease — this skips over the multi-step initialisation sequence
-- (e.g. RiscIv's @lui sp, 0 ; addi sp, sp, _@ or M68k's
-- @movea.l label, A7 ; movea.l (A7), A7@) where intermediate SP values are
-- transient address-of-label rather than the true stack top.
data StackStats w
    = StackStatsNone
    | StackStatsSp
        { ssLastSp :: !(Maybe w)
        -- ^ last observed SP value (for trend detection)
        , ssTopSp :: !(Maybe w)
        -- ^ "settled" top — the value just before the first decrease
        , ssMinSp :: !(Maybe w)
        -- ^ minimum SP observed after the first decrease
        }
    | StackStatsList
        { ssMaxDDepth :: !Int
        , ssMaxRDepth :: !Int
        }
    deriving (Show)

data RuntimeStats w = RuntimeStats
    { rsInstructions :: !Int
    , rsStack :: !(StackStats w)
    }
    deriving (Show)

initStackStats :: StackInfo w -> StackStats w
initStackStats NoStack = StackStatsNone
initStackStats SpStack{} = StackStatsSp{ssLastSp = Nothing, ssTopSp = Nothing, ssMinSp = Nothing}
initStackStats ListStack{} = StackStatsList{ssMaxDDepth = 0, ssMaxRDepth = 0}

mergeStack :: (Ord w) => StackInfo w -> StackStats w -> StackStats w
mergeStack NoStack acc = acc
mergeStack SpStack{spInitialised = False} acc = acc
mergeStack SpStack{sp, spInitialised = True} acc =
    case acc of
        StackStatsSp{ssLastSp = Nothing} ->
            StackStatsSp{ssLastSp = Just sp, ssTopSp = Just sp, ssMinSp = Nothing}
        StackStatsSp{ssLastSp = Just prev, ssMinSp = Nothing} ->
            if sp < prev
                then StackStatsSp{ssLastSp = Just sp, ssTopSp = Just prev, ssMinSp = Just sp}
                else StackStatsSp{ssLastSp = Just sp, ssTopSp = Just sp, ssMinSp = Nothing}
        StackStatsSp{ssTopSp, ssMinSp = Just lo} ->
            StackStatsSp{ssLastSp = Just sp, ssTopSp, ssMinSp = Just (min lo sp)}
        _ -> acc
mergeStack ListStack{dDepth, rDepth} acc =
    case acc of
        StackStatsList{ssMaxDDepth, ssMaxRDepth} ->
            StackStatsList{ssMaxDDepth = max ssMaxDDepth dDepth, ssMaxRDepth = max ssMaxRDepth rDepth}
        _ -> acc

data Simulation st isa w = Simulation
    { log :: [Trace st isa]
    , machineState :: st
    , pc2label :: HashMap Int String
    , instructionCount :: Int
    , instructionLimits :: Int
    , stateRecordCount :: Int
    , stateRecordLimits :: Int
    , takePartOnStateRecordLimit :: Int
    , stackStats :: StackStats w
    }

tellState :: (StateInterspector st m isa w, Ord w) => st -> State (Simulation st isa w) ()
tellState machineState = modify
    $ \sim@Simulation{log, stateRecordCount, stateRecordLimits, takePartOnStateRecordLimit, stackStats} ->
        let stackStats' = mergeStack (stackInfo machineState) stackStats
            sim' = sim{stackStats = stackStats'}
         in if stateRecordCount >= stateRecordLimits
                then
                    let n = (stateRecordLimits `div` takePartOnStateRecordLimit)
                        rest = drop n log
                        rest' =
                            filter
                                ( \case
                                    TState _ -> False
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
                        { log = TState machineState : log
                        , stateRecordCount = stateRecordCount + 1
                        }

tellError msg = modify $ \sim@Simulation{log} ->
    sim{log = TError msg : log}

simulate :: (Machine st isa w, StateInterspector st m isa w, Ord w) => Simulation st isa w -> ([Trace st isa], RuntimeStats w)
simulate sim =
    let Simulation{log, instructionCount, stackStats} = execState simulate' sim
     in (reverse log, RuntimeStats{rsInstructions = instructionCount, rsStack = stackStats})

simulateInstructionStep :: (Machine st isa w) => State (Simulation st isa w) ()
simulateInstructionStep =
    modify $ \sim@Simulation{machineState, instructionCount} ->
        sim
            { machineState = execState instructionStep machineState
            , instructionCount = instructionCount + 1
            }

simulate' :: (Machine st isa w, StateInterspector st m isa w, Ord w) => State (Simulation st isa w) ()
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
    (Machine st isa w, MachineWord w, StateInterspector st m isa w) =>
    Int
    -> Int
    -> HashMap String w
    -> st
    -> Either Text ([Trace st isa], RuntimeStats w)
powerOn instructionLimits stateRecordLimits labels machineInitState = do
    let pc2label = fromList $ map (\(a, b) -> (fromEnum b, a)) $ toPairs labels
        initialStack = initStackStats (stackInfo machineInitState)
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
                , stackStats = initialStack
                }
