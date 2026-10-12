{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- The instances below are orphans: their classes live in "Wrench.Machine.Types"
-- and "Wrench.Report", their type in "Wrench.Isa.Wasm32.State", and their
-- bodies need this module's own helpers -- so there is no module that holds
-- all three. "Wrench.Isa.Wasm32" imports every one of these modules, so no
-- user of 'Wasm32St' can reach it without the instances coming too.
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Running a program: working out its control flow when it loads, and then
executing one instruction at a time.

Both halves are here because they are the same job seen twice. A @block@, a
@br@ and an @end@ carry no target; where each one goes follows from the
nesting it sits in, which is fixed the moment the program is in memory. So
'resolveProgram' settles all of it once, into a 'ScopePlan', and the
instructions below look their answers up rather than scanning for them.
-}
module Wrench.Isa.Wasm32.Execute (
    resolveScopeTargets,
    resolveProgram,
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.IntSet qualified as IntSet
import Relude
import Relude.Extra (insert, toPairs, (!?))
import Wrench.Isa.Wasm32.Instruction
import Wrench.Isa.Wasm32.State
import Wrench.Machine.Memory
import Wrench.Machine.Types

-- | Match every @block@\/@loop@\/@if@\/@else@ with its own @end@ and
-- report where each one jumps to, keyed by its own address.
--
-- Run once, by 'initState', over the instructions the program loaded --
-- never per execution. Real WebAssembly's scope instructions carry no
-- target either: an engine validates the nesting and builds exactly this
-- table while loading a module, which is also why a @block@ whose @end@
-- is missing is caught here, before the first instruction runs, rather
-- than whenever some forward scan happens to fall off the end of memory.
--
-- A @block@ or @loop@ maps to its own matching @end@, which is what goes
-- into its control record as @csEnd@. An @if@ maps to where its skip
-- path resumes -- the first instruction of the @else@ body, or just past
-- the @end@ when there is none -- and an @else@ to just past its @end@.
resolveScopeTargets :: [(Int, Wasm32Isa w w)] -> Either Text (IntMap Int)
resolveScopeTargets = go []
    where
        -- \| One entry per scope still waiting for its @end@: the address
        -- of the instruction to resolve, and whether it wants the @end@'s
        -- own address ('AtEnd', for @block@\/@loop@, which store it as
        -- @csEnd@) or the address just past it ('PastEnd', for the
        -- @if@\/@else@ that skip over it).
        go open [] = case open of
            [] -> Right mempty
            (addr, _) : _ -> Left $ "block/loop/if at " <> hexAddr 2 addr <> " has no matching end"
        go open ((addr, instruction) : rest) = case instruction of
            Block -> go ((addr, AtEnd) : open) rest
            Loop -> go ((addr, AtEnd) : open) rest
            If -> go ((addr, PastEnd) : open) rest
            Else -> case open of
                -- The @if@ resumes at the first instruction of this
                -- @else@'s body; this @else@ takes the @if@'s place in the
                -- open list, to be closed by the same @end@.
                (ifAddr, PastEnd) : outer ->
                    insert ifAddr (addr + byteSize instruction)
                        <$> go ((addr, PastEnd) : outer) rest
                _ -> Left $ "else at " <> hexAddr 2 addr <> " has no matching if"
            End -> case open of
                (openAddr, want) : outer ->
                    let target = case want of
                            AtEnd -> addr
                            PastEnd -> addr + byteSize End
                     in insert openAddr target <$> go outer rest
                [] -> Left $ "end at " <> hexAddr 2 addr <> " has no matching block/loop/if"
            _ -> go open rest

-- | Which address a scope instruction wants out of its own matching
-- @end@ -- see 'resolveScopeTargets'.
data ScopeTargetKind = AtEnd | PastEnd

-- | The instruction runs a loaded program consists of: each one a
-- maximal stretch of instructions that follow one another with no gap,
-- in address order. Reads the dump's cells rather than memory, so it
-- sees the program as laid out and never a byte of @.data@ as an
-- instruction.
--
-- Split at the gaps rather than handed over as one stream, because a
-- scope cannot span one: a @block@ whose body runs off the end of its
-- own @.text@ into the @.data@ that follows has no path to an @end@ on
-- the far side, so pairing it with one there would accept a program that
-- cannot execute. Matching each run on its own keeps an unbalanced
-- stretch unbalanced.
codeRuns :: forall w. (IsWord w) => IoMem (Wasm32Isa w w) w -> [[(Int, Wasm32Isa w w)]]
codeRuns mem = foldr step [] [(addr, i) | (addr, Instruction i) <- toPairs (dumpCells mem)]
    where
        step entry@(addr, instruction) (run@((next, _) : _) : rest)
            | addr + byteSize instruction == next = (entry : run) : rest
        step entry runs = [entry] : runs

-- | Work out what every @br@ and @br_if@ in one run does, and which of
-- its @end@s close a scope.
--
-- A second walk over the same run, because a @br@ out of a @block@ jumps
-- past that block's @end@ -- which the first walk only learns when it
-- gets there. Takes the first walk's answers ('resolveScopeTargets') and
-- reads the blocks' ends straight out of them.
--
-- A depth deeper than the nesting the branch sits in is rejected here.
-- At run time that was a branch stepping outward until it hit the
-- enclosing call's record; written down, it is a branch naming a scope
-- that is not there, and saying so needs no execution to discover.
resolveBranches :: forall w. (IsWord w) => IntMap Int -> [(Int, Wasm32Isa w w)] -> Either Text ScopePlan
resolveBranches targets = go []
    where
        -- \| The scopes open at this point, innermost first, each as its
        -- kind and its own address. @if@ is in here so that an @end@
        -- knows what it closes, but filtered out for a branch depth --
        -- only @block@ and @loop@ count as levels.
        go _ [] = Right mempty
        go open ((addr, instruction) : rest) = case instruction of
            Block -> go ((Block, addr) : open) rest
            Loop -> go ((Loop, addr) : open) rest
            If -> go ((If, addr) : open) rest
            -- The @end@ still closes an @if@; only the address moves.
            Else -> go ((If, addr) : drop 1 open) rest
            End -> case open of
                (kind, _) : outer
                    | isScopeRecord kind ->
                        withEndPop addr <$> go outer rest
                _ : outer -> go outer rest
                [] -> Right mempty
            Br depth -> plan addr (fromEnum depth) rest
            BrIf depth -> plan addr (fromEnum depth) rest
            _ -> go open rest
            where
                plan brAddr depth rest' = do
                    p <- branchPlan brAddr depth (filter (isScopeRecord . fst) open)
                    withBranch brAddr p <$> go open rest'

        isScopeRecord Block = True
        isScopeRecord Loop = True
        isScopeRecord _ = False

        branchPlan brAddr depth levels = case drop depth levels of
            (Loop, loopAddr) : _ ->
                -- Re-entering a loop leaves its own record open, so only
                -- what is nested inside it retires.
                Right BranchPlan{bpTarget = loopAddr + byteSize (Loop @w @w), bpPop = depth, bpCheck = depth + 1}
            (Block, blockAddr) : _ -> do
                end <-
                    maybe
                        (Left $ "block at " <> hexAddr 2 blockAddr <> " has no resolved end")
                        Right
                        (targets !? blockAddr)
                Right BranchPlan{bpTarget = end + byteSize (End @w @w), bpPop = depth + 1, bpCheck = depth + 1}
            _ ->
                Left $
                    "br/br_if at "
                        <> hexAddr 2 brAddr
                        <> ": depth "
                        <> show depth
                        <> " names a scope that does not enclose it, only "
                        <> show (length levels)
                        <> " open here"

        withEndPop addr plan = plan{spEndPops = IntSet.insert addr (spEndPops plan)}
        withBranch addr p plan = plan{spBranches = insert addr p (spBranches plan)}

-- | Everything fixed about a loaded program's control flow -- the two
-- walks above over each of its 'codeRuns'. Addresses are unique across
-- runs, so the tables merge without overlapping.
resolveProgram :: forall w. (IsWord w) => IoMem (Wasm32Isa w w) w -> Either Text ScopePlan
resolveProgram = fmap mconcat . traverse runPlan . codeRuns
    where
        runPlan run = do
            targets <- resolveScopeTargets run
            plan <- resolveBranches targets run
            return plan{spTargets = targets}

instance (IsWord w) => InitState (Wasm32St w) where
    initState pc dump _randomStream =
        Wasm32St
            { pc
            , sp = stackRoot dump
            , ctrlBase = stackRoot dump
            , ctrlSp = stackRoot dump
            , frameBase = stackRoot dump - byteSizeT @w
            , localCount = 0
            , memAccessCount = 0
            , scopePlan = fromRight mempty resolved
            , mem = dump
            , stopped = False
            , -- Malformed nesting stops the program before its first
              -- instruction, through the same field a trap uses: nothing
              -- about it depends on which path would have executed, so
              -- there is no reason to wait and find out.
              internalError = leftToMaybe resolved
            }
        where
            resolved = resolveProgram dump

-- | Return from the nearest enclosing call: forward the declared
-- results, discard the callee's frame, and restore the caller's.
--
-- Everything the callee still had open inside that frame goes with it.
-- Those records need no reading: closing a @block@ or @loop@ is only a
-- retreat of @ctrlSp@, so finding the call's own record is the only part
-- that has to touch memory -- one word per record on the way out to read
-- its tag, then the record itself, once.
returnFromCall :: forall w. (IsWord w) => State (Wasm32St w) ()
returnFromCall = do
    st <- get
    go (scopeAddrs @w st)
    where
        go [] = raiseInternalError "return without active function frame"
        go (addr : outer) = do
            -- Word 0 carries the tag, so one read per record is enough to
            -- step over a @block@ or @loop@. Only the call record itself
            -- is worth completing, and its word 0 is already in hand.
            word0 <- getWord @w addr
            case decodeScopeTag word0 of
                Left err -> raiseInternalError $ corrupted addr err
                Right CallTag -> closeCall addr word0
                Right _ -> go outer

        closeCall addr word0 = do
            rest <- mapM (getWord @w) (drop 1 (scopeWordAddrs @w addr))
            case decodeScope @w (word0 : rest) of
                Left err -> raiseInternalError $ corrupted addr err
                Right CallScope{csCallerFrameBase, csCallerLocalCount, csReturnPc, csResultCount} -> do
                    results <- popValues csResultCount
                    Wasm32St{frameBase} <- get
                    modify $ \st ->
                        st
                            { sp = frameBase + byteSizeT @w
                            , -- Everything from this record up was the
                              -- callee's; the record goes too.
                              ctrlSp = addr
                            , frameBase = csCallerFrameBase
                            , localCount = csCallerLocalCount
                            }
                    mapM_ pushValue results
                    setPc csReturnPc
                Right other -> raiseInternalError $ corrupted addr ("expected a call record, found " <> show other)

        corrupted addr err = "control stack corrupted at " <> hexAddr 2 addr <> ": " <> err

-- | Branch to the @block@\/@loop@ the 'BranchPlan' for this @br@ names:
-- a @loop@ stays open (continue), a @block@ closes along with everything
-- nested inside it (break).
--
-- Both where to jump and how many records to retire were settled when
-- the program loaded, so neither is searched for here. What is checked
-- is that the records really are the ones the plan expects -- see
-- 'checkOwnScopes' -- and then retiring them is arithmetic on @ctrlSp@.
branchTo :: forall w. (IsWord w) => Int -> State (Wasm32St w) ()
branchTo pc = do
    Wasm32St{scopePlan = ScopePlan{spBranches}} <- get
    case spBranches !? pc of
        Nothing ->
            raiseInternalError $
                "control flow error: the branch at "
                    <> hexAddr 2 pc
                    <> " is not one the loaded program resolved"
        Just BranchPlan{bpTarget, bpPop, bpCheck} ->
            checkOwnScopes @w bpCheck >>= \case
                Left err -> raiseInternalError err
                Right () -> do
                    modify $ \st -> st{ctrlSp = ctrlSp st - bpPop * scopeRecordBytes @w}
                    setPc bpTarget

instance (IsWord w) => Machine (Wasm32St w) (Wasm32Isa w w) w where
    instructionFetch = do
        st <- get
        case st of
            Wasm32St{stopped = True} -> return $ Left halted
            Wasm32St{internalError = Just err} -> return $ Left err
            Wasm32St{pc, mem} ->
                case readInstruction mem pc of
                    Left err -> return $ Left err
                    Right (mem', instruction) -> do
                        put st{mem = mem'}
                        countMemAccess
                        return $ Right (pc, instruction)

    -- Resets 'memAccessCount' before the fetch, not just before the
    -- execute, so the instruction's own fetch counts toward the total.
    instructionStep = do
        modify $ \st -> st{memAccessCount = 0}
        (pc, instruction) <- either (error . ("internal error: " <>)) id <$> instructionFetch
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
            I32Eq -> compareS (==) >> nextPc instruction
            I32LtS -> compareS (<) >> nextPc instruction
            I32LeS -> compareS (<=) >> nextPc instruction
            I32GtS -> compareS (>) >> nextPc instruction
            I32GeS -> compareS (>=) >> nextPc instruction
            I32LtU -> compareU (<) >> nextPc instruction
            I32LeU -> compareU (<=) >> nextPc instruction
            I32GtU -> compareU (>) >> nextPc instruction
            I32GeU -> compareU (>=) >> nextPc instruction
            I32Load offset -> load (fromEnum offset) getWord id
            I32Load8U offset -> load (fromEnum offset) getByte fromIntegral
            I32Load8S offset -> load (fromEnum offset) getByte (fromIntegral . (fromIntegral :: Word8 -> Int8))
            I32Store offset -> store (fromEnum offset) setWord id
            I32Store8 offset -> store (fromEnum offset) setByte fromIntegral
            If -> do
                condition <- popValue
                if condition /= 0
                    then nextPc instruction
                    else withScopeTarget setPc
            Else -> withScopeTarget setPc
            End -> do
                -- Whether this @end@ closes a record is settled: an
                -- @if@'s @end@ closes nothing, since @if@ opened nothing.
                -- Comparing a record's @csEnd@ against @pc@ used to be
                -- how that was told apart, which cost reading the record.
                Wasm32St{scopePlan = ScopePlan{spEndPops}} <- get
                if not (pc `IntSet.member` spEndPops)
                    then nextPc instruction
                    else
                        -- One word, to be sure the record being retired
                        -- is a scope's and not some call's -- the same
                        -- thing 'checkOwnScopes' guards for a branch.
                        ctrlTopOf @w <$> get >>= \top ->
                            if top == nullAddr
                                then raiseInternalError "end: no open control record to close" >> nextPc instruction
                                else
                                    readScopeTagAt @w top >>= \case
                                        Left err -> raiseInternalError err
                                        Right CallTag ->
                                            raiseInternalError $
                                                "end at "
                                                    <> hexAddr 2 pc
                                                    <> " would close the enclosing function's own record"
                                        Right _ -> popScope @w >> nextPc instruction
            Loop ->
                withScopeTarget $ \endPc -> do
                    pushScope LoopScope{csStart = pc + byteSize instruction, csEnd = endPc}
                    nextPc instruction
            Block ->
                withScopeTarget $ \endPc -> do
                    pushScope BlockScope{csEnd = endPc}
                    nextPc instruction
            Br{} -> branchTo pc
            BrIf{} -> do
                condition <- popValue
                if condition == 0
                    then nextPc instruction
                    else branchTo pc
            Drop -> popValue @w >> nextPc instruction
            Select -> do
                condition <- popValue
                onFalse <- popValue
                onTrue <- popValue
                pushValue (if condition /= 0 then onTrue else onFalse)
                nextPc instruction
            Unreachable -> raiseInternalError "unreachable"
            LocalGet i -> do
                addr <- localAddr (fromEnum i)
                getWord addr >>= pushValue
                nextPc instruction
            LocalSet i -> do
                addr <- localAddr (fromEnum i)
                popValue >>= setWord addr
                nextPc instruction
            LocalTee i -> do
                addr <- localAddr (fromEnum i)
                value <- popValue
                setWord addr value
                pushValue value
                nextPc instruction
            LocalsReserve n' -> do
                Wasm32St{sp} <- get
                let n = fromEnum n'
                    step = byteSizeT @w
                    sp' = sp - n * step
                checkOperandRoom sp' $ do
                    -- Zeroed, like real WebAssembly's locals: memory
                    -- starts out randomised by default, so an unwritten
                    -- slot would otherwise read as noise, not 0.
                    forM_ [sp - step, sp - 2 * step .. sp'] $ \addr -> setWord addr 0
                    modify $ \st -> st{sp = sp', localCount = localCount st + n}
                nextPc instruction
            Call{callParamCount, callResultCount} -> do
                target <- popValue
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
                modify $ \st -> st{frameBase = calleeFrameBase, localCount = paramCount}
                setPc (fromEnum target)
            Return -> returnFromCall
            SpInit -> do
                addr <- fromEnum <$> popValue
                Wasm32St{mem} <- get
                let floor_ = operandFloor mem
                    ceiling_ = memCapacity mem
                if addr < floor_ || addr > ceiling_
                    then
                        raiseInternalError $
                            "sp.init: root "
                                <> hexAddr 2 addr
                                <> " is outside the stack region "
                                <> hexAddr 2 floor_
                                <> ".."
                                <> hexAddr 2 ceiling_
                    else do
                        -- Every field @initState@ sets from the root is set
                        -- again here, @localCount@ included: the frame this
                        -- lands in starts out with no locals, and leaving a
                        -- stale count behind would point the @locals@ and
                        -- @layout@ views at words that are not locals at
                        -- all. @frameBase@ keeps the same relation to @sp@
                        -- that 'initState' sets up: one word below, where
                        -- the first push lands (see 'localAddr').
                        modify $ \st ->
                            st
                                { sp = addr
                                , ctrlBase = addr
                                , ctrlSp = addr
                                , frameBase = addr - byteSizeT @w
                                , localCount = 0
                                }
                nextPc instruction
            Halt -> modify $ \st -> st{stopped = True}
        where
            -- \| A load: pop an address, add this instruction's own static
            -- offset, read the bytes there with @readAt@, and push them
            -- widened to a full value by @extend@.
            load :: forall a. Int -> (Int -> State (Wasm32St w) a) -> (a -> w) -> State (Wasm32St w) ()
            load offset readAt extend = do
                addr <- popValue
                raw <- readAt (fromEnum addr + offset)
                pushValue (extend raw)
                nextPc instruction
            -- \| A store, mirroring @load@: the value is on top, the
            -- address under it, and @narrow@ picks the bytes to write.
            store :: forall a. Int -> (Int -> a -> State (Wasm32St w) ()) -> (w -> a) -> State (Wasm32St w) ()
            store offset writeAt narrow = do
                value <- popValue
                addr <- popValue
                writeAt (fromEnum addr + offset) (narrow value)
                nextPc instruction
            -- \| Where this scope instruction jumps to, from the table
            -- 'initState' built. A miss means the instruction was not in
            -- the program that was loaded -- memory rewritten under the
            -- machine, say -- so it is reported rather than guessed at.
            withScopeTarget k = do
                Wasm32St{scopePlan = ScopePlan{spTargets}} <- get
                case spTargets !? pc of
                    Just target -> k target
                    Nothing ->
                        raiseInternalError $
                            "control flow error: the scope instruction at "
                                <> hexAddr 2 pc
                                <> " is not one the loaded program resolved"
            binary op = do
                y <- popValue
                x <- popValue
                pushValue (x `op` y)
            unsignedBinary op = do
                y <- fromSign <$> popValue
                x <- fromSign <$> popValue
                pushValue (op x y)
            -- \| wasm's @idiv_s@: truncates toward zero ('quot'), not toward
            -- negative infinity ('div'), and traps both on a zero divisor
            -- and on the single signed overflow (@minBound \/ -1@).
            signedDiv = do
                y <- popValue
                x <- popValue
                if
                    | y == 0 -> raiseInternalError "integer divide by zero"
                    | x == minBound && y == -1 -> raiseInternalError "integer overflow"
                    | otherwise -> pushValue (x `quot` y)
            -- \| wasm's @irem_s@: traps only on a zero divisor. Unlike
            -- @idiv_s@, @minBound % -1@ is defined (as 0) and must not
            -- trap -- 'rem' already answers 0 there without dividing.
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
