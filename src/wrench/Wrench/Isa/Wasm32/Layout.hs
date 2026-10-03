{- | The @stack@, @locals@, @layout@ and @dump@ report views: everything
that turns the live stacks back into something readable. Pure throughout
-- a 'StackView' is all it takes, so none of this has to go through the
interpreter's own state or its memory-access accounting.
-}
module Wrench.Isa.Wasm32.Layout (
    StackView (..),
    stackWords,
    localWords,
    renderLayout,
    renderDump,
) where

import Data.Text qualified as T
import Relude
import Relude.Extra (toPairs)
import Wrench.Isa.Wasm32.ControlRecord
import Wrench.Machine.Memory
import Wrench.Machine.Types

-- | Everything the stack views need out of the machine: the memory, the
-- pointers bounding the two live stacks, and where the open control
-- records are. Taking this rather than the interpreter's whole state is
-- what keeps these views a pure function of memory and a few numbers.
data StackView isa w = StackView
    { svMem :: IoMem isa w
    , svPc :: Int
    -- ^ The live `pc`, for naming the function each frame is in.
    , svSp :: Int
    , svFrameBase :: Int
    , svLocalCount :: Int
    , svCtrlSp :: Int
    -- ^ The control stack's frontier.
    , svCtrlBase :: Int
    -- ^ The root both stacks grow from.
    , svDataTop :: Int
    -- ^ Where code+data end and the stack region begins.
    , svScopeAddrs :: [Int]
    -- ^ Open control records, innermost first.
    }

-- | A pure word read, for views that render whatever is there and print
-- @?@ for what they can't reach.
wordAt :: (ByteSize isa, IsWord w) => IoMem isa w -> Int -> Maybe w
wordAt mem addr = either (const Nothing) (Just . snd) (readWord mem addr)

-- | The address of this frame's oldest operand: locals count down from
-- @svFrameBase@, so the first word past them is the first operand.
frameStackBase :: forall isa w. (IsWord w) => StackView isa w -> Int
frameStackBase StackView{svFrameBase, svLocalCount} = svFrameBase - svLocalCount * byteSizeT @w

-- | The active frame's operand stack, top first -- `sp` already names the
-- top under the descending-stack convention, and 'frameStackBase' names
-- the oldest operand, so the range needs no offset at either end.
stackWords :: forall isa w. (ByteSize isa, IsWord w) => StackView isa w -> [w]
stackWords view@StackView{svMem, svSp} =
    mapMaybe (wordAt svMem) [svSp, svSp + step .. frameStackBase view]
    where
        step = byteSizeT @w

-- | The active frame's locals, in index order.
localWords :: forall isa w. (ByteSize isa, IsWord w) => StackView isa w -> [w]
localWords view@StackView{svMem, svFrameBase} =
    mapMaybe (wordAt svMem) [svFrameBase, svFrameBase - step .. frameStackBase view + step]
    where
        step = byteSizeT @w

-- | One active call's own slice of the stacks, as address ranges only.
data FrameLayout = FrameLayout
    { flPc :: Int
    -- ^ Where this frame is: the live `pc` for the innermost frame, or
    -- the paused return address (a shallower 'CallScope's own
    -- @csReturnPc@) for every frame below it.
    , flFrameBase :: Int
    , flLocalCount :: Int
    , flScanLower :: Int
    -- ^ Inclusive lower bound of this frame's own visible region --
    -- `sp` for the innermost frame, or one word above the next (deeper)
    -- frame's own 'flFrameBase' otherwise, since everything at or below
    -- that belongs to the call this frame made, not to this frame.
    , flControls :: [(Int, Int, Scope)]
    -- ^ This frame's own open records -- any `block`\/`loop` it has
    -- open, plus (last) the 'CallScope' that entered it, if any --
    -- address-ascending, each as @(start, end, scope)@.
    }

-- | Split the stacks into per-call frames, innermost\/live one first.
-- The frame boundaries are the 'CallScope's in @svScopeAddrs@, which is why
-- this walks that list rather than slicing it by address.
walkFrames :: forall isa w. (ByteSize isa, IsWord w) => StackView isa w -> [FrameLayout]
walkFrames StackView{svMem, svPc, svSp, svFrameBase, svLocalCount, svScopeAddrs} =
    go svFrameBase svLocalCount svPc svSp svScopeAddrs
    where
        step = byteSizeT @w

        go fb lc p scanLower addrs =
            let (controls, next) = collectControls addrs
                frame =
                    FrameLayout
                        { flPc = p
                        , flFrameBase = fb
                        , flLocalCount = lc
                        , flScanLower = scanLower
                        , flControls = sortOn (\(s, _, _) -> s) controls
                        }
             in frame : case next of
                    Nothing -> []
                    Just (fb', lc', p', outer) -> go fb' lc' p' (fb + step) outer

        -- \| Take every open `block`/`loop` from the head of @addrs@ as
        -- this frame's own, stopping at (and including) the 'CallScope'
        -- that entered this frame -- whose fields are what the next,
        -- shallower frame needs. 'Nothing' once the list runs out, or at
        -- a record that doesn't decode, which a view renders around.
        collectControls [] = ([], Nothing)
        collectControls (addr : outer) = case readScopeFrom svMem addr of
            Nothing -> ([], Nothing)
            Just scope ->
                let span_ = (addr, addr + scopeRecordBytes @w, scope)
                 in case scope of
                        CallScope{csCallerFrameBase, csCallerLocalCount, csReturnPc} ->
                            ([span_], Just (csCallerFrameBase, csCallerLocalCount, csReturnPc, outer))
                        _ ->
                            let (rest, result) = collectControls outer
                             in (span_ : rest, result)

        readScopeFrom mem addr =
            either (const Nothing) Just . decodeScope @w =<< mapM (wordAt mem) (scopeWordAddrs @w addr)

-- | One line per contiguous span of a frame's visible range, push-order
-- (highest address first) within each frame and from the outermost frame
-- down to the live one -- a memory dump annotated with what each span
-- is: a frame's locals, one of its open control records (decoded), or a
-- run of operand values.
--
-- @frameLimit@ keeps only the most recent N frames, for when a deep call
-- stack would repeat the same shape once per suspended caller.
renderLayout ::
    forall isa w.
    (ByteSize isa, IsWord w) =>
    HashMap Text w
    -> StackView isa w
    -> (w -> Text)
    -> (Int -> Text)
    -> Maybe Int
    -> Text
renderLayout labels view showWord showAddr frameLimit =
    T.intercalate "\n" $ omittedNote <> concatMap renderFrame kept
    where
        step = byteSizeT @w
        StackView{svMem} = view

        frames = reverse (walkFrames view)
        liveIndex = length frames - 1
        tagged = zipWith (\i frame -> (i, i == liveIndex, frame)) [0 :: Int ..] frames
        -- Keep the last N: frames is outermost-first, so that's the tail.
        kept = maybe tagged (\n -> drop (max 0 (length tagged - n)) tagged) frameLimit
        omittedNote
            | n <- length tagged - length kept
            , n > 0 =
                ["  (" <> show n <> " earlier frame(s) omitted)"]
            | otherwise = []

        -- \| The innermost label at or before @addr@ -- "which function"
        -- a frame paused (or sitting) at @addr@ is in, assuming every
        -- function starts at its own label.
        funcNameAt addr =
            fromMaybe "?"
                $ viaNonEmpty last
                $ map snd
                $ sortOn fst
                $ filter ((<= addr) . fst)
                $ map (\(l, a) -> (fromEnum a, l))
                $ toPairs labels

        -- \| A suspended frame gets a @#N funcName (pc=...)@ header: that
        -- @pc@ is the @csReturnPc@ in the 'CallScope' that suspended it,
        -- so it describes something really in memory. The live frame gets
        -- none -- its @pc@ lives only in the interpreter's state, and
        -- @{pc}@ already shows it.
        renderFrame (i, isLive, FrameLayout{flPc, flFrameBase, flLocalCount, flScanLower, flControls}) =
            header <> localsLines <> controlLines <> operandLines
            where
                header
                    | isLive = []
                    | otherwise = ["#" <> show i <> " " <> funcNameAt flPc <> " (pc=" <> showAddr flPc <> ")"]
                opTop = flFrameBase - flLocalCount * step
                localsLines
                    | flLocalCount == 0 = []
                    | otherwise = indexedSpan (opTop + step) (flFrameBase + step) "locals"
                -- Exclusive upper bound of the operand span: one word
                -- above this frame's oldest operand, which sits directly
                -- below its locals whether or not it was entered by a
                -- `call`.
                operandTop = opTop + step
                operandLines
                    | flScanLower >= operandTop = ["  (operands): empty, base=" <> showAddr operandTop]
                    | otherwise = indexedSpan flScanLower operandTop "(operands)"
                controlLines = concatMap controlSpan flControls
                controlSpan (lo, hi, scope) =
                    ["  mem[" <> showAddr lo <> ".." <> showAddr (hi - 1) <> "]: " <> describeScope showAddr scope]
                -- \| The span's address range, then one indented
                -- "index: value" line per word, highest address first --
                -- param index order for locals, push order for operands.
                indexedSpan lo hi tag =
                    ("  mem[" <> showAddr lo <> ".." <> showAddr (hi - 1) <> "]: " <> tag)
                        : zipWith
                            (\idx a -> "    " <> show (idx :: Int) <> ": " <> maybe "?" showWord (wordAt svMem a))
                            [0 :: Int ..]
                            [hi - step, hi - 2 * step .. lo]

-- | The whole configured memory in address order: code and @.data@, the
-- free space below the operand stack, the live stacks themselves (via
-- 'renderLayout', which decodes the control records), then the free
-- space above the control stack.
renderDump ::
    forall isa w.
    (ByteSize isa, IsWord w, Show isa) =>
    HashMap Text w
    -> StackView isa w
    -> (w -> Text)
    -> (Int -> Text)
    -> Maybe Int
    -> Text
renderDump labels view@StackView{svMem, svSp, svCtrlSp, svDataTop} showWord showAddr frameLimit =
    T.intercalate "\n" $
        filter
            (not . T.null)
            [ dumpRange 0 svDataTop
            , dumpRange svDataTop svSp
            , renderLayout labels view showWord showAddr frameLimit
            , dumpRange svCtrlSp (memCapacity svMem)
            ]
    where
        dumpRange lo hi = prettyDump True showAddr labels (fromList (sliceMem [lo .. hi - 1] (dumpCells svMem)))
