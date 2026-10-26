{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Wrench.Isa.Wasm32.Report () where

import Data.String.Interpolate (i)
import Data.Text qualified as T
import Relude
import Relude.Extra (insertWith, toPairs, (!?))
import Wrench.Isa.Wasm32.Instruction
import Wrench.Isa.Wasm32.State
import Wrench.Machine.BitPack (unpackWords)
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Report
import Wrench.Report.View (Format (..), View, format, lit, mkView, optArg)

instance (IsWord w) => Inspectable (Wasm32St w) where
    type WordOf (Wasm32St w) = w
    type IsaOf (Wasm32St w) = Wasm32Isa w w

    programCounter Wasm32St{pc} = pc
    memoryDump Wasm32St{mem} = mem
    ioStreams Wasm32St{mem = IoMem{mIoStreams}} = mIoStreams
    isHalted Wasm32St{stopped} = stopped
    views = wasm32Views

wasm32Views :: (IsWord w) => [View (StateEnv (Wasm32St w))]
wasm32Views =
    [ values "stack" "Operand stack of the current frame, top first: `a:b:c`." stackWords
    , values "locals" "Locals of the current frame, `$0` first." localWords
    , layout
        "stack:layout"
        "Call frames, outermost first: locals, open scopes, operands; `<frames>` keeps only the innermost ones."
        renderStackLayout
    , layout
        "stack:dump"
        "Stack memory word by word, by address, with pointer tags; `<frames>` keeps only the innermost frames."
        renderStackDump
    , valueView "sp" "Address of the topmost operand." (sp . seState)
    , valueView "frame-base" "Address of local 0 of the current frame." (frameBase . seState)
    , valueView "ctrl-sp" "Top of the control stack." (ctrlSp . seState)
    , valueView "stack-root" "Where both stacks start, set by `sp.init`." (stackRoot . seState)
    , mkView
        (lit "cache:scan" *> format [Dec, Hex] Dec)
        "The last four `else`/`end` scans, most recent first: `start->end`."
        $ \StateEnv{seState = Wasm32St{scanCache, mem}} fmt ->
            let addr = if fmt == Hex then hexAddr (hexAddrWidth (memCapacity mem)) else show
             in T.intercalate ", " [addr start <> "->" <> addr end | (start, end) <- scanCache]
    , mkView
        (lit "cache:branch" *> format [Dec, Hex] Dec)
        "The last four taken branches, most recent first: `from->to`."
        $ \StateEnv{seState = Wasm32St{branchCache, mem}} fmt ->
            let addr = if fmt == Hex then hexAddr (hexAddrWidth (memCapacity mem)) else show
             in T.intercalate ", " [addr beFrom <> "->" <> addr beTo | BranchEntry{beFrom, beTo} <- branchCache]
    , mkView
        (lit "stack:tos-regs" *> format [Dec, Hex] Dec)
        "The top-of-stack registers, the last pushed first: `slot=value`."
        $ \StateEnv{seState = Wasm32St{tosRegs, mem}} fmt ->
            let slot = if fmt == Hex then hexAddr (hexAddrWidth (memCapacity mem)) else show
                value = if fmt == Hex then toText . word32ToHex else show
             in T.intercalate ", " [slot addr <> "=" <> value v | (addr, v) <- tosRegs]
    ]
    where
        values name doc f = mkView (lit name *> format [Dec, Hex] Dec) doc $ \StateEnv{seState} fmt ->
            showValues fmt (f (stackView seState))
        layout name doc render = mkView (lit name *> ((,) <$> format [Dec, Hex] Dec <*> optArg "frames")) doc $
            \StateEnv{seLabels, seState} (fmt, limit) -> render seLabels (stackView seState) (layoutFormat seState fmt limit)
        layoutFormat Wasm32St{mem} fmt limit
            | fmt == Hex =
                LayoutFormat
                    { lfWord = toText . word32ToHex
                    , lfAddr = hexAddr (hexAddrWidth (memCapacity mem))
                    , lfFrameLimit = limit
                    }
            | otherwise = LayoutFormat{lfWord = show, lfAddr = show, lfFrameLimit = limit}

stackView :: (IsWord w) => Wasm32St w -> StackView (Wasm32Isa w w) w
stackView st@Wasm32St{mem, pc, sp, ctrlSp, frameBase, localCount} =
    StackView
        { svMem = mem
        , svPc = pc
        , svSp = sp
        , svFrameBase = frameBase
        , svLocalCount = localCount
        , svCtrlSp = ctrlSp
        , svScopeAddrs = scopeAddrs st
        }

data LayoutFormat w = LayoutFormat
    { lfWord :: w -> Text
    , lfAddr :: Int -> Text
    , lfFrameLimit :: Maybe Int
    }

data StackView isa w = StackView
    { svMem :: IoMem isa w
    , svPc :: Int
    , svSp :: Int
    , svFrameBase :: Int
    , svLocalCount :: Int
    , svCtrlSp :: Int
    , svScopeAddrs :: [Int]
    }

wordAt :: (ByteSize isa, IsWord w) => IoMem isa w -> Int -> Maybe w
wordAt mem addr = either (const Nothing) (Just . snd) (readWord mem addr)

frameStackBase :: forall isa w. (IsWord w) => StackView isa w -> Int
frameStackBase StackView{svFrameBase, svLocalCount} = svFrameBase - svLocalCount * byteSizeT @w

stackWords :: forall isa w. (ByteSize isa, IsWord w) => StackView isa w -> [w]
stackWords view@StackView{svMem, svSp} =
    mapMaybe (wordAt svMem) [svSp, svSp + step .. frameStackBase view]
    where
        step = byteSizeT @w

localWords :: forall isa w. (ByteSize isa, IsWord w) => StackView isa w -> [w]
localWords view@StackView{svMem, svFrameBase} =
    mapMaybe (wordAt svMem) [svFrameBase, svFrameBase - step .. frameStackBase view + step]
    where
        step = byteSizeT @w

data FrameLayout = FrameLayout
    { flPc :: Int
    , flFrameBase :: Int
    , flLocalCount :: Int
    , flScanLower :: Int
    , flControls :: [(Int, Int, Scope)]
    }

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
            either (const Nothing) Just . unpackWords =<< mapM (wordAt mem) (scopeWordAddrs @w addr)

keepLast :: Maybe Int -> [a] -> [a]
keepLast limit xs = maybe xs (\n -> drop (max 0 (length xs - n)) xs) limit

funcNameAt :: (IsWord w) => HashMap Text w -> Int -> Text
funcNameAt labels addr =
    maybe "?" snd $ foldl' nearer Nothing $ toPairs labels
    where
        nearer best (label, labelAddr)
            | a <- fromEnum labelAddr
            , a <= addr
            , maybe True ((< a) . fst) best =
                Just (a, label)
            | otherwise = best

renderStackLayout ::
    forall isa w.
    (ByteSize isa, IsWord w) =>
    HashMap Text w
    -> StackView isa w
    -> LayoutFormat w
    -> Text
renderStackLayout labels view LayoutFormat{lfWord = showWord, lfAddr = showAddr, lfFrameLimit = frameLimit} =
    T.intercalate "\n" $ omittedNote <> concatMap renderFrame kept
    where
        step = byteSizeT @w
        StackView{svMem, svSp, svFrameBase} = view

        frames = reverse (walkFrames view)
        liveIndex = length frames - 1

        localsRange FrameLayout{flFrameBase, flLocalCount}
            | flLocalCount == 0 = Nothing
            | otherwise = Just (flFrameBase - flLocalCount * step + step, flFrameBase + step)
        tagged =
            [ (frameNo, frameNo == liveIndex, caller, frame)
            | (frameNo, caller, frame) <- zip3 [0 :: Int ..] (Nothing : map Just frames) frames
            ]

        kept = keepLast frameLimit tagged
        omittedNote
            | n <- length tagged - length kept
            , n > 0 =
                [[i|  (#{n} earlier frame(s) omitted)|]]
            | otherwise = []

        renderFrame (frameNo, isLive, caller, FrameLayout{flPc, flFrameBase, flLocalCount, flScanLower, flControls}) =
            header <> localsLines <> controlLines <> operandLines
            where
                livePointers =
                    [("sp", svSp) | isLive]
                        <> [("frameBase", svFrameBase) | isLive, flLocalCount > 0]
                pointerTags addr = T.concat [[i| [#{name :: Text}]|] | (name, ptr) <- livePointers, ptr == addr]
                liveNote = if isLive then ", live" else "" :: Text
                header = [[i|\##{frameNo} #{funcNameAt labels flPc} (pc=#{showAddr flPc}#{liveNote})|]]
                opTop = flFrameBase - flLocalCount * step
                localsLines
                    | flLocalCount == 0 = []
                    | otherwise = indexedSpan (opTop + step) (flFrameBase + step) "locals" aliasNote

                operandTop = opTop + step
                operandLines
                    | flScanLower >= operandTop = [[i|  (operands): empty, base=#{showAddr operandTop}|]]
                    | otherwise = indexedSpan flScanLower operandTop "(operands)" (const "")

                aliasNote addr = case caller of
                    Just c@FrameLayout{flFrameBase = callerBase}
                        | Just (lo, hi) <- localsRange c
                        , addr >= lo && addr < hi ->
                            [i| [also \##{frameNo - 1} local #{(callerBase - addr) `div` step}]|]
                    _ -> ""
                controlLines = concatMap controlSpan flControls
                controlSpan (lo, hi, scope) =
                    let (firstLine, restLines) = describeScope showAddr scope
                     in [i|  mem[#{showAddr lo}..#{showAddr (hi - 1)}]: #{firstLine}|] : map ("    " <>) restLines

                indexedSpan lo hi tag note =
                    [i|  mem[#{showAddr lo}..#{showAddr (hi - 1)}]: #{tag :: Text}|]
                        : zipWith (indexedWord note) [0 :: Int ..] [hi - step, hi - 2 * step .. lo]
                indexedWord note idx addr =
                    [i|    #{showAddr addr}..#{showAddr (addr + step - 1)}: #{maybe "?" showWord (wordAt svMem addr)} (#{idx})#{pointerTags addr}#{note addr :: Text}|]

describeScope :: (Int -> Text) -> Scope -> (Text, [Text])
describeScope showAddr scope = case scope of
    LoopScope{csStart, csEnd} ->
        ([i|LoopScope { csStart = #{showAddr csStart}, csEnd = #{showAddr csEnd} }|], [])
    BlockScope{csEnd} ->
        ([i|BlockScope { csEnd = #{showAddr csEnd} }|], [])
    CallScope{csCallerFrameBase, csCallerLocalCount, csReturnPc, csResultCount} ->
        ( "CallScope"
        ,
            [ [i|{ csCallerFrameBase = #{showAddr csCallerFrameBase}|]
            , [i|, csCallerLocalCount = #{csCallerLocalCount}|]
            , [i|, csReturnPc = #{showAddr csReturnPc}|]
            , [i|, csResultCount = #{csResultCount} }|]
            ]
        )

renderStackDump ::
    forall isa w.
    (ByteSize isa, IsWord w, Show isa) =>
    HashMap Text w
    -> StackView isa w
    -> LayoutFormat w
    -> Text
renderStackDump labels view LayoutFormat{lfWord = showWord, lfAddr = showAddr, lfFrameLimit = frameLimit} =
    T.intercalate "\n" $ fillFrom (max 0 (foldl' min svSp (map itemLo items))) (sortWith itemLo items)
    where
        step = byteSizeT @w
        StackView{svMem, svSp, svFrameBase, svLocalCount, svCtrlSp} = view
        memEnd = memCapacity svMem

        frames = zip [0 :: Int ..] (reverse (walkFrames view))
        kept = keepLast frameLimit frames
        omitted = length frames - length kept

        pointers =
            [("sp", svSp)]
                <> [("frameBase", svFrameBase) | svLocalCount > 0]
                <> [("ctrlSp", svCtrlSp)]
        pointerTags addr = T.concat [[i| [#{name :: Text}]|] | (name, ptr) <- pointers, ptr == addr]
        tagFirst addr = \case
            line : rest -> (line <> pointerTags addr) : rest
            [] -> []

        owner (frameNo, FrameLayout{flPc}) = [i|\##{frameNo} #{funcNameAt labels flPc}|] :: Text
        frameLocals frame@(_, FrameLayout{flFrameBase, flLocalCount}) =
            [(flFrameBase - idx * step, [i|#{owner frame} local #{idx}|]) | idx <- [0 .. flLocalCount - 1]]
        frameOperands frame@(_, FrameLayout{flFrameBase, flLocalCount, flScanLower}) =
            zipWith
                (\idx addr -> (addr, [i|#{owner frame} operand #{idx}|]))
                [0 :: Int ..]
                [opTop, opTop - step .. flScanLower]
            where
                opTop = flFrameBase - flLocalCount * step
        roles :: IntMap [Text]
        roles =
            foldl'
                (\m (addr, role) -> insertWith (flip (<>)) addr [role] m)
                mempty
                (concatMap (\frame -> frameLocals frame <> frameOperands frame) (reverse frames))

        items = concat (snd (mapAccumL frameItems [] (reverse kept))) <> recordItems <> omittedItem
        itemLo (lo, _, _) = lo
        frameItems shown frame =
            let fresh = filter ((`notElem` shown) . fst)
                locals = sortWith fst (fresh (frameLocals frame))
                operands = fresh (frameOperands frame)
             in ( map fst (locals <> operands) <> shown
                , localsItem frame (map fst locals) <> [(addr, addr + step, [wordLine addr]) | (addr, _) <- operands]
                )
        localsItem _ [] = []
        localsItem frame addrs@(lo : _) =
            let hi = foldl' max lo addrs + step
             in [(lo, hi, [i|mem[#{showAddr lo}..#{showAddr (hi - 1)}]: #{owner frame} locals|] : map (("    " <>) . wordLine) addrs)]
        wordLine addr =
            let value = maybe "?" showWord (wordAt svMem addr)
                owners = T.intercalate ", " (fromMaybe [] (roles !? addr))
             in [i|mem[#{showAddr addr}..#{showAddr (addr + step - 1)}]: #{value}  #{owners}#{pointerTags addr}|]
        recordItems =
            [ ( lo
              , hi
              , [i|mem[#{showAddr lo}..#{showAddr (hi - 1)}]: #{firstLine}  #{owner frame}#{pointerTags lo}|]
                    : map ("    " <>) restLines
              )
            | frame@(_, FrameLayout{flControls}) <- kept
            , (lo, hi, scope) <- flControls
            , let (firstLine, restLines) = describeScope showAddr scope
            ]
        omittedItem = case kept of
            (_, FrameLayout{flFrameBase, flControls = (callLo, _, _) : _}) : _
                | omitted > 0 ->
                    let lo = flFrameBase + step
                     in [(lo, callLo, [[i|mem[#{showAddr lo}..#{showAddr (callLo - 1)}]: (#{omitted} earlier frame(s) omitted)|]])]
            _ -> []

        fillFrom cursor [] = raw cursor memEnd
        fillFrom cursor ((lo, hi, itemLines) : rest) =
            raw cursor lo <> itemLines <> fillFrom (max cursor hi) rest

        raw lo hi =
            concat
                [ tagFirst a $ T.lines $ prettyDump True showAddr labels $ fromList $ sliceMem [a .. b - 1] (dumpCells svMem)
                | let lo' = max 0 lo
                      hi' = min memEnd hi
                      bounds = sortNub ([lo', hi'] <> [p | (_, p) <- pointers, lo' < p, p < hi'])
                , lo' < hi'
                , (a, b) <- zip bounds (drop 1 bounds)
                ]
