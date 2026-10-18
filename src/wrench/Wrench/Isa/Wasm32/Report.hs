{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Wrench.Isa.Wasm32.Report () where

import Data.String.Interpolate (i)
import Data.Text qualified as T
import Relude
import Relude.Extra (toPairs)
import Wrench.Isa.Wasm32.Instruction
import Wrench.Isa.Wasm32.State
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Report

instance (IsWord w) => Inspectable (Wasm32St w) where
    type WordOf (Wasm32St w) = w
    type IsaOf (Wasm32St w) = Wasm32Isa w w

    programCounter Wasm32St{pc} = pc
    memoryDump Wasm32St{mem} = mem
    ioStreams Wasm32St{mem = IoMem{mIoStreams}} = mIoStreams
    isHalted Wasm32St{stopped} = stopped
    reprState labels st v
        | Just v' <- defaultView labels st v = v'
    reprState labels st@Wasm32St{mem, sp, stackRoot, ctrlSp, frameBase, scanCache, branchCache, tosRegs} v =
        case T.splitOn ":" v of
            ["stack", f] -> formatValues f (stackWords view)
            ["locals", f] -> formatValues f (localWords view)
            ["scanCache", f] -> formatScans f
            ["branchCache", f] -> formatBranches f
            ["tosRegs", f] -> formatTos f
            ["layout", f] -> withFormat f (renderLayout labels view) Nothing
            ["layout", f, n] -> withFrameLimit n (withFormat f (renderLayout labels view))
            ["dump", f] -> withFormat f (renderDump labels view) Nothing
            ["dump", f, n] -> withFrameLimit n (withFormat f (renderDump labels view))
            [r] -> reprState labels st [i|#{r}:dec|]
            ["sp", f] -> viewRegister f sp
            ["stackRoot", f] -> viewRegister f stackRoot
            ["ctrlSp", f] -> viewRegister f ctrlSp
            ["frameBase", f] -> viewRegister f frameBase
            [r, _] -> unknownView r
            _ -> errorView v
        where
            view = stackView st

            formatValues "dec" vs = toText $ intercalate ":" $ map show vs
            formatValues "hex" vs = T.intercalate ":" $ map (toText . word32ToHex) vs
            formatValues f _ = unknownFormat f

            formatScans "dec" = T.intercalate ", " [[i|#{start}->#{end}|] | (start, end) <- scanCache]
            formatScans "hex" = T.intercalate ", " [[i|#{hexAddr' start}->#{hexAddr' end}|] | (start, end) <- scanCache]
            formatScans f = unknownFormat f
            formatBranches "dec" = T.intercalate ", " [[i|#{beFrom}->#{beTo}|] | BranchEntry{beFrom, beTo} <- branchCache]
            formatBranches "hex" = T.intercalate ", " [[i|#{hexAddr' beFrom}->#{hexAddr' beTo}|] | BranchEntry{beFrom, beTo} <- branchCache]
            formatBranches f = unknownFormat f
            formatTos "dec" = T.intercalate ", " [[i|#{addr}=#{value}|] | (addr, value) <- tosRegs]
            formatTos "hex" = T.intercalate ", " [[i|#{hexAddr' addr}=#{word32ToHex value}|] | (addr, value) <- tosRegs]
            formatTos f = unknownFormat f
            hexAddr' = hexAddr (hexAddrWidth (memCapacity mem))

            withFormat "dec" render limit =
                render LayoutFormat{lfWord = show, lfAddr = show, lfFrameLimit = limit}
            withFormat "hex" render limit =
                render
                    LayoutFormat
                        { lfWord = toText . word32ToHex
                        , lfAddr = hexAddr (hexAddrWidth (memCapacity mem))
                        , lfFrameLimit = limit
                        }
            withFormat f _ _ = unknownFormat f

            withFrameLimit n k = maybe (unknownFormat n) (k . Just) (readMaybe (toString n))

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
            either (const Nothing) Just . decodeScope =<< mapM (wordAt mem) (scopeWordAddrs @w addr)

applyMarkers :: [(Text, Text)] -> Text -> Text
applyMarkers marks = T.intercalate "\n" . map mark . T.lines
    where
        mark line = maybe line (\name -> [i|#{line} [#{name}]|]) (findName (T.stripStart line))
        findName trimmed = listToMaybe [name | (prefix, name) <- marks, prefix `T.isPrefixOf` trimmed]

renderLayout ::
    forall isa w.
    (ByteSize isa, IsWord w) =>
    HashMap Text w
    -> StackView isa w
    -> LayoutFormat w
    -> Text
renderLayout labels view LayoutFormat{lfWord = showWord, lfAddr = showAddr, lfFrameLimit = frameLimit} =
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

        kept = maybe tagged (\n -> drop (max 0 (length tagged - n)) tagged) frameLimit
        omittedNote
            | n <- length tagged - length kept
            , n > 0 =
                [[i|  (#{n} earlier frame(s) omitted)|]]
            | otherwise = []

        funcNameAt addr =
            maybe "?" snd $ foldl' nearer Nothing $ toPairs labels
            where
                nearer best (label, labelAddr)
                    | a <- fromEnum labelAddr
                    , a <= addr
                    , maybe True ((< a) . fst) best =
                        Just (a, label)
                    | otherwise = best

        renderFrame (frameNo, isLive, caller, FrameLayout{flPc, flFrameBase, flLocalCount, flScanLower, flControls}) =
            header <> localsLines <> controlLines <> operandLines
            where
                livePointers =
                    [("sp", svSp) | isLive]
                        <> [("frameBase", svFrameBase) | isLive, flLocalCount > 0]
                pointerTags addr = T.concat [[i| [#{name :: Text}]|] | (name, ptr) <- livePointers, ptr == addr]
                liveNote = if isLive then ", live" else "" :: Text
                header = [[i|\##{frameNo} #{funcNameAt flPc} (pc=#{showAddr flPc}#{liveNote})|]]
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

renderDump ::
    forall isa w.
    (ByteSize isa, IsWord w, Show isa) =>
    HashMap Text w
    -> StackView isa w
    -> LayoutFormat w
    -> Text
renderDump labels view@StackView{svMem, svPc, svCtrlSp} format =
    applyMarkers marks
        $ T.intercalate "\n"
        $ filter
            (not . T.null)
            [ renderLayout labels view format
            , dumpRange svCtrlSp (memCapacity svMem)
            ]
    where
        marks =
            [ ([i|mem[#{lfAddr format svPc}..|], "pc")
            , ([i|mem[#{lfAddr format svCtrlSp}..|], "ctrlSp")
            ]
        dumpRange lo hi =
            prettyDump True (lfAddr format) labels (fromList (sliceMem [lo .. hi - 1] (dumpCells svMem)))
