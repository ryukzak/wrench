{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | A small WebAssembly-inspired 32-bit virtual ISA for Wrench.
module Wrench.Isa.Wasm32 (
    Isa (..),
    WasmToken (..),
    FunctionMeta (..),
    FunctionTable,
    MachineState (..),
    Wasm32State,
    initWasm32State,
    translateWasm32,
    resolveWasmLabels,
    resolveWasmLocals,
) where

import Control.Monad (foldM)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Default (def)
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Relude
import Relude.Unsafe qualified as Unsafe
import Text.Megaparsec (anySingle, choice, parse, try)
import Text.Megaparsec.Char (char, hspace, hspace1, string)
import Text.Megaparsec.Error (errorBundlePretty)
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Report
import Wrench.Translator
import Wrench.Translator.Parser
import Wrench.Translator.Parser.Misc
import Wrench.Translator.Parser.Types
import Wrench.Translator.Types

-- | @cl@ is the representation of a structured-control label
-- (`block`/`loop`/`if`/`br`/`br_if`'s payload): 'Text' straight out of the
-- parser, 'Int' once resolved (see 'resolveWasmLabels'). @vl@ is the same
-- thing for a local variable reference (`local.get`/`local.set`/
-- `local.tee`'s payload; see 'resolveWasmLocals'). The two resolve
-- independently -- neither cares what phase the other is in. @l@ is,
-- again, the same idea for a `call` target, resolved by the shared
-- 'DerefMnemonic' machinery instead.
data Isa cl vl w l
    = I32Const l
    | Drop
    | Select
    | LocalGet vl
    | LocalSet vl
    | LocalTee vl
    | I32Add
    | I32Sub
    | I32Mul
    | I32DivS
    | I32DivU
    | I32RemS
    | I32RemU
    | I32And
    | I32Or
    | I32Xor
    | I32Shl
    | I32ShrS
    | I32ShrU
    | I32Eqz
    | I32Eq
    | I32Ne
    | I32LtS
    | I32LeS
    | I32GtS
    | I32GeS
    | I32LtU
    | I32LeU
    | I32GtU
    | I32GeU
    | I32Load
    | I32Store
    | I32Load8S
    | I32Load8U
    | I32Store8
    | Block cl
    | Loop cl
    | If cl
    | Else
    | End
    | Br cl
    | BrIf cl
    | Call l
    | Return
    | Halt
    | Unreachable
    | Nop
    | -- | Function metadata, embedded in code right before the body: param
      -- count, declared (non-parameter) local count, result count. Emitted
      -- by lowering a source @.func@ directive; never reached by ordinary
      -- fallthrough, only read directly by 'Call'.
      FuncEnter Int Int Int
    deriving (Eq, Show)

data FunctionMeta = FunctionMeta
    { fmParamCount :: !Int
    , fmLocalNames :: ![String]
    , fmResultCount :: !Int
    }
    deriving (Eq, Show)

type FunctionTable = IntMap FunctionMeta

-- | What the parser produces for one line. Almost everything is a real
-- 'Isa' instruction with its control labels (@cl@) and local references
-- (@vl@) still as 'Text', unresolved -- 'Insn' just wraps it. The one
-- exception is @.func@: its `(param a b) (local c d)` declares a whole
-- *name list*, which has no home in 'Isa' at all (its @cl@\/@vl@ fields
-- each hold a single reference, not a list) -- 'FuncDecl' is that name
-- list's only carrier, from here until 'stripFuncDecls' peels it off (see
-- there for what happens to it). @.endfunc@ needs no carrier of its own:
-- it's parsed straight to @Insn Return@, matching what it's always
-- actually meant (an implicit return at the end of a function body, same
-- as falling off the end in real Wasm).
data WasmToken w l
    = Insn (Isa Text Text w l)
    | FuncDecl FunctionMeta
    deriving (Show)

instance CommentStart (WasmToken w l) where
    commentStart = ";"

instance (MachineWord w) => MnemonicParser (WasmToken w (Ref w)) where
    mnemonic =
        hspace *> cmd <* eol' (commentStart @(WasmToken _ _))
        where
            cmd =
                choice
                    [ FuncDecl <$> try funcDecl
                    , Insn Return <$ try endFunc
                    , Insn . I32Const <$> cmd1 "i32.const" referenceWithDirective
                    , cmd0 "drop" (Insn Drop)
                    , cmd0 "select" (Insn Select)
                    , Insn . LocalGet <$> cmd1 "local.get" localId
                    , Insn . LocalSet <$> cmd1 "local.set" localId
                    , Insn . LocalTee <$> cmd1 "local.tee" localId
                    , cmd0 "i32.add" (Insn I32Add)
                    , cmd0 "i32.sub" (Insn I32Sub)
                    , cmd0 "i32.mul" (Insn I32Mul)
                    , cmd0 "i32.div_s" (Insn I32DivS)
                    , cmd0 "i32.div_u" (Insn I32DivU)
                    , cmd0 "i32.rem_s" (Insn I32RemS)
                    , cmd0 "i32.rem_u" (Insn I32RemU)
                    , cmd0 "i32.and" (Insn I32And)
                    , cmd0 "i32.or" (Insn I32Or)
                    , cmd0 "i32.xor" (Insn I32Xor)
                    , cmd0 "i32.shl" (Insn I32Shl)
                    , cmd0 "i32.shr_s" (Insn I32ShrS)
                    , cmd0 "i32.shr_u" (Insn I32ShrU)
                    , cmd0 "i32.eqz" (Insn I32Eqz)
                    , cmd0 "i32.eq" (Insn I32Eq)
                    , cmd0 "i32.ne" (Insn I32Ne)
                    , cmd0 "i32.lt_s" (Insn I32LtS)
                    , cmd0 "i32.le_s" (Insn I32LeS)
                    , cmd0 "i32.gt_s" (Insn I32GtS)
                    , cmd0 "i32.ge_s" (Insn I32GeS)
                    , cmd0 "i32.lt_u" (Insn I32LtU)
                    , cmd0 "i32.le_u" (Insn I32LeU)
                    , cmd0 "i32.gt_u" (Insn I32GtU)
                    , cmd0 "i32.ge_u" (Insn I32GeU)
                    , cmd0 "i32.load8_s" (Insn I32Load8S)
                    , cmd0 "i32.load8_u" (Insn I32Load8U)
                    , cmd0 "i32.load" (Insn I32Load)
                    , cmd0 "i32.store8" (Insn I32Store8)
                    , cmd0 "i32.store" (Insn I32Store)
                    , Insn . Block <$> cmd1 "block" controlLabel
                    , Insn . Loop <$> cmd1 "loop" controlLabel
                    , Insn . If <$> cmd1 "if" controlLabel
                    , cmd0 "else" (Insn Else)
                    , cmd0 "end" (Insn End)
                    , Insn . BrIf <$> cmd1 "br_if" controlLabel
                    , Insn . Br <$> cmd1 "br" controlLabel
                    , Insn . Call <$> cmd1 "call" reference
                    , cmd0 "return" (Insn Return)
                    , cmd0 "halt" (Insn Halt)
                    , cmd0 "unreachable" (Insn Unreachable)
                    , cmd0 "nop" (Insn Nop)
                    ]

funcDecl :: Parser FunctionMeta
funcDecl = try $ do
    optionalDot
    void $ string "func"
    hspace
    choice
        [try numericFuncDecl, try keywordFuncDecl, return FunctionMeta{fmParamCount = 0, fmLocalNames = [], fmResultCount = 0}]

endFunc :: Parser ()
endFunc = try $ do
    optionalDot
    void $ string "endfunc"

numericFuncDecl :: Parser FunctionMeta
numericFuncDecl = do
    params <- number
    comma
    locals <- number
    comma
    results <- number
    return
        FunctionMeta
            { fmParamCount = params
            , fmLocalNames = map show [0 .. params + locals - 1]
            , fmResultCount = results
            }

keywordFuncDecl :: Parser FunctionMeta
keywordFuncDecl = buildFuncDecl <$> some funcWord

buildFuncDecl :: [String] -> FunctionMeta
buildFuncDecl tokens =
    let params = collectAfter "params" ["locals", "result", "results"] tokens
        locals = collectAfter "locals" ["params", "result", "results"] tokens
        results =
            case dropWhile (/= "result") tokens of
                ("result" : "i32" : _) -> 1
                ("result" : "none" : _) -> 0
                ("result" : n : _) -> fromMaybe (error $ "invalid result count: " <> toText n) (readMaybe n)
                _ -> case dropWhile (/= "results") tokens of
                    ("results" : n : _) -> fromMaybe (error $ "invalid result count: " <> toText n) (readMaybe n)
                    _ -> 0
     in FunctionMeta{fmParamCount = length params, fmLocalNames = params <> locals, fmResultCount = results}

collectAfter :: String -> [String] -> [String] -> [String]
collectAfter key stops tokens =
    case dropWhile (/= key) tokens of
        [] -> []
        (_ : rest) -> takeWhile (`notElem` stops) rest

funcWord :: Parser String
funcWord = do
    hspace
    void $ optional (char ',')
    hspace
    some $ try $ do
        c <- anySingle
        guard (c `notElem` [' ', '\t', '\n', '\r', ',', ';'])
        return c

number :: Parser Int
number = Unsafe.read <$> num

comma :: Parser ()
comma = hspace >> void (char ',') >> hspace

cmd0 :: String -> a -> Parser a
cmd0 mnemonic constructor = string mnemonic >> return constructor

cmd1 :: String -> Parser a -> Parser a
cmd1 mnemonic arg = string mnemonic >> hspace1 >> arg

optionalDot :: Parser ()
optionalDot = void (optional (char '.'))

-- | A control label or local reference, straight out of the parser: any
-- run of non-blank, non-comma, non-comment characters.
localId :: Parser Text
localId =
    toText
        <$> some
            ( try $ do
                c <- anySingle
                guard (c `notElem` [' ', '\t', '\n', '\r', ',', ';'])
                return c
            )

controlLabel :: Parser Text
controlLabel = localId

instance DerefMnemonic (Isa cl vl w) w where
    derefMnemonic f _offset i =
        case i of
            I32Const l -> I32Const (deref' f l)
            Call l -> Call (deref' f l)
            Drop -> Drop
            Select -> Select
            LocalGet n -> LocalGet n
            LocalSet n -> LocalSet n
            LocalTee n -> LocalTee n
            I32Add -> I32Add
            I32Sub -> I32Sub
            I32Mul -> I32Mul
            I32DivS -> I32DivS
            I32DivU -> I32DivU
            I32RemS -> I32RemS
            I32RemU -> I32RemU
            I32And -> I32And
            I32Or -> I32Or
            I32Xor -> I32Xor
            I32Shl -> I32Shl
            I32ShrS -> I32ShrS
            I32ShrU -> I32ShrU
            I32Eqz -> I32Eqz
            I32Eq -> I32Eq
            I32Ne -> I32Ne
            I32LtS -> I32LtS
            I32LeS -> I32LeS
            I32GtS -> I32GtS
            I32GeS -> I32GeS
            I32LtU -> I32LtU
            I32LeU -> I32LeU
            I32GtU -> I32GtU
            I32GeU -> I32GeU
            I32Load -> I32Load
            I32Store -> I32Store
            I32Load8S -> I32Load8S
            I32Load8U -> I32Load8U
            I32Store8 -> I32Store8
            Block l -> Block l
            Loop l -> Loop l
            If l -> If l
            Else -> Else
            End -> End
            Br l -> Br l
            BrIf l -> BrIf l
            Return -> Return
            Halt -> Halt
            Unreachable -> Unreachable
            Nop -> Nop
            FuncEnter p l r -> FuncEnter p l r

instance ByteSize (Isa cl vl w l) where
    byteSize I32Const{} = 5
    byteSize Call{} = 5
    byteSize LocalGet{} = 2
    byteSize LocalSet{} = 2
    byteSize LocalTee{} = 2
    byteSize Block{} = 2
    byteSize Loop{} = 2
    byteSize If{} = 2
    byteSize Br{} = 2
    byteSize BrIf{} = 2
    byteSize FuncEnter{} = 4
    byteSize _ = 1

-- | Resolve `block`/`loop`/`if`/`br`/`br_if` labels from names to small,
-- per-function integer ids, scanning an already-assembled @Isa Text vl w
-- l@ code section. The id counter resets at each 'FuncEnter', keeping ids
-- small per function instead of growing across the whole program. Generic
-- in @vl@ (local references): unresolved or resolved, doesn't matter
-- here, so this composes with 'resolveWasmLocals' in either order.
resolveWasmLabels ::
    forall vl w l.
    Section (Isa Text vl w l) w Text
    -> Either Text (Section (Isa Int vl w l) w Text)
resolveWasmLabels Data{org, dataTokens} = Right Data{org, dataTokens}
resolveWasmLabels Code{org, codeTokens} = do
    tokens' <- go 0 [] codeTokens
    Right Code{org, codeTokens = tokens'}
    where
        go ::
            Int
            -> [(Text, Int)]
            -> [CodeToken (Isa Text vl w l) Text]
            -> Either Text [CodeToken (Isa Int vl w l) Text]
        go _ [] [] = Right []
        go _ ((name, _) : _) [] = Left $ "unclosed structured control label: " <> name
        go nextId open (Label l : toks) = (Label l :) <$> go nextId open toks
        go nextId open (Mnemonic instr : toks) = case instr of
            -- The three constructs that actually scope a label: mint a
            -- fresh id and push (name, id) so a later Br/BrIf can find it.
            Block name -> (Mnemonic (Block nextId) :) <$> go (nextId + 1) ((name, nextId) : open) toks
            Loop name -> (Mnemonic (Loop nextId) :) <$> go (nextId + 1) ((name, nextId) : open) toks
            If name -> (Mnemonic (If nextId) :) <$> go (nextId + 1) ((name, nextId) : open) toks
            -- Closes whichever scope is innermost.
            End -> case open of
                [] -> Left "unexpected end"
                (_ : rest) -> (Mnemonic End :) <$> go nextId rest toks
            -- Resolve by name against whatever's currently open (innermost
            -- match wins, same as any lexical scope).
            Br name -> resolveBranch Br name
            BrIf name -> resolveBranch BrIf name
            -- One function's worth of ids ends here; the next one starts
            -- fresh from 0.
            FuncEnter p d r -> (Mnemonic (FuncEnter p d r) :) <$> go 0 open toks
            -- Everything else carries no label -- same value, different
            -- (phantom, at this point) label type.
            I32Const v -> (Mnemonic (I32Const v) :) <$> go nextId open toks
            Call v -> (Mnemonic (Call v) :) <$> go nextId open toks
            Drop -> (Mnemonic Drop :) <$> go nextId open toks
            Select -> (Mnemonic Select :) <$> go nextId open toks
            LocalGet i -> (Mnemonic (LocalGet i) :) <$> go nextId open toks
            LocalSet i -> (Mnemonic (LocalSet i) :) <$> go nextId open toks
            LocalTee i -> (Mnemonic (LocalTee i) :) <$> go nextId open toks
            I32Add -> (Mnemonic I32Add :) <$> go nextId open toks
            I32Sub -> (Mnemonic I32Sub :) <$> go nextId open toks
            I32Mul -> (Mnemonic I32Mul :) <$> go nextId open toks
            I32DivS -> (Mnemonic I32DivS :) <$> go nextId open toks
            I32DivU -> (Mnemonic I32DivU :) <$> go nextId open toks
            I32RemS -> (Mnemonic I32RemS :) <$> go nextId open toks
            I32RemU -> (Mnemonic I32RemU :) <$> go nextId open toks
            I32And -> (Mnemonic I32And :) <$> go nextId open toks
            I32Or -> (Mnemonic I32Or :) <$> go nextId open toks
            I32Xor -> (Mnemonic I32Xor :) <$> go nextId open toks
            I32Shl -> (Mnemonic I32Shl :) <$> go nextId open toks
            I32ShrS -> (Mnemonic I32ShrS :) <$> go nextId open toks
            I32ShrU -> (Mnemonic I32ShrU :) <$> go nextId open toks
            I32Eqz -> (Mnemonic I32Eqz :) <$> go nextId open toks
            I32Eq -> (Mnemonic I32Eq :) <$> go nextId open toks
            I32Ne -> (Mnemonic I32Ne :) <$> go nextId open toks
            I32LtS -> (Mnemonic I32LtS :) <$> go nextId open toks
            I32LeS -> (Mnemonic I32LeS :) <$> go nextId open toks
            I32GtS -> (Mnemonic I32GtS :) <$> go nextId open toks
            I32GeS -> (Mnemonic I32GeS :) <$> go nextId open toks
            I32LtU -> (Mnemonic I32LtU :) <$> go nextId open toks
            I32LeU -> (Mnemonic I32LeU :) <$> go nextId open toks
            I32GtU -> (Mnemonic I32GtU :) <$> go nextId open toks
            I32GeU -> (Mnemonic I32GeU :) <$> go nextId open toks
            I32Load -> (Mnemonic I32Load :) <$> go nextId open toks
            I32Store -> (Mnemonic I32Store :) <$> go nextId open toks
            I32Load8S -> (Mnemonic I32Load8S :) <$> go nextId open toks
            I32Load8U -> (Mnemonic I32Load8U :) <$> go nextId open toks
            I32Store8 -> (Mnemonic I32Store8 :) <$> go nextId open toks
            Else -> (Mnemonic Else :) <$> go nextId open toks
            Return -> (Mnemonic Return :) <$> go nextId open toks
            Halt -> (Mnemonic Halt :) <$> go nextId open toks
            Unreachable -> (Mnemonic Unreachable :) <$> go nextId open toks
            Nop -> (Mnemonic Nop :) <$> go nextId open toks
            where
                resolveBranch ctor name = case lookupAssoc name open of
                    Just i -> (Mnemonic (ctor i) :) <$> go nextId open toks
                    Nothing -> Left $ "unknown control label: " <> name

-- | Resolve `local.get`/`local.set`/`local.tee` names to per-function
-- indices (params, then declared locals, in declaration order). Unlike
-- 'resolveWasmLabels', a function's local names aren't recoverable from
-- the token stream itself (`FuncEnter` only carries counts -- see its
-- haddock), so they're supplied here instead: @locals@ has one entry per
-- function -- its full local list (params ++ declared locals, in that
-- order) -- consumed in stream order as each 'FuncEnter' is encountered
-- (this is exactly what 'resolveLocalsInSections' feeds it, from the
-- metadata 'stripFuncDecls' peeled off each @.func@). Generic in @cl@
-- (structured-control labels): unresolved or resolved, doesn't matter
-- here, so this composes with
-- 'resolveWasmLabels' in either order.
resolveWasmLocals ::
    forall cl w l.
    [[Text]]
    -> Section (Isa cl Text w l) w Text
    -> Either Text (Section (Isa cl Int w l) w Text)
resolveWasmLocals _ Data{org, dataTokens} = Right Data{org, dataTokens}
resolveWasmLocals functionLocals Code{org, codeTokens} = do
    tokens' <- go [] functionLocals codeTokens
    Right Code{org, codeTokens = tokens'}
    where
        go ::
            [(Text, Int)]
            -> [[Text]]
            -> [CodeToken (Isa cl Text w l) Text]
            -> Either Text [CodeToken (Isa cl Int w l) Text]
        go _ remaining []
            | null remaining = Right []
            | otherwise = Left "resolveWasmLocals: more local lists supplied than functions in the code section"
        go current remaining (Label l : toks) = (Label l :) <$> go current remaining toks
        go current remaining (Mnemonic instr : toks) = case instr of
            -- One function's worth of locals ends here; the next
            -- FuncEnter starts a fresh table from the next supplied list.
            FuncEnter p d r -> case remaining of
                [] -> Left "resolveWasmLocals: FuncEnter without a matching local list"
                (names : rest) -> (Mnemonic (FuncEnter p d r) :) <$> go (zip names [0 ..]) rest toks
            LocalGet name -> resolveLocal LocalGet name
            LocalSet name -> resolveLocal LocalSet name
            LocalTee name -> resolveLocal LocalTee name
            -- Everything else carries no local reference -- same value,
            -- different (phantom, at this point) local type.
            Block cl -> (Mnemonic (Block cl) :) <$> go current remaining toks
            Loop cl -> (Mnemonic (Loop cl) :) <$> go current remaining toks
            If cl -> (Mnemonic (If cl) :) <$> go current remaining toks
            Br cl -> (Mnemonic (Br cl) :) <$> go current remaining toks
            BrIf cl -> (Mnemonic (BrIf cl) :) <$> go current remaining toks
            I32Const v -> (Mnemonic (I32Const v) :) <$> go current remaining toks
            Call v -> (Mnemonic (Call v) :) <$> go current remaining toks
            Drop -> (Mnemonic Drop :) <$> go current remaining toks
            Select -> (Mnemonic Select :) <$> go current remaining toks
            I32Add -> (Mnemonic I32Add :) <$> go current remaining toks
            I32Sub -> (Mnemonic I32Sub :) <$> go current remaining toks
            I32Mul -> (Mnemonic I32Mul :) <$> go current remaining toks
            I32DivS -> (Mnemonic I32DivS :) <$> go current remaining toks
            I32DivU -> (Mnemonic I32DivU :) <$> go current remaining toks
            I32RemS -> (Mnemonic I32RemS :) <$> go current remaining toks
            I32RemU -> (Mnemonic I32RemU :) <$> go current remaining toks
            I32And -> (Mnemonic I32And :) <$> go current remaining toks
            I32Or -> (Mnemonic I32Or :) <$> go current remaining toks
            I32Xor -> (Mnemonic I32Xor :) <$> go current remaining toks
            I32Shl -> (Mnemonic I32Shl :) <$> go current remaining toks
            I32ShrS -> (Mnemonic I32ShrS :) <$> go current remaining toks
            I32ShrU -> (Mnemonic I32ShrU :) <$> go current remaining toks
            I32Eqz -> (Mnemonic I32Eqz :) <$> go current remaining toks
            I32Eq -> (Mnemonic I32Eq :) <$> go current remaining toks
            I32Ne -> (Mnemonic I32Ne :) <$> go current remaining toks
            I32LtS -> (Mnemonic I32LtS :) <$> go current remaining toks
            I32LeS -> (Mnemonic I32LeS :) <$> go current remaining toks
            I32GtS -> (Mnemonic I32GtS :) <$> go current remaining toks
            I32GeS -> (Mnemonic I32GeS :) <$> go current remaining toks
            I32LtU -> (Mnemonic I32LtU :) <$> go current remaining toks
            I32LeU -> (Mnemonic I32LeU :) <$> go current remaining toks
            I32GtU -> (Mnemonic I32GtU :) <$> go current remaining toks
            I32GeU -> (Mnemonic I32GeU :) <$> go current remaining toks
            I32Load -> (Mnemonic I32Load :) <$> go current remaining toks
            I32Store -> (Mnemonic I32Store :) <$> go current remaining toks
            I32Load8S -> (Mnemonic I32Load8S :) <$> go current remaining toks
            I32Load8U -> (Mnemonic I32Load8U :) <$> go current remaining toks
            I32Store8 -> (Mnemonic I32Store8 :) <$> go current remaining toks
            Else -> (Mnemonic Else :) <$> go current remaining toks
            End -> (Mnemonic End :) <$> go current remaining toks
            Return -> (Mnemonic Return :) <$> go current remaining toks
            Halt -> (Mnemonic Halt :) <$> go current remaining toks
            Unreachable -> (Mnemonic Unreachable :) <$> go current remaining toks
            Nop -> (Mnemonic Nop :) <$> go current remaining toks
            where
                resolveLocal ctor name = case lookupAssoc name current of
                    Just i -> (Mnemonic (ctor i) :) <$> go current remaining toks
                    Nothing -> Left $ "unknown local: " <> name

translateWasm32 ::
    forall w.
    (MachineWord w) =>
    Int
    -> [Word8]
    -> FilePath
    -> String
    -> Either Text (TranslatorResult (Mem (Isa Int Int w w) w) w, FunctionTable)
translateWasm32 memorySize fillBytes fn src =
    case parse asmParser fn src of
        Left err -> Left $ toText $ errorBundlePretty err
        Right tokenSections -> do
            let stripped = map stripFuncDecls tokenSections
                sections = map fst stripped
                metas = concatMap snd stripped
            labels <- evaluateLabels sections
            let resolveLabel l = HashMap.lookup l labels
                marked = markupSectionOffsets 0 sections
            functionTable <- buildFunctionTable marked metas
            labeled <- traverse (traverse resolveWasmLabels) marked
            localed <- resolveLocalsInSections metas labeled
            let code = map (uncurry (derefSection resolveLabel)) localed
            validateCallTargets functionTable code
            let stats = computeDumpStats code
            dump <- prepareDump memorySize fillBytes code
            Right (TranslatorResult dump labels stats, functionTable)

-- | Replace every parsed @.func@ declaration with the 'FuncEnter'
-- instruction it describes, peeling its metadata off into a separate,
-- file-order list -- 'buildFunctionTable' and 'resolveLocalsInSections'
-- each match it back up against the 'FuncEnter's it produced, in the same
-- order (see 'WasmToken's haddock for why this split exists at all).
stripFuncDecls ::
    Section (WasmToken w l) w Text
    -> (Section (Isa Text Text w l) w Text, [FunctionMeta])
stripFuncDecls Data{org, dataTokens} = (Data{org, dataTokens}, [])
stripFuncDecls Code{org, codeTokens} =
    let (tokens', metas) = foldr step ([], []) codeTokens
     in (Code{org, codeTokens = tokens'}, metas)
    where
        step (Label l) (toks, metas) = (Label l : toks, metas)
        step (Mnemonic (Insn i)) (toks, metas) = (Mnemonic i : toks, metas)
        step (Mnemonic (FuncDecl meta@FunctionMeta{fmParamCount, fmLocalNames, fmResultCount})) (toks, metas) =
            (Mnemonic (FuncEnter fmParamCount (length fmLocalNames - fmParamCount) fmResultCount) : toks, meta : metas)

-- | Match each parsed function declaration (in file order) up with the
-- 'FuncEnter' it produced, now that offsets are known: the address-keyed
-- table 'validateCallTargets' checks 'Call' targets against, and report
-- views look the current function up in (see 'currentFunctionMeta').
buildFunctionTable ::
    (MachineWord w) =>
    [(w, Section (Isa cl vl w l) w Text)]
    -> [FunctionMeta]
    -> Either Text FunctionTable
buildFunctionTable sections metas = snd <$> foldM collectSection (metas, IntMap.empty) sections
    where
        collectSection (ms, table) (_, Data{}) = Right (ms, table)
        collectSection (ms, table) (offset, Code{codeTokens}) =
            foldM collectToken (ms, table, offset) codeTokens <&> \(ms', table', _) -> (ms', table')

        collectToken (ms, table, offset) (Label _) = Right (ms, table, offset)
        collectToken (ms, table, offset) (Mnemonic instr) =
            let next = offset + toEnum (byteSize instr)
             in case instr of
                    FuncEnter{} -> case ms of
                        [] -> Left "internal error: FuncEnter without a matching .func declaration"
                        (meta : rest) -> do
                            validateFunctionMeta meta
                            let addr = fromEnum offset
                            when (IntMap.member addr table) $ Left $ "duplicate function metadata at address " <> show addr
                            Right (rest, IntMap.insert addr meta table, next)
                    _ -> Right (ms, table, next)

validateFunctionMeta :: FunctionMeta -> Either Text ()
validateFunctionMeta FunctionMeta{fmLocalNames} = case firstDuplicate fmLocalNames of
    Just name -> Left $ "duplicate local name: " <> toText name
    Nothing -> Right ()

firstDuplicate :: (Eq a) => [a] -> Maybe a
firstDuplicate [] = Nothing
firstDuplicate (x : xs)
    | x `elem` xs = Just x
    | otherwise = firstDuplicate xs

-- | Thread 'resolveWasmLocals' (which resolves one 'Section' at a time)
-- across every section in the program, splitting @metas@ so each section
-- gets exactly the name lists for the 'FuncEnter's it contains.
resolveLocalsInSections ::
    [FunctionMeta]
    -> [(w, Section (Isa Int Text w l) w Text)]
    -> Either Text [(w, Section (Isa Int Int w l) w Text)]
resolveLocalsInSections = go
    where
        go _ [] = Right []
        go ms ((offset, s) : rest) = do
            let (mine, remaining) = splitAt (funcEnterCount s) ms
            s' <- resolveWasmLocals (map (map toText . fmLocalNames) mine) s
            ((offset, s') :) <$> go remaining rest

funcEnterCount :: Section (Isa cl vl w l) w Text -> Int
funcEnterCount Data{} = 0
funcEnterCount Code{codeTokens} = length [() | Mnemonic FuncEnter{} <- codeTokens]

-- | A 'Call's target must point at a declared function -- the resolved
-- stream from 'derefSection' is where it first becomes a concrete address
-- to check.
validateCallTargets :: (MachineWord w) => FunctionTable -> [Section (Isa Int Int w w) w w] -> Either Text ()
validateCallTargets functionTable = traverse_ (traverse_ checkToken . codeTokensOf)
    where
        codeTokensOf Data{} = []
        codeTokensOf Code{codeTokens} = codeTokens

        checkToken (Mnemonic (Call target))
            | IntMap.member (fromEnum target) functionTable = Right ()
            | otherwise = Left $ "call target does not point to .func: " <> show target
        checkToken _ = Right ()

type Wasm32State w = MachineState (IoMem (Isa Int Int w w) w) w

-- | What a control record on the chain represents. Encoded as a plain word
-- in memory -- see 'recordKindOffset'.
data RecordKind = RecordCall | RecordBlock | RecordLoop | RecordIf
    deriving (Bounded, Enum, Eq, Show)

-- | The kind-specific payload passed to 'enter'. docs/wasm32.md describes
-- the persisted record as a C-style tagged union (untyped fields, valid
-- shape implied by `kind`, a convention the reader has to trust); here the
-- shape is the type itself, one constructor per 'RecordKind', so a
-- mismatch between a record's kind and its payload can't be constructed in
-- the first place -- 'recordKindOf' derives the kind from the value rather
-- than needing it passed alongside.
data RecordExtra
    = -- | Block: branch target id only.
      BlockExtra {reLabel :: Int}
    | -- | If: branch target id only.
      IfExtra {reLabel :: Int}
    | -- | Loop: branch target id, and the loop's re-entry point.
      LoopExtra {reLabel :: Int, reStartPc :: Int}
    | -- | Call: caller's frame_base to restore on return, and this call's
      -- own 'FuncEnter' address (report views only).
      CallExtra {reSavedFrameBase :: Int, reEntryPc :: Int}
    deriving (Show)

recordKindOf :: RecordExtra -> RecordKind
recordKindOf BlockExtra{} = RecordBlock
recordKindOf IfExtra{} = RecordIf
recordKindOf LoopExtra{} = RecordLoop
recordKindOf CallExtra{} = RecordCall

-- | The two generic word slots a 'RecordExtra' actually occupies in
-- memory (see 'recordField4Offset'/'recordField5Offset') -- the only place
-- that needs to know both the tagged Haskell shape and the untyped
-- on-disk layout at once.
extraRawFields :: RecordExtra -> (Int, Int)
extraRawFields (BlockExtra l) = (l, 0)
extraRawFields (IfExtra l) = (l, 0)
extraRawFields (LoopExtra l s) = (l, s)
extraRawFields (CallExtra fb ep) = (fb, ep)

-- | Width, in words, of one control record. See docs/wasm32.md's "Control
-- Records" section for the field layout this mirrors.
recordWidth :: Int
recordWidth = 6

recordLinkOffset, recordKindOffset, recordEndPcOffset, recordResultCountOffset :: Int
recordLinkOffset = 0
recordKindOffset = 1
recordEndPcOffset = 2
recordResultCountOffset = 3

-- | 'RecordExtra's first field: label (Block\/If\/Loop) or savedFrameBase
-- (Call).
recordField4Offset :: Int
recordField4Offset = 4

-- | 'RecordExtra's second field: startPc (Loop only) or entryPc (Call).
recordField5Offset :: Int
recordField5Offset = 5

-- | Sentinel for "no previous record" ('ctrlTop'\/a record's `link`), and
-- for a `Call` record's `endPc` meaning "this is `_start` -- halt, don't
-- jump to a return address."
nullAddr :: Int
nullAddr = -1

data MachineState mem w = State
    { pc :: Int
    -- ^ Program counter.
    , sp :: Int
    -- ^ Top of the one runtime stack (locals, operand values, and control
    -- records all live here -- see docs/wasm32.md's Execution Model).
    , frameBase :: Int
    -- ^ Base address of the active function's locals.
    , ctrlTop :: Int
    -- ^ Address of the innermost open block\/loop\/if\/call record, or
    -- 'nullAddr'.
    , mem :: mem
    , spMax :: !Int
    -- ^ High-water mark of 'sp' (locals + operand values + control
    -- records combined -- backs @wasm32:operand-stack-max@).
    , callDepth :: !Int
    , callDepthMax :: !Int
    -- ^ Live/high-water count of open `Call` records (backs @frames@\/
    -- @wasm32:frames-max@).
    , ctrlDepth :: !Int
    , ctrlDepthMax :: !Int
    -- ^ Live/high-water count of open `Block`\/`Loop`\/`If` records (backs
    -- @wasm32:control-stack-max@).
    , functions :: !FunctionTable
    -- ^ Kept only for report\/debug views (@locals@, @local:<name>@,
    -- @stack@) -- not consulted by instruction execution, which reads
    -- function metadata from the embedded 'FuncEnter' instead.
    , stopped :: Bool
    , internalError :: Maybe Text
    }
    deriving (Show)

instance InitState (IoMem (Isa Int Int w w) w) (MachineState (IoMem (Isa Int Int w w) w) w) where
    initState pc dump _randomStream =
        State
            { pc
            , sp = memTop dump
            , frameBase = memTop dump
            , ctrlTop = nullAddr
            , mem = dump
            , spMax = memTop dump
            , callDepth = 0
            , callDepthMax = 0
            , ctrlDepth = 0
            , ctrlDepthMax = 0
            , functions = IntMap.empty
            , stopped = False
            , internalError = Nothing
            }

-- | Where the runtime stack starts: the upper half of the configured
-- memory, code+data occupying the lower half.
--
-- TODO: make the stack's size/placement independently configurable
-- instead of this fixed split of the ISA's memory-size setting.
memTop :: IoMem (Isa Int Int w w) w -> Int
memTop IoMem{mIoCells = Mem{memorySize}} = memorySize `div` 2

-- | Width, in bytes, of the 'FuncEnter' instruction every function
-- starts with. Fixed regardless of the header's actual field values.
funcEnterSize :: Int
funcEnterSize = byteSize (FuncEnter 0 0 0 :: Isa Int Int w w)

initWasm32State ::
    forall w.
    (MachineWord w) =>
    Int
    -> IoMem (Isa Int Int w w) w
    -> FunctionTable
    -> Either Text (Wasm32State w)
initWasm32State entryPc dump functionTable =
    case IntMap.lookup entryPc functionTable of
        Nothing -> Left "_start label should point to .func."
        Just meta
            | fmParamCount meta /= 0 -> Left "entry function cannot have parameters"
            | otherwise -> do
                let step = byteSizeT @w
                    base = memTop dump
                    localCount = length (fmLocalNames meta)
                    recordAddr = base + localCount * step
                    stackTop = recordAddr + recordWidth * step
                mem' <- writeRecordAt dump recordAddr nullAddr (CallExtra nullAddr entryPc) nullAddr 0
                Right
                    State
                        { pc = entryPc + funcEnterSize
                        , sp = stackTop
                        , frameBase = base
                        , ctrlTop = recordAddr
                        , mem = mem'
                        , spMax = stackTop
                        , callDepth = 1
                        , callDepthMax = 1
                        , ctrlDepth = 0
                        , ctrlDepthMax = 0
                        , functions = functionTable
                        , stopped = False
                        , internalError = Nothing
                        }

-- | Write a control record's six fields at @addr@ (word-index offsets,
-- scaled to bytes internally -- this memory is byte-addressed and a `w`
-- occupies 'byteSizeT' bytes, not one address). Pure -- used both by
-- 'initWasm32State' (before any 'State' machinery exists) and, wrapped by
-- 'writeRecord', by 'enter' during execution.
writeRecordAt ::
    forall w.
    (MachineWord w) =>
    IoMem (Isa Int Int w w) w
    -> Int
    -> Int
    -- ^ link
    -> RecordExtra
    -> Int
    -- ^ endPc
    -> Int
    -- ^ resultCount
    -> Either Text (IoMem (Isa Int Int w w) w)
writeRecordAt m addr link extra endPc resultCount =
    let step = byteSizeT @w
        (field4, field5) = extraRawFields extra
     in foldlM
            (\m' (o, v) -> writeWord m' (addr + o * step) (toEnum v))
            m
            [ (recordLinkOffset, link)
            , (recordKindOffset, fromEnum (recordKindOf extra))
            , (recordEndPcOffset, endPc)
            , (recordResultCountOffset, resultCount)
            , (recordField4Offset, field4)
            , (recordField5Offset, field5)
            ]

setPc :: Int -> State (MachineState (IoMem (Isa Int Int w w) w) w) ()
setPc addr = modify $ \st -> st{pc = addr}

nextPc :: Isa Int Int w w -> State (MachineState (IoMem (Isa Int Int w w) w) w) ()
nextPc instruction = do
    State{pc} <- get
    setPc (pc + byteSize instruction)

raiseInternalError :: Text -> State (MachineState (IoMem (Isa Int Int w w) w) w) ()
raiseInternalError msg = modify $ \st -> st{internalError = Just msg}

-- | Set `sp`, tracking its high-water mark ('spMax').
setSp :: Int -> State (MachineState (IoMem (Isa Int Int w w) w) w) ()
setSp sp' = modify $ \st -> st{sp = sp', spMax = max (spMax st) sp'}

-- | This memory is byte-addressed and a `w` occupies 'byteSizeT' bytes,
-- not one address, so `sp` must step by that width, not by 1.
pushValue :: forall w. (MachineWord w) => w -> State (MachineState (IoMem (Isa Int Int w w) w) w) ()
pushValue value = do
    State{sp} <- get
    setWord sp value
    setSp (sp + byteSizeT @w)

-- | No underflow guard: popping past the current frame's locals reads
-- whatever is physically below them (another frame's data, or a memory
-- error at the very bottom) -- the same "no floor check" gap real Wasm
-- closes with an ahead-of-time validator, not a runtime check.
popValue :: forall w. (MachineWord w) => State (MachineState (IoMem (Isa Int Int w w) w) w) w
popValue = do
    State{sp} <- get
    let sp' = sp - byteSizeT @w
    setSp sp'
    getWord sp'

popValues :: (MachineWord w) => Int -> State (MachineState (IoMem (Isa Int Int w w) w) w) [w]
popValues n = reverse <$> replicateM n popValue

getWord :: (MachineWord w) => Int -> State (MachineState (IoMem (Isa Int Int w w) w) w) w
getWord addr = do
    st@State{mem} <- get
    case readWord mem addr of
        Right (mem', w) -> do
            put st{mem = mem'}
            return w
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return def

setWord :: (MachineWord w) => Int -> w -> State (MachineState (IoMem (Isa Int Int w w) w) w) ()
setWord addr w = do
    st@State{mem} <- get
    case writeWord mem addr w of
        Right mem' -> put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err

getByte :: (MachineWord w) => Int -> State (MachineState (IoMem (Isa Int Int w w) w) w) Word8
getByte addr = do
    st@State{mem} <- get
    case readByte mem addr of
        Right (mem', b) -> do
            put st{mem = mem'}
            return b
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return 0

setByte :: (MachineWord w) => Int -> Word8 -> State (MachineState (IoMem (Isa Int Int w w) w) w) ()
setByte addr byte = do
    st@State{mem} <- get
    case writeByte mem addr byte of
        Right mem' -> put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err

lookupLocal :: forall w. (MachineWord w) => Int -> State (MachineState (IoMem (Isa Int Int w w) w) w) w
lookupLocal index = do
    State{frameBase} <- get
    getWord (frameBase + index * byteSizeT @w)

setLocal :: forall w. (MachineWord w) => Int -> w -> State (MachineState (IoMem (Isa Int Int w w) w) w) ()
setLocal index value = do
    State{frameBase} <- get
    setWord (frameBase + index * byteSizeT @w) value

readRecordField :: forall w. (MachineWord w) => Int -> Int -> State (MachineState (IoMem (Isa Int Int w w) w) w) Int
readRecordField addr offset = fromEnum <$> getWord (addr + offset * byteSizeT @w)

readRecordKind :: (MachineWord w) => Int -> State (MachineState (IoMem (Isa Int Int w w) w) w) RecordKind
readRecordKind addr = toEnum <$> readRecordField addr recordKindOffset

writeRecord ::
    (MachineWord w) =>
    Int -> Int -> RecordExtra -> Int -> Int -> State (MachineState (IoMem (Isa Int Int w w) w) w) ()
writeRecord addr link extra endPc resultCount = do
    st@State{mem} <- get
    case writeRecordAt mem addr link extra endPc resultCount of
        Right mem' -> put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err

-- | Push a new control record and make it the current one (`ctrlTop`).
-- Used by `block`/`loop`/`if` (once a branch is actually taken) and
-- `call`. See docs/wasm32.md's "Control Records" section.
enter ::
    forall w. (MachineWord w) => Int -> Int -> RecordExtra -> State (MachineState (IoMem (Isa Int Int w w) w) w) ()
enter endPc resultCount extra = do
    State{sp, ctrlTop} <- get
    writeRecord sp ctrlTop extra endPc resultCount
    setSp (sp + recordWidth * byteSizeT @w)
    modify $ \st -> st{ctrlTop = sp}
    bumpDepth (recordKindOf extra)
    where
        bumpDepth RecordCall = modify $ \st -> st{callDepth = callDepth st + 1, callDepthMax = max (callDepthMax st) (callDepth st + 1)}
        bumpDepth _ = modify $ \st -> st{ctrlDepth = ctrlDepth st + 1, ctrlDepthMax = max (ctrlDepthMax st) (ctrlDepth st + 1)}

-- | The one operation behind `end`, a taken `br`/`br_if`, and `return`:
-- collapse the record at @r@, optionally keeping it open (a taken branch
-- to a `loop`). See docs/wasm32.md's "Control Records" section.
collapse :: (MachineWord w) => Int -> Bool -> State (MachineState (IoMem (Isa Int Int w w) w) w) ()
collapse r keepOpen = do
    kind <- readRecordKind r
    case kind of
        RecordCall -> do
            -- Read every field before touching the stack below: closing
            -- overwrites the record's own bytes (splicing or relocating
            -- results into/through them), so anything still needed from
            -- it must be read first.
            resultCount <- readRecordField r recordResultCountOffset
            savedFrameBase <- readRecordField r recordField4Offset
            link <- readRecordField r recordLinkOffset
            endPc <- readRecordField r recordEndPcOffset
            -- A function declares a real result count, so closing it
            -- collects exactly that many values and reclaims everything
            -- below them: the record itself, and (since `r` sits above
            -- the callee's locals) the locals too.
            results <- popValues resultCount
            State{frameBase} <- get
            setSp frameBase
            mapM_ pushValue results
            modify $ \st -> st{frameBase = savedFrameBase, ctrlTop = link, callDepth = callDepth st - 1}
            if endPc == nullAddr
                then modify $ \st -> st{stopped = True}
                else setPc endPc
        _
            -- Branching back into a `loop` never removes anything: a
            -- well-formed body already leaves the stack exactly as it
            -- found it (Wasm32 gives block/loop/if no declared result
            -- type to collect otherwise), so there's nothing to splice.
            | keepOpen -> readRecordField r recordField5Offset >>= setPc
            -- Closing a `block`/`loop`/`if`: unlike `Call`, there are no
            -- locals of its own to reclaim -- only the record's own words
            -- need to go, so splice them out and keep everything the body
            -- pushed above them (see 'spliceOut'). Read link/endPc first
            -- -- the splice overwrites this record's own bytes.
            | otherwise -> do
                link <- readRecordField r recordLinkOffset
                -- Unlike a Call record's endPc (already a full return
                -- address), Block/Loop/If store the matching `end`
                -- instruction's own address (from findEndPc/
                -- resolveIfTargets) -- same convention `executeElse`
                -- uses, so skip past it the same way here.
                endPc <- readRecordField r recordEndPcOffset
                State{sp} <- get
                spliceOut r recordWidth sp
                modify $ \st -> st{ctrlTop = link, ctrlDepth = ctrlDepth st - 1}
                setPc (endPc + byteSize End)

-- | Remove the @widthWords@-word record at @r@, shifting @[r+width, top)@
-- (moved one whole `w` at a time) down to start at @r@, and shrinking `sp`
-- to match.
spliceOut :: forall w. (MachineWord w) => Int -> Int -> Int -> State (MachineState (IoMem (Isa Int Int w w) w) w) ()
spliceOut r widthWords top = do
    let step = byteSizeT @w
        width = widthWords * step
    forM_ [0, step .. top - r - width - 1] $ \i -> getWord (r + width + i) >>= setWord (r + i)
    setSp (top - width)

-- | Walk the control chain from `ctrlTop` for the record tagged @label@,
-- erroring rather than walking past a `Call` record (a valid program's
-- `br`/`br_if` always resolves within its own function).
findLabel :: (MachineWord w) => Int -> State (MachineState (IoMem (Isa Int Int w w) w) w) (Either Text Int)
findLabel label = get >>= go . ctrlTop
    where
        go r
            | r == nullAddr = return $ Left $ "unknown control label: " <> show label
            | otherwise = do
                kind <- readRecordKind r
                if kind == RecordCall
                    then return $ Left $ "unknown control label: " <> show label
                    else do
                        l <- readRecordField r recordField4Offset
                        if l == label
                            then return $ Right r
                            else readRecordField r recordLinkOffset >>= go

-- | Walk the control chain from `ctrlTop` for the nearest enclosing `Call`
-- record -- what `return` closes.
findCall :: (MachineWord w) => State (MachineState (IoMem (Isa Int Int w w) w) w) (Either Text Int)
findCall = get >>= go . ctrlTop
    where
        go r
            | r == nullAddr = return $ Left "return without active function frame"
            | otherwise = do
                kind <- readRecordKind r
                if kind == RecordCall
                    then return $ Right r
                    else readRecordField r recordLinkOffset >>= go

-- | Collapse every open record from `ctrlTop` down to and including @r@,
-- keeping @r@ itself open only if @keepOpen@ (a taken branch back into a
-- `loop`). Anything strictly above @r@ is always fully closed along the
-- way -- branching or returning past a scope exits it unconditionally,
-- regardless of what its own body would otherwise have preserved.
unwindTo :: (MachineWord w) => Int -> Bool -> State (MachineState (IoMem (Isa Int Int w w) w) w) ()
unwindTo r keepOpen = do
    State{ctrlTop} <- get
    if ctrlTop == r
        then collapse r keepOpen
        else do
            collapse ctrlTop False
            unwindTo r keepOpen

-- | `br`/`br_if <label>`: find the targeted record and unwind to it,
-- keeping it open only when it's a `loop` (branching back to its start).
branch :: (MachineWord w) => Int -> State (MachineState (IoMem (Isa Int Int w w) w) w) ()
branch label = do
    result <- findLabel label
    case result of
        Right r -> do
            kind <- readRecordKind r
            unwindTo r (kind == RecordLoop)
        Left err -> raiseInternalError err

findEndPc :: (MachineWord w) => IoMem (Isa Int Int w w) w -> Int -> Either Text Int
findEndPc memory start = go start (0 :: Int)
    where
        go addr depth = do
            (_, instruction) <- readInstruction memory addr
            let next = addr + byteSize instruction
            case instruction of
                Block{} -> go next (depth + 1)
                Loop{} -> go next (depth + 1)
                If{} -> go next (depth + 1)
                End
                    | depth == 0 -> Right addr
                    | otherwise -> go next (depth - 1)
                _ -> go next depth

findIfTargets :: (MachineWord w) => IoMem (Isa Int Int w w) w -> Int -> Either Text (Maybe Int, Int)
findIfTargets memory start = go start (0 :: Int) Nothing
    where
        go addr depth elsePc = do
            (_, instruction) <- readInstruction memory addr
            let next = addr + byteSize instruction
            case instruction of
                Block{} -> go next (depth + 1) elsePc
                Loop{} -> go next (depth + 1) elsePc
                If{} -> go next (depth + 1) elsePc
                Else
                    | depth == 0 -> go next depth (Just addr)
                    | otherwise -> go next depth elsePc
                End
                    | depth == 0 -> Right (elsePc, addr)
                    | otherwise -> go next (depth - 1) elsePc
                _ -> go next depth elsePc

-- | Resolve the else/end targets for the `if` starting right after the
-- given pc. Currently rescans the bytecode on every execution.
--
-- TODO: back this with a small LRU cache keyed by the `if`'s pc (default
-- size 4), storing the resolved (elsePc, endPc) pair, since the bytecode
-- is immutable and targets never change once resolved. This is the
-- branch-target-buffer-style optimization discussed for hot loops.
resolveIfTargets :: (MachineWord w) => IoMem (Isa Int Int w w) w -> Int -> Either Text (Maybe Int, Int)
resolveIfTargets = findIfTargets

-- | `else` is reached only by falling through a taken then-branch, so the
-- record it needs (the currently-open `If`) is always `ctrlTop` -- it
-- reads that record's `endPc` and jumps past it, without closing it (the
-- real `end` still has to run to close it).
executeElse :: (MachineWord w) => State (MachineState (IoMem (Isa Int Int w w) w) w) ()
executeElse = do
    State{ctrlTop} <- get
    endPc <- readRecordField ctrlTop recordEndPcOffset
    setPc (endPc + byteSize End)

-- | Pure, used only by the report/debug views below (never by instruction
-- execution): the address of the nearest enclosing `Call` record, found
-- by walking from @r@ via `link`. Shared by 'currentEntryPc' (innermost
-- only) and 'frameChain' (walking outward through the whole call chain).
findCallRecord :: (MachineWord w) => IoMem (Isa Int Int w w) w -> Int -> Maybe Int
findCallRecord m = go
    where
        go r
            | r == nullAddr = Nothing
            | otherwise = case pureRecordKind m r of
                Just RecordCall -> Just r
                Just _ -> pureRecordField m r recordLinkOffset >>= go
                Nothing -> Nothing

currentEntryPc :: (MachineWord w) => IoMem (Isa Int Int w w) w -> Int -> Maybe Int
currentEntryPc m top = findCallRecord m top >>= \r -> pureRecordField m r recordField5Offset

-- | Up to @count@ active call frames reachable from control-chain
-- position @top@ (belonging to a frame based at @fb@, whose own operand
-- region extends up to @stackTop@), innermost first, as
-- @(entryPc, frameBase, stackTop)@ triples -- 'frameView' renders each.
-- Stops early if fewer than @count@ frames are actually active.
--
-- A frame's own @stackTop@ is the live `sp` only for the innermost frame;
-- for any caller currently blocked on a call, it's exactly the callee's
-- `frameBase` -- the address where the caller's last push (the callee's
-- arguments, aliased in by 'Call') stopped.
frameChain :: (MachineWord w) => IoMem (Isa Int Int w w) w -> Int -> Int -> Int -> Int -> [(Int, Int, Int)]
frameChain m count top fb stackTop
    | count <= 0 = []
    | otherwise = case findCallRecord m top of
        Nothing -> []
        Just r ->
            let fields =
                    (,,)
                        <$> pureRecordField m r recordField5Offset
                        <*> pureRecordField m r recordField4Offset
                        <*> pureRecordField m r recordLinkOffset
             in case fields of
                    Just (entryPc, callerFb, callerTop) ->
                        (entryPc, fb, stackTop) : frameChain m (count - 1) callerTop callerFb fb
                    Nothing -> []

-- | 'Either' has no 'hush' in scope here; a local one-liner is simpler
-- than pulling in another import for it.
eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just

pureRecordField :: forall w. (MachineWord w) => IoMem (Isa Int Int w w) w -> Int -> Int -> Maybe Int
pureRecordField m addr offset = fromEnum . snd <$> eitherToMaybe (readWord m (addr + offset * byteSizeT @w))

pureRecordKind :: (MachineWord w) => IoMem (Isa Int Int w w) w -> Int -> Maybe RecordKind
pureRecordKind m addr = toEnum <$> pureRecordField m addr recordKindOffset

-- | The current function's declared signature, looked up (for reporting
-- only) via the innermost `Call` record's saved entry pc.
currentFunctionMeta :: (MachineWord w) => MachineState (IoMem (Isa Int Int w w) w) w -> Maybe FunctionMeta
currentFunctionMeta State{mem, ctrlTop, functions} = currentEntryPc mem ctrlTop >>= (`IntMap.lookup` functions)

instance
    (MachineWord w) =>
    StateInterspector (MachineState (IoMem (Isa Int Int w w) w) w) (IoMem (Isa Int Int w w) w) (Isa Int Int w w) w
    where
    programCounter State{pc} = pc
    memoryDump State{mem} = mem
    ioStreams State{mem = IoMem{mIoStreams}} = mIoStreams
    reprState labels st v
        | Just v' <- defaultView labels st v = v'
    reprState labels st v =
        case T.splitOn ":" v of
            ["stack", f] -> stackView f st
            ["locals", f] -> localsView f st
            ["local", name, f] -> localView f name st
            ["frames"] -> show (callDepth st)
            ["frame"] -> frameView "dec" 1 st
            ["frame", cnt] -> withCount cnt $ \n -> frameView "dec" n st
            ["frame", cnt, f] -> withCount cnt $ \n -> frameView f n st
            ["ctrl"] -> ctrlView st
            [r] -> reprState labels st (r <> ":dec")
            [r, _] -> unknownView r
            _ -> errorView v
        where
            formatValues "dec" values = toText $ intercalate ":" $ map show values
            formatValues "hex" values = T.intercalate ":" $ map (toText . word32ToHex) values
            formatValues f _ = unknownFormat f

            step = byteSizeT @w

            withCount cnt k = case readMaybe (toString cnt) of
                Just n | n > 0 -> k n
                _ -> unknownView cnt

            -- Raw words in [lo, hi), top first: a genuine stack dump, so
            -- this includes any block/loop/if control records currently
            -- open above the frame's own locals and its call record
            -- (which always sits right after them, see 'enter'), not
            -- just pure operand values (those interleave once any are
            -- open).
            stackAt mem lo hi f = formatValues f values
                where
                    values = mapMaybe (\a -> eitherToMaybe (readWord mem a) <&> snd) [hi - step, hi - 2 * step .. lo]

            localsAt mem fb meta f =
                T.intercalate ":" $ zipWith (\n value -> toText n <> "=" <> viewRegister f value) names values
                where
                    names = fmLocalNames meta
                    values = map (\i -> maybe def snd (eitherToMaybe (readWord mem (fb + i * step)))) [0 .. length names - 1]

            stackView f st'@State{mem, sp} = case currentFunctionMeta st' of
                Nothing -> ""
                Just meta -> stackAt mem (frameBase st' + length (fmLocalNames meta) * step + recordWidth * step) sp f

            localsView f st'@State{mem} = case currentFunctionMeta st' of
                Nothing -> ""
                Just meta -> localsAt mem (frameBase st') meta f

            -- One line per active frame, innermost first: `#i name:
            -- locals=[...] stack=[...]`, using the same raw-stack-dump
            -- convention as `stack`/`locals` above, just for a frame that
            -- isn't necessarily the current one. Stops early once fewer
            -- than @count@ frames are actually active.
            frameView f count st'@State{mem, functions, ctrlTop} =
                T.intercalate "\n" $ zipWith renderOne [0 :: Int ..] (frameChain mem count ctrlTop (frameBase st') (sp st'))
                where
                    renderOne i (entryPc, fb, hi) = case IntMap.lookup entryPc functions of
                        Nothing -> ident
                        Just meta ->
                            ident
                                <> ": locals=["
                                <> localsAt mem fb meta f
                                <> "] stack=["
                                <> stackAt mem (fb + length (fmLocalNames meta) * step + recordWidth * step) hi f
                                <> "]"
                        where
                            ident = "#" <> show i <> " " <> entryName entryPc
                    entryName entryPc = case find (\(_, addr) -> fromEnum addr == entryPc) (HashMap.toList labels) of
                        Just (name, _) -> name
                        Nothing -> "@" <> show entryPc

            localView f name st'@State{mem} = case currentFunctionMeta st' of
                Nothing -> unknownView name
                Just meta ->
                    let names = fmLocalNames meta
                        byName = snd <$> find ((== toString name) . fst) (zip names [0 :: Int ..])
                        index = case byName of
                            Just i -> Just i
                            Nothing -> readMaybe (toString name)
                     in case index of
                            Just i
                                | i >= 0 && i < length names ->
                                    maybe (unknownView name) (viewRegister f . snd) (eitherToMaybe (readWord mem (frameBase st' + i * step)))
                            _ -> unknownView name

            -- Structured control ids only (skips the enclosing `Call`),
            -- innermost first.
            ctrlView State{mem, ctrlTop} = T.intercalate ":" $ map show (go ctrlTop)
                where
                    go r
                        | r == nullAddr = []
                        | otherwise = case pureRecordKind mem r of
                            Just RecordCall -> []
                            Just _ -> case (pureRecordField mem r recordField4Offset, pureRecordField mem r recordLinkOffset) of
                                (Just label, Just link) -> label : go link
                                _ -> []
                            Nothing -> []

    summaryView _labels State{spMax, callDepthMax, ctrlDepthMax} v = case T.splitOn ":" v of
        ["wasm32", "operand-stack-max"] -> Just $ show spMax
        ["wasm32", "frames-max"] -> Just $ show callDepthMax
        ["wasm32", "control-stack-max"] -> Just $ show ctrlDepthMax
        ["isa-specific"] ->
            Just $
                "wasm32:operand-stack-max: "
                    <> show spMax
                    <> "\n"
                    <> "wasm32:frames-max:        "
                    <> show callDepthMax
                    <> "\n"
                    <> "wasm32:control-stack-max: "
                    <> show ctrlDepthMax
        _ -> Nothing

    isHalted State{stopped} = stopped

lookupAssoc :: (Eq a) => a -> [(a, b)] -> Maybe b
lookupAssoc key = fmap snd . find ((== key) . fst)

instance (MachineWord w) => Machine (MachineState (IoMem (Isa Int Int w w) w) w) (Isa Int Int w w) w where
    instructionFetch = do
        st <- get
        case st of
            State{stopped = True} -> return $ Left halted
            State{internalError = Just err} -> return $ Left err
            State{pc, mem} ->
                case readInstruction mem pc of
                    Left err -> return $ Left err
                    Right (mem', instruction) -> do
                        put st{mem = mem'}
                        return $ Right (pc, instruction)

    instructionExecute _pc instruction =
        case instruction of
            I32Const value -> pushValue value >> nextPc instruction
            Drop -> popValue >> nextPc instruction
            Select -> do
                condition <- popValue
                falseValue <- popValue
                trueValue <- popValue
                pushValue $ if condition /= 0 then trueValue else falseValue
                nextPc instruction
            LocalGet name -> lookupLocal name >>= pushValue >> nextPc instruction
            LocalSet name -> popValue >>= setLocal name >> nextPc instruction
            LocalTee name -> do
                value <- popValue
                setLocal name value
                pushValue value
                nextPc instruction
            I32Add -> binary id id (+) >> nextPc instruction
            I32Sub -> binary id id (-) >> nextPc instruction
            I32Mul -> binary id id (*) >> nextPc instruction
            I32DivS -> signedDiv div >> nextPc instruction
            I32DivU -> unsignedDiv div >> nextPc instruction
            I32RemS -> signedDiv rem >> nextPc instruction
            I32RemU -> unsignedDiv rem >> nextPc instruction
            I32And -> binary id id (.&.) >> nextPc instruction
            I32Or -> binary id id (.|.) >> nextPc instruction
            I32Xor -> binary id id xor >> nextPc instruction
            I32Shl -> binary id id (\x y -> x `shiftL` (fromEnum y .&. 0x1F)) >> nextPc instruction
            I32ShrS -> binary id id (\x y -> x `shiftR` (fromEnum y .&. 0x1F)) >> nextPc instruction
            I32ShrU -> binary fromSign id (\x y -> toSign $ x `shiftR` (fromEnum y .&. 0x1F)) >> nextPc instruction
            I32Eqz -> unary (\x -> if x == 0 then 1 else 0) >> nextPc instruction
            I32Eq -> compareS (==) >> nextPc instruction
            I32Ne -> compareS (/=) >> nextPc instruction
            I32LtS -> compareS (<) >> nextPc instruction
            I32LeS -> compareS (<=) >> nextPc instruction
            I32GtS -> compareS (>) >> nextPc instruction
            I32GeS -> compareS (>=) >> nextPc instruction
            I32LtU -> compareU (<) >> nextPc instruction
            I32LeU -> compareU (<=) >> nextPc instruction
            I32GtU -> compareU (>) >> nextPc instruction
            I32GeU -> compareU (>=) >> nextPc instruction
            I32Load -> popValue >>= getWord . fromEnum >>= pushValue >> nextPc instruction
            I32Store -> do
                value <- popValue
                addr <- popValue
                setWord (fromEnum addr) value
                nextPc instruction
            I32Load8S -> do
                addr <- popValue
                byte <- getByte (fromEnum addr)
                pushValue (fromIntegral (fromIntegral byte :: Int8))
                nextPc instruction
            I32Load8U -> do
                addr <- popValue
                byte <- getByte (fromEnum addr)
                pushValue (fromIntegral byte)
                nextPc instruction
            I32Store8 -> do
                value <- popValue
                addr <- popValue
                setByte (fromEnum addr) (fromIntegral value)
                nextPc instruction
            Block label -> do
                State{pc, mem} <- get
                case findEndPc mem (pc + byteSize instruction) of
                    Right endPc -> enter endPc 0 (BlockExtra label) >> nextPc instruction
                    Left err -> raiseInternalError $ "control flow error: " <> err
            Loop label -> do
                State{pc, mem} <- get
                case findEndPc mem (pc + byteSize instruction) of
                    Right endPc -> enter endPc 0 (LoopExtra label (pc + byteSize instruction)) >> nextPc instruction
                    Left err -> raiseInternalError $ "control flow error: " <> err
            If label -> do
                condition <- popValue
                State{pc, mem} <- get
                case resolveIfTargets mem (pc + byteSize instruction) of
                    Right (elsePc, endPc)
                        | condition /= 0 -> enter endPc 0 (IfExtra label) >> nextPc instruction
                        | Just elseAddr <- elsePc -> do
                            enter endPc 0 (IfExtra label)
                            setPc (elseAddr + byteSize Else)
                        | otherwise -> setPc (endPc + byteSize End)
                    Left err -> raiseInternalError $ "control flow error: " <> err
            Else -> executeElse
            End -> do
                State{ctrlTop} <- get
                collapse ctrlTop False
            Br label -> branch label
            BrIf label -> do
                condition <- popValue
                if condition /= 0 then branch label else nextPc instruction
            Call target -> do
                State{pc, mem} <- get
                case readInstruction mem (fromEnum target) of
                    Right (mem', FuncEnter paramCount declaredLocalCount resultCount) -> do
                        modify $ \st -> st{mem = mem'}
                        State{sp, frameBase = callerFrameBase} <- get
                        let calleeFrameBase = sp - paramCount * byteSizeT @w
                        replicateM_ declaredLocalCount (pushValue def)
                        enter (pc + byteSize instruction) resultCount (CallExtra callerFrameBase (fromEnum target))
                        modify $ \st -> st{frameBase = calleeFrameBase}
                        setPc (fromEnum target + funcEnterSize)
                    Right _ -> raiseInternalError "call target does not point to .func"
                    Left err -> raiseInternalError $ "control flow error: " <> err
            Return -> do
                result <- findCall
                case result of
                    Right r -> unwindTo r False
                    Left err -> raiseInternalError err
            Halt -> modify $ \st -> st{stopped = True}
            Unreachable -> raiseInternalError "unreachable"
            Nop -> nextPc instruction
            FuncEnter{} -> raiseInternalError "fell into function metadata; call should have jumped past it"
        where
            unary f = popValue >>= pushValue . f
            binary f1 f2 op = do
                y <- f2 <$> popValue
                x <- f1 <$> popValue
                pushValue (op x y)
            signedDiv op = do
                y <- popValue
                x <- popValue
                if y == 0
                    then raiseInternalError "integer divide by zero"
                    else
                        if x == minBound && y == -1
                            then raiseInternalError "integer overflow"
                            else pushValue (x `op` y)
            unsignedDiv op = do
                y <- fromSign <$> popValue
                x <- fromSign <$> popValue
                if y == 0
                    then raiseInternalError "integer divide by zero"
                    else pushValue (toSign (x `op` y))
            compareS op = do
                y <- popValue
                x <- popValue
                pushValue $ if x `op` y then 1 else 0
            compareU op = do
                y <- fromSign <$> popValue
                x <- fromSign <$> popValue
                pushValue $ if x `op` y then 1 else 0
