{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
A 32-bit stack-based instruction set for teaching, inspired by
<https://webassembly.github.io/spec/core/ WebAssembly>. It features:

* Two stacks, both in ordinary memory: an /operand stack/ holding locals
  and values, and a /control stack/ holding the bookkeeping for open
  @block@\/@loop@\/call scopes -- no general-purpose registers.
* Structured control flow (@block@, @loop@, @if@\/@else@) instead of
  arbitrary jumps, with every scope's target resolved at assembly time.
* Function calls where a function address is just an ordinary value, so a
  direct call and a call through a value computed at run time are the same
  instruction.
* Memory-mapped I\/O.

Which makes it useful for studying function calls, local variables, loops
and low-level memory access in a model that fits in one sitting.

Comments in Wasm32 assembly are introduced by @;@.

The per-instruction reference is on 'Wasm32Isa's own constructors. The
sections below describe how the machine works underneath: start with
\"The two stacks\", since everything else refers to it.
-}
module Wrench.Isa.Wasm32 (
    -- * Instructions
    -- $instructionIndex
    Wasm32Isa (..),

    -- * Machine state
    Wasm32St (..),

    -- * Program structure
    -- $programStructure

    -- * The two stacks
    -- $stacks

    -- ** Locals
    -- $locals

    -- ** Operand values
    -- $operands

    -- * Control flow
    -- $controlFlow

    -- * Functions
    -- $functions

    -- * Differences from WebAssembly
    -- $differences
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Default (def)
import Data.Text qualified as T
import Relude
import Relude.Extra ((!?))
import Text.Megaparsec (choice, try)
import Text.Megaparsec.Char (char, hspace, hspace1, string)
import Wrench.Isa.Wasm32.ControlRecord
import Wrench.Isa.Wasm32.Layout
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Report
import Wrench.Translator.Parser.Misc
import Wrench.Translator.Parser.Types
import Wrench.Translator.Types

data Wasm32Isa w l
    = -- | @i32.const \<value\>@ -- push an immediate onto the operand stack.
      -- A label reference is an ordinary immediate, which is how a function
      -- address is produced (see the \"Functions\" section).
      --
      -- > stack.push(value)
      I32Const l
    | -- | @i32.add@ -- > y <- pop; x <- pop; push (x + y)
      I32Add
    | -- | @i32.sub@ -- subtract the second-pushed from the first-pushed.
      --
      -- > y <- pop; x <- pop; push (x - y)
      I32Sub
    | -- | @i32.mul@ -- > y <- pop; x <- pop; push (x * y)
      I32Mul
    | -- | @i32.div_s@ -- signed division, truncating toward zero, so
      -- @-7 \/ 2@ is @-3@ and not @-4@. Traps on a zero divisor and on the
      -- one unrepresentable quotient, @minBound \/ -1@.
      --
      -- > y <- pop; x <- pop; push (trunc (x / y))
      I32DivS
    | -- | @i32.rem_s@ -- signed remainder, whose sign follows the dividend,
      -- so @-7 % 2@ is @-1@. Only a zero divisor traps: unlike 'I32DivS',
      -- @minBound % -1@ is defined, and is @0@.
      --
      -- > y <- pop; x <- pop; push (x - y * trunc (x / y))
      I32RemS
    | -- | @i32.and@ -- > y <- pop; x <- pop; push (x .&. y)
      I32And
    | -- | @i32.or@ -- > y <- pop; x <- pop; push (x .|. y)
      I32Or
    | -- | @i32.xor@ -- > y <- pop; x <- pop; push (xor x y)
      I32Xor
    | -- | @i32.shl@ -- shift left, masking the shift amount to its low 5
      -- bits, so a shift of 33 is a shift of 1.
      --
      -- > y <- pop; x <- pop; push (x << (y .&. 0x1F))
      I32Shl
    | -- | @i32.shr_s@ -- arithmetic (sign-extending) shift right, same
      -- 5-bit masking.
      --
      -- > y <- pop; x <- pop; push (x >>arithmetic (y .&. 0x1F))
      I32ShrS
    | -- | @i32.shr_u@ -- logical (zero-filling) shift right, same 5-bit
      -- masking.
      --
      -- > y <- pop; x <- pop; push (unsigned x >>logical (y .&. 0x1F))
      I32ShrU
    | -- | @i32.eq@ -- push @1@ if the two values are equal, else @0@. With
      -- @i32.const 0@ in front of it this is also the ISA's zero test and
      -- its only negation (see the instruction index above).
      --
      -- > y <- pop; x <- pop; push (if x == y then 1 else 0)
      I32Eq
    | -- | @i32.lt_s@ -- signed @\<@. > y <- pop; x <- pop; push (x < y)
      I32LtS
    | -- | @i32.le_s@ -- signed @\<=@.
      I32LeS
    | -- | @i32.gt_s@ -- signed @\>@.
      I32GtS
    | -- | @i32.ge_s@ -- signed @\>=@.
      I32GeS
    | -- | @i32.lt_u@ -- unsigned @\<@, reading both operands as unsigned,
      -- so @-1@ compares /greater/ than @1@.
      I32LtU
    | -- | @i32.le_u@ -- unsigned @\<=@.
      I32LeU
    | -- | @i32.gt_u@ -- unsigned @\>@.
      I32GtU
    | -- | @i32.ge_u@ -- unsigned @\>=@.
      I32GeU
    | -- | @i32.load [\<offset\>]@ -- pop an address, push the 4-byte word at
      -- address plus offset.
      --
      -- Every memory instruction carries a static offset, added to the
      -- popped address: the base is computed at run time and pushed, the
      -- displacement into it is written into the instruction, which makes
      -- indexing a struct field or a small array one instruction rather
      -- than three. The offset is unsigned and one byte
      -- ('immediateBitWidth'), so @0@-@255@; omit it when it is zero, and
      -- reach further by computing the address with @i32.add@.
      --
      -- > i32.const point       ; base address
      -- > i32.load 4            ; point.y, no i32.add needed
      --
      -- > addr <- pop; push mem[addr + offset]
      I32Load Int
    | -- | @i32.store [\<offset\>]@ -- pop a value, then an address, and write
      -- the value's 4 bytes at address plus offset. The value sits /above/
      -- the address, matching real WebAssembly, so the destination has to
      -- be pushed before the value is computed.
      --
      -- > value <- pop; addr <- pop; mem[addr + offset] <- value
      I32Store Int
    | -- | @i32.load8_u [\<offset\>]@ -- like 'I32Load', but reads one byte
      -- and zero-extends it.
      I32Load8U Int
    | -- | @i32.load8_s [\<offset\>]@ -- like 'I32Load8U', but sign-extends.
      I32Load8S Int
    | -- | @i32.store8 [\<offset\>]@ -- like 'I32Store', but writes only the
      -- value's low byte.
      I32Store8 Int
    | -- | @if@ -- pop a condition; fall into the body that follows if it is
      -- non-zero, otherwise skip to the matching @else@ (if there is one) or
      -- past the matching @end@.
      --
      -- The @Int@ is the byte distance to resume at on the skip path,
      -- resolved at translate time by 'resolveScopeTargets' and not written
      -- in source. @if@ pushes no control record: nothing branches out of a
      -- taken branch early, so there is nothing to remember.
      If Int
    | -- | @else@ -- marks the alternative body, reached only by a taken
      -- @if@-body falling through to it, at which point it skips
      -- unconditionally past the matching @end@. So the @else@-body never
      -- runs after the @if@-body already did.
      Else Int
    | -- | @end@ -- closes the innermost open @block@ or @loop@. An @if@\'s
      -- @end@ closes nothing, since @if@ opened nothing.
      End
    | -- | @loop@ -- open a scope a branch can jump /backward/ into, landing
      -- just after this instruction. On its own it runs its body once, like
      -- @block@. The @Int@ is the byte distance to its own matching @end@.
      Loop Int
    | -- | @block@ -- open a scope a branch can jump /forward/ past, landing
      -- just after its matching @end@. This is what makes a \"break\"
      -- possible, which @loop@ alone does not. The @Int@ is the byte
      -- distance to that @end@.
      Block Int
    | -- | @br \<depth\>@ -- branch unconditionally to the enclosing scope
      -- @depth@ levels out, @0@ being the innermost. See the \"Control
      -- flow\" section for what reaching a @loop@ versus a @block@ does.
      Br Int
    | -- | @br_if \<depth\>@ -- pop a condition and branch like 'Br' only
      -- if it is non-zero, otherwise fall through. Branching on /zero/ is
      -- @i32.const 0@, @i32.eq@, @br_if@.
      BrIf Int
    | -- | @dup@ -- duplicate the top of the operand stack.
      --
      -- > x <- pop; push x; push x
      Dup
    | -- | @drop@ -- discard the top of the operand stack. 'Dup' the
      -- other way round, and the only way to get rid of a value the program
      -- does not want -- a callee result the caller ignores, say. Nothing
      -- else can do it: @local.set@ needs a spare local to land in, and
      -- @i32.store@ needs its destination pushed /under/ the value.
      --
      -- > pop
      Drop
    | -- | @select@ -- pop a condition, then two values; push the
      -- first-pushed of the two if the condition is non-zero, the second
      -- otherwise. A branchless two-way choice.
      --
      -- > c <- pop; y <- pop; x <- pop; push (if c /= 0 then x else y)
      Select
    | -- | @unreachable@ -- trap. Marks a point the program believes it can
      -- never reach, and says so if it does.
      Unreachable
    | -- | @local.get \<i\>@ -- push local @i@\'s value.
      --
      -- > push locals[i]
      LocalGet Int
    | -- | @local.set \<i\>@ -- pop the top of the stack into local @i@.
      --
      -- > locals[i] <- pop
      LocalSet Int
    | -- | @local.tee \<i\>@ -- like @local.set@, but pushes the value back,
      -- leaving the stack depth unchanged.
      --
      -- > x <- pop; locals[i] <- x; push x
      LocalTee Int
    | -- | @locals.reserve \<n\>@ -- claim @n@ more locals, zeroed, at the
      -- indices directly after the ones the frame already has. Belongs at a
      -- function's entry, before anything is pushed and outside any loop --
      -- see the \"Locals\" section for what happens otherwise. @n@ is one
      -- byte, so @0@-@255@, and reserving past the operand stack's wall is
      -- an @operand stack overflow@. @return@ reclaims them along with the
      -- parameters.
      --
      -- > sp <- sp - n*4; mem[sp .. sp + n*4 - 4] <- 0; localCount <- localCount + n
      LocalsReserve Int
    | -- | @call \<paramCount\>, \<resultCount\>@ -- pop a target address, an
      -- ordinary value, so a known target and one computed at run time are
      -- the same instruction. The @paramCount@ values pushed just before it
      -- become the callee's locals; @resultCount@ values are expected back.
      -- Neither count is checked against the callee. See the \"Functions\"
      -- section.
      Call Int Int
    | -- | @return@ -- return from the nearest enclosing call, closing any
      -- @block@ or @loop@ still open along the way and forwarding the
      -- declared results to the caller.
      Return
    | -- | @sp.init@ -- pop an address and make it the root both stacks grow
      -- from: operands descend below it, control records ascend above it.
      -- This is how a program trades operand space against scope and call
      -- depth; 'stackRoot' is the default split. Only meaningful before
      -- anything has been pushed: it resets the frame to having no locals,
      -- and abandons rather than unwinds any control record already open.
      --
      -- The address has to land in the stack region ('operandFloor' to the
      -- end of memory) -- outside it there is no root to speak of, and a
      -- root below the region would put a frame's locals in @.data@.
      -- Within the region nothing checks the split leaves either stack
      -- enough room; that is the trade the instruction exists to make.
      --
      -- > addr <- pop; sp <- addr; ctrlSp <- addr; frameBase <- addr - 4
      SpInit
    | -- | @halt@ -- stop execution.
      Halt
    deriving (Eq, Functor, Show)

instance CommentStart (Wasm32Isa w l) where
    commentStart = ";"

instance (IsWord w) => MnemonicParser (Wasm32Isa w (Ref w)) where
    mnemonic =
        hspace *> cmd <* (hspace1 <|> eol' (commentStart @(Wasm32Isa _ _)))
        where
            cmd =
                choice
                    [ I32Const <$> cmd1 "i32.const" referenceWithDirective
                    , cmd0 "i32.add" I32Add
                    , cmd0 "i32.sub" I32Sub
                    , cmd0 "i32.mul" I32Mul
                    , cmd0 "i32.div_s" I32DivS
                    , cmd0 "i32.rem_s" I32RemS
                    , cmd0 "i32.and" I32And
                    , cmd0 "i32.or" I32Or
                    , cmd0 "i32.xor" I32Xor
                    , cmd0 "i32.shl" I32Shl
                    , cmd0 "i32.shr_s" I32ShrS
                    , cmd0 "i32.shr_u" I32ShrU
                    , cmd0 "i32.eq" I32Eq
                    , cmd0 "i32.lt_s" I32LtS
                    , cmd0 "i32.le_s" I32LeS
                    , cmd0 "i32.gt_s" I32GtS
                    , cmd0 "i32.ge_s" I32GeS
                    , cmd0 "i32.lt_u" I32LtU
                    , cmd0 "i32.le_u" I32LeU
                    , cmd0 "i32.gt_u" I32GtU
                    , cmd0 "i32.ge_u" I32GeU
                    , -- Longer alternative first: a bare-word `choice`
                      -- alternative doesn't backtrack once it has
                      -- consumed input, so "i32.load" ahead of
                      -- "i32.load8_u" would swallow the prefix and then
                      -- fail on "8_u". Same for "i32.store"/"i32.store8".
                      -- The `try`-wrapped alternatives below are immune.
                      memOp "i32.load8_u" I32Load8U
                    , memOp "i32.load8_s" I32Load8S
                    , memOp "i32.load" I32Load
                    , memOp "i32.store8" I32Store8
                    , memOp "i32.store" I32Store
                    , -- The four scope instructions take no operand in
                      -- source: their target is their own matching @end@,
                      -- which 'resolveStructure' fills in at translate
                      -- time. 'unresolvedTarget' marks the placeholder.
                      cmd0 "if" (If unresolvedTarget)
                    , cmd0 "else" (Else unresolvedTarget)
                    , cmd0 "end" End
                    , cmd0 "loop" (Loop unresolvedTarget)
                    , cmd0 "block" (Block unresolvedTarget)
                    , try (Br <$> cmd1 "br" intLit)
                    , try (BrIf <$> cmd1 "br_if" intLit)
                    , cmd0 "dup" Dup
                    , cmd0 "drop" Drop
                    , cmd0 "select" Select
                    , cmd0 "unreachable" Unreachable
                    , try (LocalGet <$> cmd1 "local.get" intLit)
                    , try (LocalSet <$> cmd1 "local.set" intLit)
                    , try (LocalTee <$> cmd1 "local.tee" intLit)
                    , try (LocalsReserve <$> cmd1 "locals.reserve" intLit)
                    , try
                        ( do
                            void $ string "call"
                            hspace1
                            params <- intLit
                            comma
                            Call params <$> intLit
                        )
                    , cmd0 "return" Return
                    , cmd0 "sp.init" SpInit
                    , cmd0 "halt" Halt
                    ]

cmd0 :: String -> a -> Parser a
cmd0 mnemonic constructor = string mnemonic >> return constructor

cmd1 :: String -> Parser a -> Parser a
cmd1 mnemonic arg = string mnemonic >> hspace1 >> arg

-- | A load\/store, whose static offset may be left off when it is zero:
-- @i32.load@ and @i32.load 0@ assemble to the same thing.
memOp :: String -> (Int -> a) -> Parser a
memOp mnemonic constructor =
    string mnemonic >> (constructor <$> (try (hspace1 >> intLit) <|> pure 0))

-- | An integer immediate -- a branch depth, a local index, one of
-- @call@'s counts, a load\/store offset -- in decimal or @0x@ hex.
-- Fails as a parser rather than handing an empty string to @read@:
-- 'num' is built from 'many', so it matches nothing happily, and an
-- optional immediate (see 'memOp') needs to find that out by
-- backtracking.
intLit :: Parser Int
intLit = do
    digits <- try hexNum <|> num
    maybe (fail $ "expected an integer literal, got " <> show digits) return (readMaybe digits)

-- | Separates @call@'s two operands (paramCount, resultCount).
comma :: Parser ()
comma = hspace >> void (char ',') >> hspace

instance (IsWord w) => DerefMnemonic (Wasm32Isa w) w where
    resolveStructure = resolveScopeTargets

    -- Exactly one constructor carries a label, and @l@ is this type's
    -- last parameter, so the derived 'Functor' already is this
    -- traversal: the longhand version was 44 lines of @X -> X@ around
    -- the one line that does anything.
    derefMnemonic f _offset = fmap (deref' f)

-- | Match every @block@\/@loop@\/@if@\/@else@ with its own @end@ and
-- write the byte distance between them into the instruction, once, at
-- translate time -- so the interpreter only ever adds it to @pc@, and an
-- unbalanced @end@ is a translation error rather than something found
-- when a run-time forward scan falls off the end of memory.
resolveScopeTargets :: forall w. (IsWord w) => [(w, Wasm32Isa w w)] -> Either Text [Wasm32Isa w w]
resolveScopeTargets marked = do
    mapM_ checkSourceOperands marked
    resolved <- matchScopes marked
    mapM_ checkResolvedOperands resolved
    return (map snd resolved)

-- | Walk the stream pairing each scope instruction with its own @end@,
-- and write the byte distance between them into it. Unbalanced nesting
-- is a 'Left' naming the offending address.
matchScopes :: forall w. (IsWord w) => [(w, Wasm32Isa w w)] -> Either Text [(w, Wasm32Isa w w)]
matchScopes marked = do
    patches <- go [] (zip [0 ..] marked)
    let patched = fromList patches :: IntMap Int
        resolve i (addr, instruction) = (addr, maybe instruction (retarget instruction) (patched !? i))
    return (zipWith resolve [0 ..] marked)
    where
        -- \| One entry per scope still waiting for its @end@: the index of
        -- the instruction to patch, its own address, and whether that
        -- instruction wants the @end@'s own address ('AtEnd', for
        -- @block@\/@loop@, which store it as @csEnd@) or the address just
        -- past it ('PastEnd', for the @if@\/@else@ that skip over it).
        go open [] = case open of
            [] -> Right []
            (_, addr, _) : _ -> Left $ "block/loop/if at " <> hexAddr 2 addr <> " has no matching end"
        go open ((i, (addr, instruction)) : rest) = case instruction of
            Block{} -> go ((i, fromEnum addr, AtEnd) : open) rest
            Loop{} -> go ((i, fromEnum addr, AtEnd) : open) rest
            If{} -> go ((i, fromEnum addr, PastEnd) : open) rest
            Else{} -> case open of
                -- The @if@ resumes at the first instruction of this
                -- @else@'s body; this @else@ takes the @if@'s place in the
                -- open list, to be closed by the same @end@.
                (j, ifAddr, PastEnd) : outer ->
                    ((j, fromEnum addr + byteSize instruction - ifAddr) :)
                        <$> go ((i, fromEnum addr, PastEnd) : outer) rest
                _ -> Left $ "else at " <> hexAddr 2 (fromEnum addr) <> " has no matching if"
            End -> case open of
                (j, openAddr, want) : outer ->
                    let target = case want of
                            AtEnd -> fromEnum addr
                            PastEnd -> fromEnum addr + byteSize End
                     in ((j, target - openAddr) :) <$> go outer rest
                [] -> Left $ "end at " <> hexAddr 2 (fromEnum addr) <> " has no matching block/loop/if"
            _ -> go open rest

        retarget instruction d = case instruction of
            Block{} -> Block d
            Loop{} -> Loop d
            If{} -> If d
            Else{} -> Else d
            other -> other

-- | One operand an instruction encodes after its opcode byte. Three
-- things need to agree about every one of them -- how many bytes the
-- instruction occupies, what range the assembler accepts, and when it
-- is in a position to check -- so they are all read off a single
-- description of the instruction's shape ('operands') rather than
-- maintained as three parallel case expressions.
data Operand
    = -- | An immediate written in source: already final when the
      -- assembler first sees the instruction.
      Source Text Int Int
    | -- | A target 'resolveScopeTargets' fills in, holding
      -- 'unresolvedTarget' until it does -- so it is only worth checking
      -- once resolution has run.
      Target Text Int Int
    | -- | A full machine word. No range for a value to fall outside, which
      -- is why a label reference needs no check.
      FullWord

-- | Every operand an instruction carries, in the order it encodes them.
--
-- Listed exhaustively, with no catch-all: an instruction added without a
-- line here is a missing-pattern warning, where a catch-all would have
-- silently given it a bare opcode's size and no range check on its
-- immediate. That combination is how @call 0, 70@ once assembled happily
-- and then became @call 0, 6@.
operands :: Wasm32Isa w l -> [Operand]
operands instruction = case instruction of
    I32Const{} -> [FullWord]
    I32Add -> []
    I32Sub -> []
    I32Mul -> []
    I32DivS -> []
    I32RemS -> []
    I32And -> []
    I32Or -> []
    I32Xor -> []
    I32Shl -> []
    I32ShrS -> []
    I32ShrU -> []
    I32Eq -> []
    I32LtS -> []
    I32LeS -> []
    I32GtS -> []
    I32GeS -> []
    I32LtU -> []
    I32LeU -> []
    I32GtU -> []
    I32GeU -> []
    I32Load o -> [offset o]
    I32Store o -> [offset o]
    I32Load8U o -> [offset o]
    I32Load8S o -> [offset o]
    I32Store8 o -> [offset o]
    If d -> [target "if body" d]
    Else d -> [target "else body" d]
    End -> []
    Loop d -> [target "loop body" d]
    Block d -> [target "block body" d]
    Br d -> [narrow "br depth" d]
    BrIf d -> [narrow "br_if depth" d]
    Dup -> []
    Drop -> []
    Select -> []
    Unreachable -> []
    LocalGet i -> [narrow "local.get index" i]
    LocalSet i -> [narrow "local.set index" i]
    LocalTee i -> [narrow "local.tee index" i]
    LocalsReserve n -> [narrow "locals.reserve count" n]
    Call p r -> [narrow "call paramCount" p, narrow "call resultCount" r]
    Return -> []
    SpInit -> []
    Halt -> []
    where
        narrow name = Source name immediateBitWidth
        offset = narrow "load/store offset"
        target name = Target name wideImmediateBitWidth

-- | How many bytes an operand occupies, rounding a field that is not a
-- whole number of bits up: it still has to be stored in whole bytes.
operandBytes :: Operand -> Int
operandBytes FullWord = byteSizeT @Int32
operandBytes (Source _ width _) = (width + 7) `div` 8
operandBytes (Target _ width _) = (width + 7) `div` 8

-- | Reject an operand whose value doesn't fit the field it is encoded in,
-- naming the field and the instruction's own address. Masking a high bit
-- off doesn't make a smaller number, it makes a different one, so this is
-- a source error rather than something to fix up at run time.
operandFits :: (IsWord w) => w -> Operand -> Either Text ()
operandFits addr operand = case operand of
    FullWord -> Right ()
    Source name width value -> fits name width value
    Target name width value -> fits name width value
    where
        fits name width value
            | value >= 0 && value < 1 `shiftL` width = Right ()
            | otherwise =
                Left $
                    name
                        <> " "
                        <> show value
                        <> " at "
                        <> hexAddr 2 (fromEnum addr)
                        <> " doesn't fit in "
                        <> show width
                        <> " bits"

-- | Check the operands written in source. Runs before resolution, so it
-- has to skip the scope targets -- they are still 'unresolvedTarget'.
checkSourceOperands :: forall w. (IsWord w) => (w, Wasm32Isa w w) -> Either Text ()
checkSourceOperands (addr, instruction) =
    traverse_ (operandFits addr) [o | o@Source{} <- operands instruction]

-- | Check the targets 'matchScopes' just wrote: a body too long for the
-- forward distance its own scope instruction can carry.
checkResolvedOperands :: forall w. (IsWord w) => (w, Wasm32Isa w w) -> Either Text ()
checkResolvedOperands (addr, instruction) =
    traverse_ (operandFits addr) [o | o@Target{} <- operands instruction]

-- | Which address a scope instruction wants out of its own matching
-- @end@ -- see 'resolveScopeTargets'.
data ScopeTargetKind = AtEnd | PastEnd

instance ByteSize (Wasm32Isa w l) where
    byteSize instruction = 1 + sum (map operandBytes (operands instruction))

-- | A scope instruction's forward distance to its own matching @end@.
-- Two bytes, so any body that fits in the largest configurable memory
-- can be jumped over.
wideImmediateBitWidth :: Int
wideImmediateBitWidth = 16

-- | Every one-byte instruction immediate: a branch depth, a local index,
-- one of @call@'s counts, a load\/store offset. A 'CallScope' stores the
-- counts at this same width, so what the source can say and what a call
-- record can hold agree. A 0-255 offset covers a struct field or a small
-- array index; further is still reachable with @i32.add@.
immediateBitWidth :: Int
immediateBitWidth = 8

-- | What the parser puts in a scope instruction's target slot before
-- 'resolveStructure' matches it with its own @end@. Out of range on
-- purpose, so an unresolved target can't pass for a real distance.
unresolvedTarget :: Int
unresolvedTarget = -1

data Wasm32St w = Wasm32St
    { pc :: Int
    , sp :: Int
    -- ^ Top of the operand\/locals stack (descending)
    , ctrlBase :: Int
    -- ^ The root both stacks grow from: the operand stack descends from
    -- it, the control stack ascends from it. Set by 'initState' and
    -- relocated by @sp.init@ (see 'stackRoot').
    , ctrlSp :: Int
    -- ^ The control stack's frontier: one past the innermost live
    -- control record, ascending from 'ctrlBase' the way @sp@ descends
    -- from the top of memory. Every record is the same width and they
    -- close in strict LIFO order, so this one pointer locates all of
    -- them ('scopeAddrs') and closing one is a bump-pointer retreat.
    , frameBase :: Int
    -- ^ Base address of the *active* function's locals (param 0's
    -- address): carved out of the caller's already-pushed arguments by
    -- @call@, saved in the callee's 'CallScope', restored by @return@.
    , localCount :: Int
    -- ^ How many words at @frameBase@ are locals rather than operand
    -- stack -- the active function's own paramCount, saved and restored
    -- alongside @frameBase@ so the report views know where this frame's
    -- stack really starts.
    , memAccessCount :: Int
    -- ^ How many memory accesses the *last executed* instruction made,
    -- its own fetch included. Reset per step in 'instructionStep';
    -- exposed to reports as @memAccesses@.
    , mem :: IoMem (Wasm32Isa w w) w
    , stopped :: Bool
    , internalError :: Maybe Text
    }
    deriving (Show)

type instance MemOf (Wasm32St w) = IoMem (Wasm32Isa w w) w

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
            , mem = dump
            , stopped = False
            , internalError = Nothing
            }

-- | Where code+data end and the stacks' own region begins: the lower
-- half of memory holds code+data, the upper half the stacks. Code+data
-- spilling past it is not checked.
memTop :: IoMem (Wasm32Isa w w) w -> Int
memTop IoMem{mIoCells = Mem{memorySize}} = memorySize `div` 2

-- | The lowest address the operand stack may occupy: 'memTop', except
-- that a memory-mapped I\/O port mapped at or above it pushes the floor
-- up past the port.
--
-- A port is ordinary memory to every instruction, so without this a
-- deep-enough operand stack writes /through/ a port: with
-- @memory_size: 0x100@ the stack region starts at @0x80@, which is
-- exactly where the examples map theirs, and the 24th push would emit a
-- character instead of reporting an overflow. Every example that maps
-- ports gives itself at least @0x200@, keeping them in the code+data
-- half where they belong, so this only ever fires on a memory too small
-- for the ports it asks for -- and then it fires as a stack overflow
-- naming the port, instead of silently doing I\/O.
operandFloor :: forall w. (IsWord w) => IoMem (Wasm32Isa w w) w -> Int
operandFloor mem@IoMem{mIoKeys} =
    foldr max (memTop mem) [port + byteSizeT @w | port <- mIoKeys, port >= memTop mem]

-- | The root both stacks grow from, three quarters of the way up the
-- stack's half of memory: the operand stack descends from it toward
-- 'memTop', the control stack ascends from it toward the end of memory.
-- Neither can reach the other, and each has one fixed wall of its own.
--
-- Three quarters rather than half because a program pushes far more
-- operands than it opens scopes -- a @block@ or a call is eight bytes,
-- and the deepest nesting in the examples is four. A program wanting
-- the other balance moves the root with @sp.init@.
--
-- A push decrements before writing, so the root itself is never written
-- to, and @_start@'s @frameBase@ sits one word below it -- the relation
-- @call@ sets up for a frame with no parameters (see 'localAddr').
stackRoot :: IoMem (Wasm32Isa w w) w -> Int
stackRoot mem@IoMem{mIoCells = Mem{memorySize}} =
    let base = memTop mem in base + 3 * (memorySize - base) `div` 4

setPc :: Int -> State (Wasm32St w) ()
setPc addr = modify $ \st -> st{pc = addr}

nextPc :: Wasm32Isa w w -> State (Wasm32St w) ()
nextPc instruction = do
    Wasm32St{pc} <- get
    setPc (pc + byteSize instruction)

raiseInternalError :: Text -> State (Wasm32St w) ()
raiseInternalError msg = modify $ \st -> st{internalError = Just msg}

-- | Counts every 'getWord'\/'setWord'\/'getByte'\/'setByte'\/
-- 'readInstruction' call -- operand pushes\/pops, control-record and
-- local access, and the instruction's own fetch -- toward
-- 'memAccessCount', reset per step in 'instructionStep'.
countMemAccess :: State (Wasm32St w) ()
countMemAccess = modify $ \st -> st{memAccessCount = memAccessCount st + 1}

getWord :: (IsWord w) => Int -> State (Wasm32St w) w
getWord addr = do
    countMemAccess
    st@Wasm32St{mem} <- get
    case readWord mem addr of
        Right (mem', w) -> put st{mem = mem'} >> return w
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return def

setWord :: (IsWord w) => Int -> w -> State (Wasm32St w) ()
setWord addr w = do
    countMemAccess
    st@Wasm32St{mem} <- get
    case writeWord mem addr w of
        Right mem' -> put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err

getByte :: (IsWord w) => Int -> State (Wasm32St w) Word8
getByte addr = do
    countMemAccess
    st@Wasm32St{mem} <- get
    case readByte mem addr of
        Right (mem', b) -> put st{mem = mem'} >> return b
        Left err -> do
            raiseInternalError $ "memory access error: " <> err
            return 0

setByte :: (IsWord w) => Int -> Word8 -> State (Wasm32St w) ()
setByte addr b = do
    countMemAccess
    st@Wasm32St{mem} <- get
    case writeByte mem addr b of
        Right mem' -> put st{mem = mem'}
        Left err -> raiseInternalError $ "memory access error: " <> err

-- | The two stacks grow apart from 'ctrlBase', so neither can ever reach
-- the other; what each can reach is its own end of the stack region.
-- These two check a push against that wall before it happens, so an
-- overflow is reported where it starts rather than found later as
-- corrupted globals or a confusing memory-access error. The two get
-- different messages because the fixes differ: too much on the operand
-- stack, or too deep a nesting of scopes and calls. A real machine
-- draws these lines with a stack-limit register.
checkOperandRoom :: forall w. (IsWord w) => Int -> State (Wasm32St w) () -> State (Wasm32St w) ()
checkOperandRoom newSp act = do
    Wasm32St{mem} <- get
    let wall = operandFloor mem
    if newSp >= wall
        then act
        else
            raiseInternalError $
                "operand stack overflow: push to "
                    <> hexAddr 2 newSp
                    <> " would reach below the stack region at "
                    <> hexAddr 2 wall

checkControlRoom :: (IsWord w) => Int -> State (Wasm32St w) () -> State (Wasm32St w) ()
checkControlRoom newCtrlSp act = do
    Wasm32St{mem} <- get
    let wall = memCapacity mem
    if newCtrlSp <= wall
        then act
        else
            raiseInternalError $
                "control stack overflow: scope record ending at "
                    <> hexAddr 2 newCtrlSp
                    <> " would reach past the end of memory at "
                    <> hexAddr 2 wall

-- | The operand stack grows downward, and @sp@ names the top word
-- rather than the next free slot -- so a push decrements, then writes.
pushValue :: forall w. (IsWord w) => w -> State (Wasm32St w) ()
pushValue value = do
    Wasm32St{sp} <- get
    let sp' = sp - byteSizeT @w
    checkOperandRoom sp' $ do
        modify $ \st -> st{sp = sp'}
        setWord sp' value

-- | No underflow guard: popping past the bottom of the active frame's
-- own operand stack reads whatever is there. Running off the top of
-- memory is caught, but as a plain memory-access error.
popValue :: forall w. (IsWord w) => State (Wasm32St w) w
popValue = do
    Wasm32St{sp} <- get
    value <- getWord sp
    modify $ \st -> st{sp = sp + byteSizeT @w}
    return value

-- | Address of local @i@: a fixed offset from @frameBase@, unaffected by
-- either stack pointer -- which is the point, since a value merely
-- sitting on a stack isn't. Locals count *down*, so param 0 sits
-- highest: the order the caller pushed them in.
localAddr :: forall w. (IsWord w) => Int -> State (Wasm32St w) Int
localAddr i = do
    Wasm32St{frameBase} <- get
    return $ frameBase - i * byteSizeT @w

-- | Addresses of every open control record, innermost first. The
-- control stack is a fixed-stride array growing up from 'ctrlBase', so
-- this is arithmetic -- no stored links, no memory read to find the
-- next record.
scopeAddrs :: forall w. (IsWord w) => Wasm32St w -> [Int]
scopeAddrs Wasm32St{ctrlSp, ctrlBase} =
    takeWhile (>= ctrlBase) [ctrlSp - stride, ctrlSp - 2 * stride ..]
    where
        stride = scopeRecordBytes @w

-- | Address of the innermost open record, or 'nullAddr' when nothing is
-- open -- derived from @ctrlSp@ rather than tracked alongside it.
ctrlTopOf :: forall w. (IsWord w) => Wasm32St w -> Int
ctrlTopOf = fromMaybe nullAddr . listToMaybe . scopeAddrs @w

-- | Read and decode the control record at @addr@. 'Left' when the words
-- there don't decode as a record at all, which can only mean the control
-- stack has been corrupted -- reported rather than guessed at.
readScopeAt :: forall w. (IsWord w) => Int -> State (Wasm32St w) (Either Text Scope)
readScopeAt addr = do
    let step = byteSizeT @w
    words_ <- mapM (\i -> getWord (addr + i * step)) [0 .. scopeRecordWords - 1]
    return $ first (\err -> "control stack corrupted at " <> hexAddr 2 addr <> ": " <> err) (decodeScope @w words_)

-- | Read up to @n@ records from the innermost outward, each with its own
-- address -- fewer if the stack is shallower, and stopping at the first
-- record that fails to decode.
readScope :: forall w. (IsWord w) => Int -> State (Wasm32St w) (Either Text [(Int, Scope)])
readScope n = do
    st <- get
    go (take n (scopeAddrs st))
    where
        go [] = return $ Right []
        go (addr : rest) =
            readScopeAt addr >>= \case
                Left err -> return $ Left err
                Right scope -> fmap ((addr, scope) :) <$> go rest

-- | Read the innermost open record. 'Nothing' when nothing is open,
-- which some callers treat as "nothing to do" rather than an error.
readTopScope :: forall w. (IsWord w) => State (Wasm32St w) (Either Text (Maybe (Int, Scope)))
readTopScope = fmap listToMaybe <$> readScope 1

-- | Push a scope at the current frontier and advance it. Never touches
-- @sp@ -- the two stacks grow apart from 'ctrlBase'.
pushScope :: forall w. (IsWord w) => Scope -> State (Wasm32St w) ()
pushScope scope = do
    Wasm32St{ctrlSp} <- get
    let step = byteSizeT @w
        ctrlSp' = ctrlSp + scopeRecordBytes @w
    checkControlRoom ctrlSp' $ case encodeScope @w scope of
        Left err -> raiseInternalError $ "control record doesn't fit: " <> err
        Right words_ -> do
            forM_ (zip [0 ..] words_) $ \(i, word) -> setWord (ctrlSp + i * step) word
            modify $ \st -> st{ctrlSp = ctrlSp'}

-- | Pop the innermost record: a bump allocator's "pop" is nothing but
-- moving the frontier back, so this never touches memory.
popScope :: forall w. (IsWord w) => State (Wasm32St w) ()
popScope = modify $ \st -> st{ctrlSp = ctrlSp st - scopeRecordBytes @w}

-- | Find the control record @depth@ levels out from the innermost open
-- one (0 = innermost). Errors at a 'CallScope' rather than reaching
-- through it: a @br@ is scoped to its own function's nesting, and
-- leaving a function is @return@'s job. The two failures get different
-- messages -- a reader told only "out of range" tries a different depth
-- when the fix is a @return@.
findBranchTarget :: forall w. (IsWord w) => Int -> State (Wasm32St w) (Either Text Int)
findBranchTarget depth = do
    records <- readScope (depth + 1)
    return $ do
        records' <- records
        case drop depth records' of
            _
                | any (isCallScope . snd) records' -> Left "br/br_if: depth reaches past the enclosing function; use return to leave it"
            (target, _) : _ -> Right target
            [] -> Left $ "br/br_if: depth " <> show depth <> " is out of range, only " <> show (length records') <> " scope(s) open"

isCallScope :: Scope -> Bool
isCallScope CallScope{} = True
isCallScope _ = False

-- | Find the nearest enclosing 'CallScope' -- what @return@ closes.
-- Unlike 'findBranchTarget', this reaches *through* any @block@\/@loop@
-- in the way: a @return@ inside a loop must still close it.
findCall :: forall w. (IsWord w) => State (Wasm32St w) (Either Text Int)
findCall = do
    st <- get
    go (scopeAddrs st)
    where
        go [] = return $ Left "return without active function frame"
        go (addr : outer) =
            readScopeAt addr >>= \case
                Left err -> return $ Left err
                Right CallScope{} -> return $ Right addr
                Right _ -> go outer

-- | Close the innermost open record (always a LIFO pop). If @keepOpen@
-- (a taken branch back into a loop), just jump to its start. A
-- 'CallScope' is never @keepOpen@; everything else retreats @ctrlSp@
-- past it and jumps past its @end@.
closeScope :: forall w. (IsWord w) => Bool -> State (Wasm32St w) ()
closeScope keepOpen =
    readTopScope >>= \case
        Left err -> raiseInternalError err
        Right Nothing -> raiseInternalError "closeScope: no open control record"
        Right (Just (_, scope)) -> case scope of
            LoopScope{csStart} | keepOpen -> setPc csStart
            LoopScope{csEnd} -> popScope @w >> setPc (csEnd + byteSize End)
            BlockScope{csEnd} -> popScope @w >> setPc (csEnd + byteSize End)
            CallScope{csCallerFrameBase, csCallerLocalCount, csReturnPc, csResultCount} -> do
                results <- popValues csResultCount
                Wasm32St{frameBase} <- get
                modify $ \st -> st{sp = frameBase + byteSizeT @w}
                mapM_ pushValue results
                popScope @w
                modify $ \st -> st{frameBase = csCallerFrameBase, localCount = csCallerLocalCount}
                setPc csReturnPc

popValues :: forall w. (IsWord w) => Int -> State (Wasm32St w) [w]
popValues n = reverse <$> replicateM n popValue

-- | Close every open record from the innermost down to and including
-- @r@, keeping @r@ open only if @keepOpen@ (branching back into a loop).
-- Anything nested inside @r@ is closed whatever its kind.
unwindTo :: forall w. (IsWord w) => Int -> Bool -> State (Wasm32St w) ()
unwindTo r keepOpen = do
    st <- get
    if ctrlTopOf @w st == r
        then closeScope keepOpen
        else do
            closeScope False
            unwindTo r keepOpen

-- | Branch to the @block@\/@loop@ @depth@ levels out (0 = innermost): a
-- @loop@ stays open (continue), a @block@ closes along with everything
-- nested inside it (break). 'findBranchTarget' already guarantees the
-- target is never a 'CallScope'.
branchTo :: forall w. (IsWord w) => Int -> State (Wasm32St w) ()
branchTo depth = do
    result <- findBranchTarget depth
    case result of
        Left err -> raiseInternalError err
        Right target ->
            readScopeAt target >>= \case
                Left err -> raiseInternalError err
                Right LoopScope{} -> unwindTo target True
                Right _ -> unwindTo target False

instance (IsWord w) => Inspectable (Wasm32St w) where
    type WordOf (Wasm32St w) = w
    type IsaOf (Wasm32St w) = Wasm32Isa w w

    programCounter Wasm32St{pc} = pc
    memoryDump Wasm32St{mem} = mem
    ioStreams Wasm32St{mem = IoMem{mIoStreams}} = mIoStreams
    isHalted Wasm32St{stopped} = stopped
    reprState labels st v
        | Just v' <- defaultView labels st v = v'
    reprState labels st@Wasm32St{mem, memAccessCount} v =
        case T.splitOn ":" v of
            ["stack", f] -> formatValues f (stackWords view)
            ["locals", f] -> formatValues f (localWords view)
            ["layout", f] -> withShow f (renderLayout labels view) Nothing
            ["layout", f, n] -> withFrameLimit n (withShow f (renderLayout labels view))
            ["dump", f] -> withShow f (renderDump labels view) Nothing
            ["dump", f, n] -> withFrameLimit n (withShow f (renderDump labels view))
            ["memAccesses", "dec"] -> show memAccessCount
            ["memAccesses", f] -> unknownFormat f
            [r] -> reprState labels st (r <> ":dec")
            [r, _] -> unknownView r
            _ -> errorView v
        where
            view = stackView st

            formatValues "dec" vs = toText $ intercalate ":" $ map show vs
            formatValues "hex" vs = T.intercalate ":" $ map (toText . word32ToHex) vs
            formatValues f _ = unknownFormat f

            -- Both formats already exist for the word *values* a span
            -- shows; extend the same choice to every address it mentions
            -- rather than leaving addresses permanently decimal.
            -- 'word32ToHex's full 8 digits stays for word values, which
            -- are arbitrary 32-bit data however small the memory is;
            -- addresses get only the digits this memory could need.
            withShow "dec" render = render show show
            withShow "hex" render = render (toText . word32ToHex) (hexAddr (hexAddrWidth (memCapacity mem)))
            withShow f _ = const (unknownFormat f)

            -- \| Parse @layout:hex:N@\/@dump:hex:N@'s trailing @N@ -- how
            -- many of the most recent frames (the live one, then its
            -- nearest callers) to show in full; @N = 1@ means the live
            -- frame only.
            withFrameLimit :: Text -> (Maybe Int -> Text) -> Text
            withFrameLimit n k = maybe (unknownFormat n) (k . Just) (readMaybe (toString n))

-- | Hand the report views what they need: the memory, the pointers
-- bounding the live stacks, and where the open control records are.
stackView :: forall w. (IsWord w) => Wasm32St w -> StackView (Wasm32Isa w w) w
stackView st@Wasm32St{mem, pc, sp, ctrlSp, ctrlBase, frameBase, localCount} =
    StackView
        { svMem = mem
        , svPc = pc
        , svSp = sp
        , svFrameBase = frameBase
        , svLocalCount = localCount
        , svCtrlSp = ctrlSp
        , svCtrlBase = ctrlBase
        , svDataTop = operandFloor mem
        , svScopeAddrs = scopeAddrs st
        }

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
            I32Load offset -> load offset getWord id
            I32Load8U offset -> load offset getByte fromIntegral
            I32Load8S offset -> load offset getByte (fromIntegral . (fromIntegral :: Word8 -> Int8))
            I32Store offset -> store offset setWord id
            I32Store8 offset -> store offset setByte fromIntegral
            If skip -> do
                condition <- popValue
                if condition /= 0
                    then nextPc instruction
                    else skipForward skip
            Else skip -> skipForward skip
            End -> do
                -- A plain @end@ closes a @block@\/@loop@ only when it is
                -- that scope's own matching @end@; an @if@'s @end@ finds
                -- some enclosing scope on top instead, whose own @end@
                -- is elsewhere. It never closes a call -- @return@ does.
                top <- readTopScope
                case top of
                    Left err -> raiseInternalError err
                    Right (Just (_, LoopScope{csEnd})) | csEnd == pc -> popScope @w
                    Right (Just (_, BlockScope{csEnd})) | csEnd == pc -> popScope @w
                    Right _ -> return ()
                nextPc instruction
            Loop toEnd ->
                withScopeEnd toEnd $ \endPc -> do
                    pushScope LoopScope{csStart = pc + byteSize instruction, csEnd = endPc}
                    nextPc instruction
            Block toEnd ->
                withScopeEnd toEnd $ \endPc -> do
                    pushScope BlockScope{csEnd = endPc}
                    nextPc instruction
            Br depth -> branchTo depth
            BrIf depth -> do
                condition <- popValue
                if condition == 0
                    then nextPc instruction
                    else branchTo depth
            Dup -> do
                top <- popValue
                pushValue top
                pushValue top
                nextPc instruction
            Drop -> popValue @w >> nextPc instruction
            Select -> do
                condition <- popValue
                onFalse <- popValue
                onTrue <- popValue
                pushValue (if condition /= 0 then onTrue else onFalse)
                nextPc instruction
            Unreachable -> raiseInternalError "unreachable"
            LocalGet i -> do
                addr <- localAddr i
                getWord addr >>= pushValue
                nextPc instruction
            LocalSet i -> do
                addr <- localAddr i
                popValue >>= setWord addr
                nextPc instruction
            LocalTee i -> do
                addr <- localAddr i
                value <- popValue
                setWord addr value
                pushValue value
                nextPc instruction
            LocalsReserve n -> do
                Wasm32St{sp} <- get
                let step = byteSizeT @w
                    sp' = sp - n * step
                checkOperandRoom sp' $ do
                    -- Zeroed, like real WebAssembly's locals: memory
                    -- starts out randomised by default, so an unwritten
                    -- slot would otherwise read as noise, not 0.
                    forM_ [sp - step, sp - 2 * step .. sp'] $ \addr -> setWord addr 0
                    modify $ \st -> st{sp = sp', localCount = localCount st + n}
                nextPc instruction
            Call paramCount resultCount -> do
                target <- popValue
                Wasm32St{sp, frameBase, localCount} <- get
                let calleeFrameBase = sp + (paramCount - 1) * byteSizeT @w
                pushScope
                    CallScope
                        { csCallerFrameBase = frameBase
                        , csCallerLocalCount = localCount
                        , csReturnPc = pc + byteSize instruction
                        , csResultCount = resultCount
                        }
                modify $ \st -> st{frameBase = calleeFrameBase, localCount = paramCount}
                setPc (fromEnum target)
            Return -> do
                result <- findCall
                case result of
                    Left err -> raiseInternalError err
                    Right r -> unwindTo r False
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
            -- \| Resolve a scope instruction's own forward distance, which
            -- 'resolveScopeTargets' already computed at translate time.
            -- Only an instruction that never went through it can be
            -- unresolved, so this is a sanity check, not a code path a
            -- program can reach.
            withScopeEnd d k
                | d < 0 = raiseInternalError "control flow error: unresolved scope target"
                | otherwise = k (pc + d)
            skipForward d = withScopeEnd d setPc
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
                if y == 0
                    then raiseInternalError "integer divide by zero"
                    else
                        if x == minBound && y == -1
                            then raiseInternalError "integer overflow"
                            else pushValue (x `quot` y)
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

-- $instructionIndex
--
-- Each constructor below documents one instruction: its assembly syntax,
-- what it does, and its effect written as pseudo-operations on the operand
-- stack. Every comparison pushes @1@ for true and @0@ for false.
--
-- For a two-operand instruction the /second/ operand pushed ends up on top
-- and is popped first, so @i32.const 10@, @i32.const 3@, @i32.sub@ computes
-- @10 - 3 = 7@. That holds for every non-commutative instruction:
-- subtraction, division, remainder, shifts, and all comparisons read their
-- operands as (first-pushed, second-pushed).
--
-- There is no dedicated zero test and no inequality test. @i32.const 0@,
-- @i32.eq@ serves as both: against a value it asks \"is this zero\", and
-- against a comparison's own @0@\/@1@ result it negates it. That is how the
-- comparisons this ISA does not have are written --
--
-- > i32.gt_s                  ; a <= b  ==  not (a > b)
-- > i32.const 0
-- > i32.eq
--
-- and the same idiom turns @br_if@, which branches on non-zero, into a
-- branch-if-zero.

-- $programStructure
--
-- A program uses the normal @.data@ and @.text@ sections, and execution
-- starts at the @_start@ label. A function is nothing more than an ordinary
-- label followed by instructions: no directive declares one, no header
-- instruction sits at its entry and no footer at its exit. A function ends
-- wherever its last @return@ -- or its last instruction, falling through --
-- happens to be.
--
-- > .text
-- >
-- > _start:
-- >     i32.const 5
-- >     i32.const factorial
-- >     call 1, 1
-- >     halt
-- >
-- > factorial:
-- >     local.get 0
-- >     i32.const 1
-- >     i32.le_s
-- >     if
-- >         i32.const 1
-- >         return
-- >     end
-- >     local.get 0
-- >     local.get 0
-- >     i32.const 1
-- >     i32.sub
-- >     i32.const factorial
-- >     call 1, 1
-- >     i32.mul
-- >     return

-- $stacks
--
-- Memory is split in half: the lower half holds code and @.data@, the upper
-- half is where the two stacks live. Both grow from a single __root__, in
-- opposite directions.
--
-- > 0                     memTop                  root            memorySize
-- > |-- .text, .data ------ | -------------------- R ----------------- |
-- >                         |    <- operand stack  |  control stack -> |
-- >                         ^ wall          sp <-- R --> ctrlSp        ^ wall
--
-- [operand stack]: Holds a function's locals and its values. It descends
-- from the root, the classic hardware convention: @sp@ is the address of
-- the most recently pushed word, decremented /before/ a push writes and
-- incremented /after/ a pop reads. Its wall is 'operandFloor' -- 'memTop',
-- raised past any memory-mapped I\/O port that would otherwise sit inside
-- the stack region.
--
-- [control stack]: Holds one fixed-size record per open @block@, @loop@ or
-- call -- see "Wrench.Isa.Wasm32.ControlRecord". It ascends from the root.
-- @ctrlSp@ is its frontier, one past the innermost live record. Its wall is
-- the end of memory.
--
-- Because they grow /apart/, neither can ever reach the other: an
-- arithmetic instruction cannot pop into a live control record, and pushing
-- a scope cannot scribble over a local. What each /can/ reach is its own
-- wall, and the machine checks every push against it:
--
-- > operand stack overflow: push to 0xfc would reach below the stack region at 0x100
-- > control stack overflow: scope record ending at 0x208 would reach past the end of memory at 0x200
--
-- Two messages rather than one, because the fixes differ: too much on the
-- operand stack, or too deep a nesting of scopes and calls. This is the one
-- place the ISA stops a malformed program instead of letting it corrupt
-- itself.
--
-- The root defaults to three quarters of the way up the stack region (see
-- 'stackRoot'), since a program pushes far more operands than it opens
-- scopes and a record is eight bytes. 'SpInit' moves it, which is how a
-- program that needs the other balance asks for it: a deeply recursive
-- function wants the root low, a value-heavy loop wants it high. The new
-- root has to stay inside the stack region, but within it nothing checks
-- that the split leaves either stack enough room.
--
-- A memory-mapped I\/O port is ordinary memory to every instruction, so a
-- port mapped inside the stack region would be writable by a deep enough
-- operand stack -- a push doing I\/O. 'operandFloor' raises the wall past
-- any such port, turning that into the overflow it should have been.
-- Keeping ports in the code+data half avoids the question entirely, which
-- is what every example does.
--
-- Code and @.data@ spilling past 'memTop' is /not/ checked either, so a
-- program whose code and data exceed half the configured memory will have
-- the operand stack descend into its own globals.
--
-- @if@ is the exception among scopes: it pushes nothing. Nothing branches
-- out of a taken branch early, so there is nothing to remember.

-- $locals
--
-- A function's locals start with its parameters. They occupy a contiguous
-- run of words from @frameBase@, one per index, addressed directly as
-- @frameBase - index * 4@ -- the operand stack descends, so local @0@, the
-- first argument pushed, ends up at the /highest/ address and each later
-- one lower. Whatever values the caller pushed just before @call@ are, from
-- the callee's perspective, its locals @0@ through @paramCount - 1@.
-- @frameBase@ is not fixed: it moves to wherever the current function's
-- locals start, and is saved and restored across calls.
--
-- 'LocalsReserve' claims @n@ more, zeroed, at the indices directly after
-- the parameters. Since the operand stack descends, the words just below
-- the current locals are exactly where the next ones belong, so this is a
-- stack-pointer decrement and nothing more -- @call@ never needed to know
-- how wide a callee's frame is, and @return@ already discards everything
-- below @frameBase@ in one step.
--
-- > fact:
-- >     locals.reserve 1       ; local 1: scratch, this activation only
-- >     local.get 0
-- >     local.set 1
--
-- The point is that a reserved local is private to the activation. A
-- recursive function that keeps scratch in a @.data@ cell has one cell
-- shared by every level, so the nested call overwrites what the caller was
-- holding -- and the result is silently wrong, not a trap. @_start@ can
-- reserve locals too, which matters because @_start@ is never called and so
-- has no parameters at all.
--
-- Being a stack-pointer decrement and nothing more, @locals.reserve@ is not
-- checked against the two ways to misuse it, and both fail silently:
--
-- * __Reserving after pushing operands__ claims the words /below/ them, so
--   values already on the stack get renumbered as locals. @i32.const 42@,
--   @locals.reserve 1@, @local.get 0@ pushes @42@ -- the constant became
--   local @0@, and the zeroed word it reserved became an operand. Reserve
--   before pushing anything.
-- * __Reserving inside a loop body__ reserves again on every iteration, so
--   the frame grows until the operand stack hits its wall. Reserve once, at
--   the function's entry.
--
-- A local index is one byte, and nothing checks it against the active
-- function's actual local count: @local.get 7@ in a one-parameter function
-- that reserved nothing reads whatever word happens to sit there.

-- $operands
--
-- Every arithmetic, comparison and memory instruction reads its operands
-- from the top of the operand stack and pushes its result back; see the
-- instruction index above for the operand order that implies.
--
-- 'Dup' duplicates whatever is on top and 'Drop' discards it, neither
-- needing a local. A value pushed before a @block@ or @loop@ is opened
-- stays reachable inside it, because scope records are on their own stack
-- and so nothing gets in the way of reaching down the operand stack. A
-- counter can live on the operand stack across loop iterations the same way
-- it can live in a local or a @.data@ cell.
--
-- Nothing checks for operand-stack /underflow/. Popping more than the
-- active frame pushed reads into its locals, and past those into the
-- caller's frame -- whatever is physically there.

-- $controlFlow
--
-- @block@, @loop@ and @if@ each open a scope; @end@ closes the innermost
-- one still open. All three are written bare in source: the assembler finds
-- each one's matching @end@ and writes the byte distance into the
-- instruction (see 'resolveScopeTargets'), so nothing is scanned for at run
-- time and an unbalanced @block@, @else@ or @end@ is a translation error
-- rather than something discovered mid-execution.
--
-- 'Br' and 'BrIf' name a scope not by label but by /depth/: how many
-- enclosing scopes out to reach, counting the innermost currently-open one
-- as @0@. An @if@ does not count as a level -- only @block@ and @loop@ do.
--
-- > block                  ; depth 1 from inside the loop below
-- >     loop                ; depth 0 from inside its own body
-- >         local.get 0
-- >         i32.const 0
-- >         i32.eq
-- >         br_if 1          ; exit the block: "break"
-- >         local.get 0
-- >         i32.const 1
-- >         i32.sub
-- >         local.set 0
-- >         br 0             ; jump back to the loop's own start: "continue"
-- >     end
-- > end
--
-- Reaching a @loop@ by depth jumps back to just after the @loop@
-- instruction and leaves the scope open -- this is how a loop continues.
-- Reaching a @block@ jumps forward to just after its matching @end@ and
-- closes it, along with every scope nested between the branch and it --
-- this is how a program breaks out of one or more levels at once.
--
-- A branch to depth @n@ steps out @n@ records, closes every record strictly
-- between the branch and the target whatever its kind, and finally either
-- re-enters the target (a loop) or closes it too (a block). Closing a
-- record is nothing but retreating @ctrlSp@; the operand stack is
-- untouched, so anything a scope's body pushed and never popped survives
-- the scope closing.
--
-- A branch's depth can never reach across a function call boundary:
-- stepping outward stops with an error the moment it would pass through a
-- call's own record. A structured branch is scoped to the function it
-- appears in; leaving a function is @return@'s job, not a branch's, and the
-- diagnostic says so.

-- $functions
--
-- @i32.const some_function@ produces a function's address exactly the way
-- it produces any other label's address -- a function is just data once you
-- have its address. 'Call' always pops its target off the top of the stack;
-- there is no separate instruction for a statically-known target. A call
-- whose target is known when the program is written is simply @i32.const
-- target@ immediately before the @call@, and because the target is /just/ a
-- value a program can equally compute it -- read it out of a local that was
-- set to a function's address earlier -- and call through that instead.
-- Both are the same instruction; only where the value came from differs.
--
-- @call@ takes two more operands written directly after it: how many values
-- pushed just before the target are this call's arguments, and how many
-- values the call is expected to leave behind. Neither is looked up
-- anywhere, and nothing checks that a callee's actual behaviour matches what
-- a particular call site declared. Getting it wrong is not caught; it
-- corrupts the stack silently, the same way any other malformed program
-- does.
--
-- At the moment @call@ executes, the values pushed immediately before the
-- popped target -- as many as its declared argument count -- become the
-- callee's locals @0@ downward, without being copied anywhere: @call@
-- records where they already are as the new @frameBase@ and jumps to the
-- target. A record describing the call goes on the control stack.
--
-- 'Return' finds the nearest enclosing call, closing any @block@ or @loop@
-- still open along the way, pops exactly as many values as that call
-- declared it would return, discards the callee's entire frame -- its
-- locals and its own record -- in one step, and pushes the results back for
-- the caller at exactly the depth they would be at had the call consumed
-- its arguments and produced its results in place. Falling off the end of a
-- function's instructions without an explicit @return@ is only correct if
-- nothing is left open.

-- $differences
--
-- This ISA borrows WebAssembly's shape, not its semantics, and the gaps are
-- deliberate. The ones worth knowing:
--
-- * __Blocks have no result arity.__ Real WebAssembly gives every
--   @block@\/@loop@\/@if@ a block type, and a branch carries exactly that
--   many values out, dropping the rest. Here a scope has no type at all:
--   whatever the body pushed and did not pop simply stays on the operand
--   stack. That makes a branch cheap and the model smaller, but a branch
--   out of a half-finished computation leaves its partial results behind.
-- * __@if@ is not a branch target.__ In real WebAssembly an @if@ is a label
--   like any other block, so @br 0@ inside one exits the @if@. Here only
--   @block@ and @loop@ count toward a depth.
-- * __A call's arity lives at the call site.__ Real WebAssembly takes both
--   counts from the callee's declared type and validates every call against
--   it. Here the caller writes both numbers itself and nothing checks them.
-- * __Locals are reserved by an instruction, not declared in a header.__
--   Real WebAssembly encodes a function's extra locals in a declaration
--   vector ahead of its body. Here 'LocalsReserve' does the same job at run
--   time, as the function's first instruction -- same zeroing, same
--   per-activation privacy, but nothing validates that a function reserves
--   before it indexes.
-- * __The target of a call is an address, not an index.__ Real WebAssembly
--   has @call@ by function index and @call_indirect@ through a table. Here a
--   function address is an ordinary value, so one instruction covers both.
-- * __A lot is simply absent, on purpose.__ There is no
--   @i32.clz@\/@i32.ctz@\/@i32.popcnt@, no @i32.rotl@\/@i32.rotr@, no
--   @i32.div_u@\/@i32.rem_u@, no 16-bit loads or stores, no @br_table@, no
--   @nop@, and neither @i32.eqz@ nor @i32.ne@. The bit-counting and rotate
--   instructions are left out precisely /because/ counting leading zeros,
--   counting set bits, computing parity and rotating a word are exercises
--   in this course -- an instruction that is the whole answer to an
--   assignment teaches nothing. The rest are out because no other ISA here
--   has them: a capability absent from risc-iv and m68k is one the course
--   has already decided its students don't need. Rotates are @i32.shl@,
--   @i32.shr_u@ and @i32.or@; a switch is a chain of @i32.eq@ and @br_if@;
--   a halfword is two byte accesses.
-- * __Operand order and arithmetic follow the spec.__ Where this ISA does
--   implement something WebAssembly has, it matches: @i32.store@ takes its
--   value above its address, shift amounts mask to 5 bits, @i32.div_s@
--   truncates toward zero and traps on @INT_MIN \/ -1@, and @i32.rem_s@
--   traps only on a zero divisor.
