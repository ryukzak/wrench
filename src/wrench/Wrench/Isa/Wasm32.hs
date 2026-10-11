{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
A 32-bit stack-based instruction set for teaching, inspired by
<https://webassembly.github.io/spec/core/ WebAssembly>. It features:

* Structured control flow (@block@, @loop@, @if@\/@else@) instead of
  arbitrary jumps, with every scope's target worked out once when the
  program loads -- never by scanning for a matching @end@ as it runs.
* Two stacks, both in ordinary memory: an /operand stack/ holding locals
  and values, and a /control stack/ holding the bookkeeping for open
  @block@\/@loop@\/call scopes -- no general-purpose registers.
* Function calls where a function address is just an ordinary value, so a
  direct call and a call through a value computed at run time are the same
  instruction.

Which makes it useful for studying function calls, local variables, loops
and low-level memory access in a model that fits in one sitting.

Comments in Wasm32 assembly are introduced by @;@.
-}
module Wrench.Isa.Wasm32 (
    -- * Instructions
    -- $instructionIndex
    Wasm32Isa (..),

    -- ** Instruction encoding
    -- $encoding

    -- * Machine state
    Wasm32St (ctrlBase),

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
import Data.IntSet qualified as IntSet
import Data.Text qualified as T
import Relude
import Relude.Extra (insert, toPairs, (!?))
import Text.Megaparsec (choice, getSourcePos, lookAhead, try)
import Text.Megaparsec.Char (char, digitChar, hspace, hspace1, string)
import Wrench.Isa.Wasm32.ControlRecord
import Wrench.Isa.Wasm32.Layout
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Machine.Word (u8)
import Wrench.Report
import Wrench.Translator.Parser.Misc
import Wrench.Translator.Parser.Types
import Wrench.Translator.Types

-- | One instruction. @w@ is the machine word, @l@ the immediate a label
-- resolves to -- a 'Ref' before the translator fills it in, a word after.
data Wasm32Isa w l
    = -- | @i32.const \<value\>@: push an immediate onto the operand stack.
      -- A label reference is an ordinary immediate, which is how a function
      -- address is produced (see the \"Functions\" section).
      --
      -- > stack.push(value)
      I32Const l
    | -- | @i32.add@: add.
      --
      -- > y <- pop
      -- > x <- pop
      -- > push (x + y)
      I32Add
    | -- | @i32.sub@: subtract the second-pushed from the first-pushed.
      --
      -- > y <- pop
      -- > x <- pop
      -- > push (x - y)
      I32Sub
    | -- | @i32.mul@: multiply, keeping the low 32 bits of the product.
      --
      -- > y <- pop
      -- > x <- pop
      -- > push (x * y)
      I32Mul
    | -- | @i32.div_s@: signed division, truncating toward zero, so
      -- @-7 \/ 2@ is @-3@ and not @-4@. Traps on a zero divisor and on the
      -- one unrepresentable quotient, @minBound \/ -1@.
      --
      -- > y <- pop
      -- > x <- pop
      -- > push (trunc (x / y))
      I32DivS
    | -- | @i32.rem_s@: signed remainder, whose sign follows the dividend,
      -- so @-7 % 2@ is @-1@. Only a zero divisor traps: unlike 'I32DivS',
      -- @minBound % -1@ is defined, and is @0@.
      --
      -- > y <- pop
      -- > x <- pop
      -- > push (x - y * trunc (x / y))
      I32RemS
    | -- | @i32.and@: bitwise and.
      --
      -- > y <- pop
      -- > x <- pop
      -- > push (x .&. y)
      I32And
    | -- | @i32.or@: bitwise or.
      --
      -- > y <- pop
      -- > x <- pop
      -- > push (x .|. y)
      I32Or
    | -- | @i32.xor@: bitwise exclusive or.
      --
      -- > y <- pop
      -- > x <- pop
      -- > push (xor x y)
      I32Xor
    | -- | @i32.shl@: shift left, masking the shift amount to its low 5
      -- bits, so a shift of 33 is a shift of 1.
      --
      -- > y <- pop
      -- > x <- pop
      -- > push (x << (y .&. 0x1F))
      I32Shl
    | -- | @i32.shr_s@: arithmetic (sign-extending) shift right, same
      -- 5-bit masking.
      --
      -- > y <- pop
      -- > x <- pop
      -- > push (x >> (y .&. 0x1F))
      I32ShrS
    | -- | @i32.shr_u@: logical (zero-filling) shift right, same 5-bit
      -- masking.
      --
      -- > y <- pop
      -- > x <- pop
      -- > push (unsigned x >> (y .&. 0x1F))
      I32ShrU
    | -- | @i32.eq@: push @1@ if the two values are equal, else @0@. With
      -- @i32.const 0@ in front of it this is also the ISA's zero test and
      -- its only negation (see the instruction index above).
      --
      -- > y <- pop
      -- > x <- pop
      -- > push (if x == y then 1 else 0)
      I32Eq
    | -- | @i32.lt_s@: signed @\<@.
      I32LtS
    | -- | @i32.le_s@: signed @\<=@.
      I32LeS
    | -- | @i32.gt_s@: signed @\>@.
      I32GtS
    | -- | @i32.ge_s@: signed @\>=@.
      I32GeS
    | -- | @i32.lt_u@: unsigned @\<@, reading both operands as unsigned,
      -- so @-1@ compares /greater/ than @1@.
      I32LtU
    | -- | @i32.le_u@: unsigned @\<=@.
      I32LeU
    | -- | @i32.gt_u@: unsigned @\>@.
      I32GtU
    | -- | @i32.ge_u@: unsigned @\>=@.
      I32GeU
    | -- | @i32.load [\<offset\>]@: pop an address, push the 4-byte word at
      -- address plus offset.
      --
      -- Every memory instruction carries a static offset, added to the
      -- popped address: the base is computed at run time and pushed, the
      -- displacement into it is written into the instruction, which makes
      -- indexing a struct field or a small array one instruction rather
      -- than three. The offset is unsigned and one byte
      -- ('immediateBytes'), so @0@-@255@; omit it when it is zero, and
      -- reach further by computing the address with @i32.add@.
      --
      -- > i32.const point       ; base address
      -- > i32.load 4            ; point.y, no i32.add needed
      --
      -- > addr <- pop
      -- > push mem[addr + offset]
      I32Load l
    | -- | @i32.store [\<offset\>]@: pop a value, then an address, and write
      -- the value's 4 bytes at address plus offset. The value sits /above/
      -- the address, matching real WebAssembly, so the destination has to
      -- be pushed before the value is computed.
      --
      -- > value <- pop
      -- > addr <- pop
      -- > mem[addr + offset] <- value
      I32Store l
    | -- | @i32.load8_u [\<offset\>]@: like 'I32Load', but reads one byte
      -- and zero-extends it.
      I32Load8U l
    | -- | @i32.load8_s [\<offset\>]@: like 'I32Load8U', but sign-extends.
      I32Load8S l
    | -- | @i32.store8 [\<offset\>]@: like 'I32Store', but writes only the
      -- value's low byte.
      I32Store8 l
    | -- | @if@: pop a condition; fall into the body that follows if it is
      -- non-zero, otherwise skip to the matching @else@ (if there is one) or
      -- past the matching @end@.
      --
      -- Where that skip lands is not encoded in the instruction: like every
      -- scope instruction here it is a bare opcode, and 'resolveScopeTargets'
      -- works the targets out once, when the machine loads the program. @if@
      -- pushes no control record: nothing branches out of a taken branch
      -- early, so there is nothing to remember.
      If
    | -- | @else@: marks the alternative body, reached only by a taken
      -- @if@-body falling through to it, at which point it skips
      -- unconditionally past the matching @end@. So the @else@-body never
      -- runs after the @if@-body already did.
      Else
    | -- | @end@: closes the innermost open @block@ or @loop@. An @if@\'s
      -- @end@ closes nothing, since @if@ opened nothing -- which of the two
      -- a given @end@ is follows from the nesting, so loading the program
      -- settles it ('spEndPops') rather than a record being read at run
      -- time to find out.
      End
    | -- | @loop@: open a scope a branch can jump /backward/ into, landing
      -- just after this instruction. On its own it runs its body once, like
      -- @block@.
      Loop
    | -- | @block@: open a scope a branch can jump /forward/ past, landing
      -- just after its matching @end@. This is what makes a \"break\"
      -- possible, which @loop@ alone does not.
      Block
    | -- | @br \<depth\>@: branch unconditionally to the enclosing scope
      -- @depth@ levels out, @0@ being the innermost. See the \"Control
      -- flow\" section for what reaching a @loop@ versus a @block@ does.
      --
      -- The depth is what the source says and what a dump shows; where it
      -- leads, and how many control records go with it, are worked out
      -- when the program loads ('resolveBranches'). A depth with no such
      -- scope around it is rejected then.
      Br l
    | -- | @br_if \<depth\>@: pop a condition and branch like 'Br' only
      -- if it is non-zero, otherwise fall through. Branching on /zero/ is
      -- @i32.const 0@, @i32.eq@, @br_if@.
      BrIf l
    | -- | @drop@: discard the top of the operand stack -- the only way to
      -- get rid of a value the program does not want, a callee result the
      -- caller ignores being the usual one. Nothing else can do it:
      -- @local.set@ needs a spare local to land in, and @i32.store@ needs
      -- its destination pushed /under/ the value.
      --
      -- > pop
      Drop
    | -- | @select@: pop a condition, then two values; push the
      -- first-pushed of the two if the condition is non-zero, the second
      -- otherwise. A branchless two-way choice.
      --
      -- > c <- pop
      -- > y <- pop
      -- > x <- pop
      -- > push (if c /= 0 then x else y)
      Select
    | -- | @unreachable@: trap. Marks a point the program believes it can
      -- never reach, and says so if it does.
      Unreachable
    | -- | @local.get \<i\>@: push local @i@\'s value.
      --
      -- > push locals[i]
      LocalGet l
    | -- | @local.set \<i\>@: pop the top of the stack into local @i@.
      --
      -- > locals[i] <- pop
      LocalSet l
    | -- | @local.tee \<i\>@: like @local.set@, but pushes the value back,
      -- leaving the stack depth unchanged.
      --
      -- > x <- pop
      -- > locals[i] <- x
      -- > push x
      LocalTee l
    | -- | @locals.reserve \<n\>@: claim @n@ more locals, zeroed, at the
      -- indices directly after the ones the frame already has. Belongs at a
      -- function's entry, before anything is pushed and outside any loop --
      -- see the \"Locals\" section for what happens otherwise. @n@ is one
      -- byte, so @0@-@255@, and reserving past the operand stack's wall is
      -- an @operand stack overflow@. @return@ reclaims them along with the
      -- parameters.
      --
      -- > sp <- sp - n*4
      -- > mem[sp .. sp + n*4 - 4] <- 0
      -- > localCount <- localCount + n
      LocalsReserve l
    | -- | @call \<paramCount\>, \<resultCount\>@: pop a target address, an
      -- ordinary value, so a known target and one computed at run time are
      -- the same instruction. The @paramCount@ values pushed just before it
      -- become the callee's locals; @resultCount@ values are expected back.
      -- Neither count is checked against the callee. See the \"Functions\"
      -- section.
      Call {callParamCount :: l, callResultCount :: l}
    | -- | @return@: return from the nearest enclosing call, closing any
      -- @block@ or @loop@ still open along the way and forwarding the
      -- declared results to the caller.
      Return
    | -- | @sp.init@: pop an address and make it the root both stacks grow
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
      -- > addr <- pop
      -- > sp <- addr
      -- > ctrlSp <- addr
      -- > frameBase <- addr - 4
      SpInit
    | -- | @halt@: stop execution.
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
                    , memOp "i32.load8_u" I32Load8U
                    , memOp "i32.load8_s" I32Load8S
                    , memOp "i32.load" I32Load
                    , memOp "i32.store8" I32Store8
                    , memOp "i32.store" I32Store
                    , cmd0 "if" If
                    , cmd0 "else" Else
                    , cmd0 "end" End
                    , cmd0 "loop" Loop
                    , cmd0 "block" Block
                    , Br <$> cmd1 "br" constantRef
                    , BrIf <$> cmd1 "br_if" constantRef
                    , cmd0 "drop" Drop
                    , cmd0 "select" Select
                    , cmd0 "unreachable" Unreachable
                    , LocalGet <$> cmd1 "local.get" constantRef
                    , LocalSet <$> cmd1 "local.set" constantRef
                    , LocalTee <$> cmd1 "local.tee" constantRef
                    , LocalsReserve <$> cmd1 "locals.reserve" constantRef
                    , do
                        void $ cmd1 "call" (pure ())
                        callParamCount <- constantRef
                        comma
                        callResultCount <- constantRef
                        return Call{callParamCount, callResultCount}
                    , cmd0 "return" Return
                    , cmd0 "sp.init" SpInit
                    , cmd0 "halt" Halt
                    ]

cmd0 :: String -> a -> Parser a
cmd0 mnemonic constructor = string mnemonic >> return constructor

-- | A mnemonic and its one operand.
--
-- The @try@ covers only the mnemonic and the space after it -- just
-- enough to let @local.get@, @local.set@ and @local.tee@, which share a
-- prefix, be told apart, and to let @br@ fall through to @br_if@. The
-- operand is parsed committed, so a complaint about it is reported where
-- it happened instead of being backtracked away and resurfacing as
-- \"no mnemonic matched here\".
cmd1 :: String -> Parser a -> Parser a
cmd1 mnemonic arg = try (string mnemonic >> hspace1) >> arg

-- | A load\/store, whose static offset may be left off when it is zero:
-- @i32.load@ and @i32.load 0@ assemble to the same thing.
--
-- Deciding which it is needs one character of lookahead and nothing
-- more: a digit starts an offset, anything else (another mnemonic on the
-- same line, a comment, the end of the line) means there isn't one.
--
-- An absent offset still becomes a 'Ref', carrying the mnemonic's own
-- position: that is what a field guard reports against, and every other
-- operand in this ISA reaches 'derefMnemonic' the same way.
memOp :: (IsWord w) => String -> (Ref w -> a) -> Parser a
memOp mnemonic constructor = do
    pos <- getSourcePos
    void $ string mnemonic
    constructor <$> (offset <|> pure (zeroAt pos))
    where
        offset = try (hspace1 >> void (lookAhead digitChar)) >> constantRef
        zeroAt pos = ValueR{refPrepare = id, refPos = pos, refValue = 0}

instance (IsWord w) => DerefMnemonic (Wasm32Isa w) w where
    derefMnemonic f _offset i =
        let fieldErr what x = what <> " " <> x <> " is not in 0..255"
         in case i of
                I32Const l -> I32Const (resolveRef Right f l)
                I32Load k -> I32Load (resolveRef (u8 (fieldErr "i32.load offset")) f k)
                I32Store k -> I32Store (resolveRef (u8 (fieldErr "i32.store offset")) f k)
                I32Load8U k -> I32Load8U (resolveRef (u8 (fieldErr "i32.load8_u offset")) f k)
                I32Load8S k -> I32Load8S (resolveRef (u8 (fieldErr "i32.load8_s offset")) f k)
                I32Store8 k -> I32Store8 (resolveRef (u8 (fieldErr "i32.store8 offset")) f k)
                Br k -> Br (resolveRef (u8 (fieldErr "br depth")) f k)
                BrIf k -> BrIf (resolveRef (u8 (fieldErr "br_if depth")) f k)
                LocalGet k -> LocalGet (resolveRef (u8 (fieldErr "local.get index")) f k)
                LocalSet k -> LocalSet (resolveRef (u8 (fieldErr "local.set index")) f k)
                LocalTee k -> LocalTee (resolveRef (u8 (fieldErr "local.tee index")) f k)
                LocalsReserve k -> LocalsReserve (resolveRef (u8 (fieldErr "locals.reserve count")) f k)
                Call{callParamCount, callResultCount} ->
                    Call
                        { callParamCount = resolveRef (u8 (fieldErr "call paramCount")) f callParamCount
                        , callResultCount = resolveRef (u8 (fieldErr "call resultCount")) f callResultCount
                        }
                I32Add -> I32Add
                I32Sub -> I32Sub
                I32Mul -> I32Mul
                I32DivS -> I32DivS
                I32RemS -> I32RemS
                I32And -> I32And
                I32Or -> I32Or
                I32Xor -> I32Xor
                I32Shl -> I32Shl
                I32ShrS -> I32ShrS
                I32ShrU -> I32ShrU
                I32Eq -> I32Eq
                I32LtS -> I32LtS
                I32LeS -> I32LeS
                I32GtS -> I32GtS
                I32GeS -> I32GeS
                I32LtU -> I32LtU
                I32LeU -> I32LeU
                I32GtU -> I32GtU
                I32GeU -> I32GeU
                If -> If
                Else -> Else
                End -> End
                Loop -> Loop
                Block -> Block
                Drop -> Drop
                Select -> Select
                Unreachable -> Unreachable
                Return -> Return
                SpInit -> SpInit
                Halt -> Halt

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

-- | What a @br@ does, worked out from where it is written.
--
-- Both halves are static: the scope @depth@ levels out is fixed by the
-- nesting the branch sits in, and so is whether reaching it means
-- re-entering a @loop@ or stepping out of a @block@. The machine needs
-- no stored link to follow and no record to read in order to decide.
data BranchPlan = BranchPlan
    { bpTarget :: Int
    -- ^ Where to jump: a @loop@'s first body instruction, or just past a
    -- @block@'s own matching @end@.
    , bpPop :: Int
    -- ^ Records to retire -- the ones nested between the branch and its
    -- target, plus the target itself when it is a @block@, since
    -- reaching a block closes it and reaching a loop does not.
    , bpCheck :: Int
    -- ^ Records whose kind the machine confirms before touching
    -- anything: everything it would pop, plus the one it lands on. See
    -- 'checkOwnScopes'.
    }
    deriving (Eq, Show)

-- | Everything about a loaded program's control flow that is fixed
-- before it runs, so that nothing has to be searched for while it does.
data ScopePlan = ScopePlan
    { spTargets :: IntMap Int
    -- ^ @block@\/@loop@ to its own matching @end@, @if@\/@else@ to where
    -- its skip path resumes. See 'resolveScopeTargets'.
    , spBranches :: IntMap BranchPlan
    -- ^ @br@\/@br_if@ to what it does. See 'resolveBranches'.
    , spEndPops :: IntSet
    -- ^ The @end@s that close a @block@ or @loop@, as opposed to an
    -- @if@\'s @end@, which closes nothing. Knowing which is which here
    -- is what replaced comparing a record's @csEnd@ against @pc@ at run
    -- time to find out.
    }
    deriving (Eq, Show)

instance Semigroup ScopePlan where
    a <> b =
        ScopePlan
            { spTargets = spTargets a <> spTargets b
            , spBranches = spBranches a <> spBranches b
            , spEndPops = spEndPops a <> spEndPops b
            }

instance Monoid ScopePlan where
    mempty = ScopePlan mempty mempty mempty

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

-- | The size in bytes of each operand an instruction encodes after its
-- opcode, which is all 'byteSize' needs to know about any of them. See the
-- \"Instruction encoding\" section for the four shapes these make up.
--
-- Listed exhaustively, with no catch-all: an instruction added without a
-- line here is a missing-pattern warning, where a catch-all would have
-- silently given it a bare opcode's size. Checking that a value fits the
-- width named here is 'derefMnemonic''s job.
operandBytes :: Wasm32Isa w l -> [Int]
operandBytes instruction = case instruction of
    I32Const{} -> [byteSizeT @Int32]
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
    I32Load{} -> [immediateBytes]
    I32Store{} -> [immediateBytes]
    I32Load8U{} -> [immediateBytes]
    I32Load8S{} -> [immediateBytes]
    I32Store8{} -> [immediateBytes]
    If -> []
    Else -> []
    End -> []
    Loop -> []
    Block -> []
    Br{} -> [immediateBytes]
    BrIf{} -> [immediateBytes]
    Drop -> []
    Select -> []
    Unreachable -> []
    LocalGet{} -> [immediateBytes]
    LocalSet{} -> [immediateBytes]
    LocalTee{} -> [immediateBytes]
    LocalsReserve{} -> [immediateBytes]
    Call{} -> [immediateBytes, immediateBytes]
    Return -> []
    SpInit -> []
    Halt -> []

instance ByteSize (Wasm32Isa w l) where
    byteSize instruction = opcodeBytes + sum (operandBytes instruction)

-- | One byte, which 44 instructions leave plenty of room in. Nothing here
-- encodes an operand into spare opcode bits, so an instruction's size is
-- its opcode plus its operands and nothing else.
opcodeBytes :: Int
opcodeBytes = 1

-- | Every instruction immediate but @i32.const@'s: a branch depth, a local
-- index, one of @call@'s counts, a load\/store offset. One byte, so
-- @0@-@255@. A 'CallScope' stores the counts at this same width, so what
-- the source can say and what a call record can hold agree. A 0-255 offset
-- covers a struct field or a small array index; further is still reachable
-- with @i32.add@.
--
-- 'derefMnemonic' enforces it, with @u8@ -- the same shared field guard
-- the other ISAs check their immediates against. None of these operands
-- can be a label, so each is parsed with 'constantRef': a depth, an index
-- and a count are not addresses, and an @i32.add@ is the way to reach a
-- computed one.
immediateBytes :: Int
immediateBytes = 1

-- | The machine: a program counter, the two stack pointers and the frame
-- they describe, the loaded program's 'ScopePlan', and memory. Opaque on
-- purpose -- reports reach it through 'reprState' and the @stack@,
-- @locals@, @layout@ and @dump@ views, which is everything outside this
-- module ever needs from it. The \"The two stacks\" section below explains
-- what the pointers mean.
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
    -- ^ How many memory accesses the *last executed* instruction made,
    -- its own fetch included. Reset per step in 'instructionStep';
    -- exposed to reports as @memAccesses@.
    , scopePlan :: ScopePlan
    -- ^ Everything about the loaded program's control flow that is fixed
    -- before it runs -- where each scope instruction jumps to, what each
    -- @br@ does, which @end@s close a record. Built once by 'initState'
    -- (see 'resolveProgram'), read-only thereafter. A real engine keeps
    -- the same tables, for the same reason: the alternative is searching
    -- memory for an answer that was never going to change.
    , mem :: IoMem (Wasm32Isa w w) w
    , memAccessCount :: Int
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

-- | Which kind of record sits at @addr@, reading its first word only --
-- half of what decoding a whole record costs. Enough for a caller that
-- already knows what the record should be and only has to confirm it.
readScopeTagAt :: forall w. (IsWord w) => Int -> State (Wasm32St w) (Either Text ControlTag)
readScopeTagAt addr = do
    word0 <- getWord @w addr
    return $ first (\err -> "control stack corrupted at " <> hexAddr 2 addr <> ": " <> err) (decodeScopeTag word0)

-- | Confirm the innermost @n@ records are the @block@\/@loop@ records the
-- 'ScopePlan' says enclose this point, before a branch retires any of
-- them.
--
-- A branch's target and its pop count come from where it is written, so
-- the one thing the machine cannot know from that alone is whether
-- control /arrived/ the way the nesting implies. It cannot if a function
-- body sits inside another function's scope and is entered by @call@ --
-- and then the records the branch would pop include that call's own,
-- whose loss would strand the callee. One word per record is what it
-- costs to refuse instead: the tag, nothing else.
--
-- The branch that matters most pays least. @br 0@ back into a loop
-- retires nothing and checks the one record it re-enters.
checkOwnScopes :: forall w. (IsWord w) => Int -> State (Wasm32St w) (Either Text ())
checkOwnScopes n = do
    st <- get
    let addrs = take n (scopeAddrs @w st)
    if length addrs < n
        then
            return
                $ Left
                $ "br/br_if: depth reaches past the "
                    <> show (length addrs)
                    <> " scope(s) actually open"
        else go addrs
    where
        go [] = return $ Right ()
        go (addr : rest) =
            readScopeTagAt @w addr >>= \case
                Left err -> return $ Left err
                Right CallTag -> return $ Left "br/br_if: depth reaches past the enclosing function; use return to leave it"
                Right _ -> go rest

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

popValues :: forall w. (IsWord w) => Int -> State (Wasm32St w) [w]
popValues n = reverse <$> replicateM n popValue

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
            ["layout", f] -> withFormat f (renderLayout labels view) Nothing
            ["layout", f, n] -> withFrameLimit n (withFormat f (renderLayout labels view))
            ["dump", f] -> withFormat f (renderDump labels view) Nothing
            ["dump", f, n] -> withFrameLimit n (withFormat f (renderDump labels view))
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
            -- rather than leaving addresses permanently decimal. See
            -- 'LayoutFormat' for what each half gets.
            withFormat :: Text -> (LayoutFormat w -> Text) -> Maybe Int -> Text
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

-- $encoding
--
-- Nothing here is encoded to real bytes: the simulator holds a decoded
-- 'Wasm32Isa' per instruction, so the layout below is what an instruction
-- /would/ occupy. That is not idle, though -- 'byteSize' hands out exactly
-- these sizes, so every code address, every @pc@ a dump prints and every
-- target a @call@ reaches depends on them.
--
-- In Erlang bit syntax -- illustrative notation only, the project is
-- Haskell, but a precise and standard way to say which bits are which, and
-- the same notation "Wrench.Isa.Wasm32.ControlRecord" uses for a control
-- record. Four shapes, most-significant field first:
--
-- > %% 31 bare opcodes -- 1 byte
-- > %% i32.add, i32.eq, if, else, end, block, loop, drop, select,
-- > %% unreachable, return, sp.init, halt, ...
-- > <<Opcode:8>>
-- >
-- > %% i32.const -- 5 bytes
-- > <<Opcode:8, Value:32>>
-- >
-- > %% 11 with one one-byte immediate -- 2 bytes
-- > %% i32.load, i32.store, i32.load8_u, i32.load8_s, i32.store8,
-- > %% br, br_if, local.get, local.set, local.tee, locals.reserve
-- > <<Opcode:8, Imm:8>>
-- >
-- > %% call -- 3 bytes
-- > <<Opcode:8, ParamCount:8, ResultCount:8>>
--
-- The rules behind those four:
--
-- * __One opcode byte__ ('opcodeBytes'). 44 instructions fit in 256 with
--   room left over, and no operand is squeezed into spare opcode bits --
--   which is why an instruction's size is its opcode plus its operands and
--   nothing more.
-- * __Operands are whole bytes__ ('operandBytes'), never bitfields packed
--   across a byte boundary. A control record earns its bit packing by being
--   written and read at run time, thousands of times in a loop; an
--   instruction is laid out once, when the program loads.
-- * __@i32.const@ carries a full machine word__ (4 bytes), because what it
--   pushes is any value at all -- a constant, a @.data@ address, a function
--   address. There is no narrower form and no @%hi@\/@%lo@ pair to build
--   one from, which is the single thing this ISA spends the most space on
--   and the reason @i32.const@ is 5 bytes where every other instruction is
--   1, 2 or 3.
-- * __Every other immediate is one byte__ ('immediateBytes'), so @0@-@255@,
--   and 'derefMnemonic' refuses anything else rather than truncating it.
--   @call@'s two counts share the width with the 'CallScope' field that
--   stores them, so what a call site can declare and what its record can
--   hold agree exactly.
--
-- An instruction therefore occupies 1, 2, 3 or 5 bytes, and a dump shows
-- the span each one covers:
--
-- > mem[0x004..0x008] (5 B): 	I32Const 2147483647 	@_start
-- > mem[0x009..0x00a] (2 B): 	I32Load 255
-- > mem[0x00b..0x00d] (3 B): 	Call {callParamCount = 1, callResultCount = 1}
-- > mem[0x00e..0x00e] (1 B): 	Halt

-- $stacks
--
-- Memory is split in half: the lower half holds code and @.data@, the upper
-- half is where the two stacks live. Both grow from a single __root__, in
-- opposite directions.
--
-- With @memory_size: 0x100@ the halves meet at @0x80@ and the root lands
-- three quarters up the stack region, at @0xe0@:
--
-- > 0x000                0x080                 0x0e0             0x100
-- >   |--- .text, .data ---|--- operand stack ---|- control stack -|
-- >                                        sp <--+--> ctrlSp
-- >                        ^ operandFloor        ^ root            ^ memCapacity
--
-- Those are the names the @layout@ report view prints. Here is what it shows
-- once @_start@ has called @f(5)@ and @f@ has reserved itself one local:
--
-- > #0 _start (pc=0x1c)
-- >   (operands): empty, base=0xe0
-- >   mem[0xd8..0xdf]: locals
-- >     0: 0x00000005
-- >     1: 0x00000007
-- >   mem[0xe0..0xe7]: CallScope { csCallerFrameBase = 0xdc, csCallerLocalCount = 0, csReturnPc = 0x1c, csResultCount = 1 }
-- >   mem[0xd4..0xd7]: (operands)
-- >     0: 0x0000000c
--
-- Every span below @0xe0@ is operand stack -- @f@'s two locals, then the one
-- value it has computed -- and the single record at @0xe0@ and up is the
-- control stack: the call that suspended @_start@. The two meet at the root
-- and grow away from it, so the picture above /is/ the view, read sideways.
-- See "Wrench.Isa.Wasm32.Layout" for the full reading guide.
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
-- 'Drop' discards whatever is on top, without needing a local to put it
-- in. A value pushed before a @block@ or @loop@ is opened
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
-- one still open. All four are bare opcodes carrying nothing, exactly as in
-- real WebAssembly: loading the program matches each one with its own @end@
-- once and remembers where it jumps to ('resolveScopeTargets'), so nothing
-- is scanned for while the program runs, and an unbalanced @block@, @else@
-- or @end@ stops it before its first instruction rather than whenever some
-- forward scan happens to fall off the end of memory.
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
-- Both halves of what a branch does are fixed before the program runs: the
-- scope @n@ levels out is whichever one the nesting puts there, and whether
-- reaching it re-enters a @loop@ or steps out of a @block@ follows from
-- which it is. So a branch searches for nothing -- it retires @n@ records
-- (or @n + 1@, for a block, which closes too) by moving @ctrlSp@, and jumps
-- to an address 'resolveBranches' already worked out. A depth naming a
-- scope that does not enclose the branch is rejected when the program
-- loads, not when the branch runs.
--
-- Retiring a record is nothing but retreating @ctrlSp@; the operand stack
-- is untouched, so anything a scope's body pushed and never popped survives
-- the scope closing.
--
-- A branch's depth still cannot reach across a function call boundary. The
-- one thing the nesting cannot say is how control /arrived/ -- a function
-- whose body sits inside another function's scope and is entered by @call@
-- reaches its branch with that call's record in the way -- so before
-- retiring anything a branch confirms the records are the ones it expects,
-- at one word each ('checkOwnScopes'). Finding the call's record there is
-- an error: a structured branch is scoped to the function it appears in,
-- and leaving a function is @return@'s job.

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
-- * __Scope nesting is checked when the program loads, not before.__ Real
--   WebAssembly validates a module's structure up front and refuses to
--   instantiate a malformed one. Here the same matching happens at the same
--   point -- 'initState' builds a 'ScopePlan' and reports an unbalanced
--   @block@ before the first instruction runs -- but it is the machine
--   doing it, not a separate validation pass, and nothing else about a
--   program is validated alongside it.
-- * __A memory instruction has no alignment immediate.__ Real
--   WebAssembly's loads and stores carry an alignment hint alongside the
--   offset, which an engine uses to pick a faster access and which has no
--   effect on semantics. There is nothing here for it to hint at.
-- * __A lot is simply absent, on purpose.__ There is no
--   @i32.clz@\/@i32.ctz@\/@i32.popcnt@, no @i32.rotl@\/@i32.rotr@, no
--   @i32.div_u@\/@i32.rem_u@, no 16-bit loads or stores, no @br_table@, no
--   @nop@, and neither @i32.eqz@ nor @i32.ne@. The bit-counting and rotate
--   instructions are left out precisely /because/ counting leading zeros,
--   counting set bits, computing parity and rotating a word are exercises
--   in this course -- an instruction that is the whole answer to an
--   assignment teaches nothing. @i32.div_u@, @i32.rem_u@, @br_table@, @nop@
--   and the 16-bit accesses are out because no other ISA here has them: a
--   capability absent from risc-iv and m68k is one the course has already
--   decided its students don't need. Rotates are @i32.shl@, @i32.shr_u@
--   and @i32.or@; a switch is a chain of @i32.eq@ and @br_if@; a halfword
--   is two byte accesses.
--
--   @i32.eqz@ is the one real cost. It is the single most-used instruction
--   in real WebAssembly, since every loop condition ends in one, and
--   writing it as @i32.const 0@, @i32.eq@ spends two instructions at every
--   one of them -- visible in all of the examples. It stays out anyway, so
--   that there is exactly one way to ask \"is this zero\" and exactly one
--   way to negate, and both are the same two instructions, rather than a
--   dedicated opcode for one and the idiom for the other.
-- * __Operand order and arithmetic follow the spec.__ Where this ISA does
--   implement something WebAssembly has, it matches: @i32.store@ takes its
--   value above its address, shift amounts mask to 5 bits, @i32.div_s@
--   truncates toward zero and traps on @INT_MIN \/ -1@, and @i32.rem_s@
--   traps only on a zero divisor.
