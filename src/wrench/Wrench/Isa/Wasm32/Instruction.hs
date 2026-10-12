{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
The instruction set itself: the 'Wasm32Isa' type, the assembly syntax that
produces it, the field guards every immediate has to pass, and the size each
instruction occupies. Everything here is about /the language/ -- none of it
knows there is a machine.

The reference for each instruction is on its constructor below, and
"Wrench.Isa.Wasm32" collects it alongside the prose that explains the model.
-}
module Wrench.Isa.Wasm32.Instruction (
    Wasm32Isa (..),
    operandBytes,
    opcodeBytes,
    immediateBytes,
) where

import Relude
import Text.Megaparsec (choice, getSourcePos, lookAhead, try)
import Text.Megaparsec.Char (digitChar, hspace, hspace1, string)
import Wrench.Machine.Types
import Wrench.Machine.Word (u8)
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
