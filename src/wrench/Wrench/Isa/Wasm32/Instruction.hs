{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
The instruction set itself: the 'Wasm32Isa' type, the assembly syntax that
produces it, the field guards every immediate has to pass, and the size each
instruction occupies.
-}
module Wrench.Isa.Wasm32.Instruction (
    Wasm32Isa (..),
    wasm32Mnemonics,
) where

import Data.String.Interpolate (i)
import Relude
import Text.Megaparsec (choice, getSourcePos, lookAhead, try)
import Text.Megaparsec.Char (digitChar, hspace, hspace1, string)
import Wrench.Machine.Types
import Wrench.Machine.Word (u8)
import Wrench.Translator.Parser.Misc
import Wrench.Translator.Parser.Types
import Wrench.Translator.Types

-- | One instruction. Each entry starts with its syntax and stack effect.
data Wasm32Isa w l
    = -- | > i32.const <value: i32> ( -- value: i32 )
      --
      -- Push an immediate: a number or a label, which is how a function
      -- address is produced (see Functions and locals).
      I32Const l
    | -- | > i32.add ( a: i32, b: i32 -- sum: i32 )
      I32Add
    | -- | > i32.sub ( a: i32, b: i32 -- difference: i32 )
      --
      -- @a - b@.
      I32Sub
    | -- | > i32.mul ( a: i32, b: i32 -- product: i32 )
      --
      -- Keeps the low 32 bits of the product.
      I32Mul
    | -- | > i32.div_s ( a: i32, b: i32 -- quotient: i32 )
      --
      -- Signed division truncating toward zero: @-7 \/ 2@ is @-3@. Stops
      -- with an error on a zero divisor and on @minBound \/ -1@.
      I32DivS
    | -- | > i32.rem_s ( a: i32, b: i32 -- remainder: i32 )
      --
      -- Signed remainder with the sign of @a@: @-7 % 2@ is @-1@. Stops with
      -- an error only on a zero divisor; @minBound % -1@ is @0@.
      I32RemS
    | -- | > i32.and ( a: i32, b: i32 -- bits: i32 )
      I32And
    | -- | > i32.or ( a: i32, b: i32 -- bits: i32 )
      I32Or
    | -- | > i32.xor ( a: i32, b: i32 -- bits: i32 )
      I32Xor
    | -- | > i32.shl ( value: i32, amount: i32 -- shifted: i32 )
      --
      -- Shift left. The amount is taken modulo 32, as for every shift.
      I32Shl
    | -- | > i32.shr_s ( value: i32, amount: i32 -- shifted: i32 )
      --
      -- Arithmetic (sign-extending) shift right.
      I32ShrS
    | -- | > i32.shr_u ( value: i32, amount: i32 -- shifted: i32 )
      --
      -- Logical (zero-filling) shift right.
      I32ShrU
    | -- | > i32.eqz ( value: i32 -- flag: i32 )
      --
      -- Push @1@ if the value is @0@, else @0@.
      I32Eqz
    | -- | > i32.eq ( a: i32, b: i32 -- flag: i32 )
      --
      -- Push @1@ if @a = b@, else @0@, like every comparison.
      I32Eq
    | -- | > i32.lt_s ( a: i32, b: i32 -- flag: i32 )
      --
      -- Signed @a \< b@.
      I32LtS
    | -- | > i32.le_s ( a: i32, b: i32 -- flag: i32 )
      --
      -- Signed @a \<= b@.
      I32LeS
    | -- | > i32.gt_s ( a: i32, b: i32 -- flag: i32 )
      --
      -- Signed @a \> b@.
      I32GtS
    | -- | > i32.ge_s ( a: i32, b: i32 -- flag: i32 )
      --
      -- Signed @a \>= b@.
      I32GeS
    | -- | > i32.lt_u ( a: i32, b: i32 -- flag: i32 )
      --
      -- Unsigned @a \< b@, so @-1@ compares greater than @1@.
      I32LtU
    | -- | > i32.le_u ( a: i32, b: i32 -- flag: i32 )
      --
      -- Unsigned @a \<= b@.
      I32LeU
    | -- | > i32.gt_u ( a: i32, b: i32 -- flag: i32 )
      --
      -- Unsigned @a \> b@.
      I32GtU
    | -- | > i32.ge_u ( a: i32, b: i32 -- flag: i32 )
      --
      -- Unsigned @a \>= b@.
      I32GeU
    | -- | > i32.load <offset: u8> ( address: i32 -- value: i32 )
      --
      -- Push the word at @address + offset@. Every load and store takes
      -- an optional offset, @0@ to @255@, so a struct field or a small
      -- array element needs no @i32.add@:
      --
      -- > i32.const point
      -- > i32.load 4            ; point.y
      I32Load l
    | -- | > i32.load8_u <offset: u8> ( address: i32 -- value: i32 )
      --
      -- Like 'I32Load', but reads one byte and zero-extends it.
      I32Load8U l
    | -- | > i32.load8_s <offset: u8> ( address: i32 -- value: i32 )
      --
      -- Like 'I32Load8U', but sign-extends.
      I32Load8S l
    | -- | > i32.store <offset: u8> ( address: i32, value: i32 -- )
      --
      -- Write the word at @address + offset@. The address goes under the
      -- value, so push it first.
      I32Store l
    | -- | > i32.store8 <offset: u8> ( address: i32, value: i32 -- )
      --
      -- Like 'I32Store', but writes only the low byte.
      I32Store8 l
    | -- | > block ( -- )
      --
      -- Open a scope; a branch to it continues after its @end@.
      Block
    | -- | > loop ( -- )
      --
      -- Open a scope; a branch to it continues at the start of its body.
      -- Without a branch the body runs once.
      Loop
    | -- | > if ( condition: i32 -- )
      --
      -- Run the following body if the condition is non-zero, otherwise
      -- continue after the matching @else@ or @end@. Opens no scope.
      If
    | -- | > else ( -- )
      --
      -- Reached at the end of a taken @if@ body: continue after the
      -- matching @end@.
      Else
    | -- | > end ( -- )
      --
      -- Close the @block@ or @loop@ it belongs to; for an @if@, do
      -- nothing.
      End
    | -- | > br <depth: u8> ( -- )
      --
      -- Branch to the scope @depth@ levels out, @0@ being the innermost
      -- open @block@ or @loop@, and close every scope inside it. A target
      -- @block@ is closed too, since execution leaves it; a target @loop@
      -- stays open.
      Br l
    | -- | > br_if <depth: u8> ( condition: i32 -- )
      --
      -- 'Br' if the condition is non-zero, otherwise continue.
      BrIf l
    | -- | > unreachable ( -- )
      --
      -- Stop with an error.
      Unreachable
    | -- | > call <paramCount: u8>, <resultCount: u8> ( ...params: i32, target: i32 -- ...results: i32 )
      --
      -- Call the function at @target@: the @paramCount@ values under it
      -- become its first locals, and @resultCount@ values come back.
      -- Neither count is checked against the callee.
      Call {callParamCount :: l, callResultCount :: l}
    | -- | > return ( ...results: i32 -- )
      --
      -- Leave the current function, closing every scope it still has open
      -- and passing the call's @resultCount@ values from the top of the
      -- stack back to the caller.
      Return
    | -- | > local.get <index: u8> ( -- value: i32 )
      LocalGet l
    | -- | > local.set <index: u8> ( value: i32 -- )
      LocalSet l
    | -- | > local.tee <index: u8> ( value: i32 -- value: i32 )
      --
      -- Like @local.set@, but keeps the value on the stack.
      LocalTee l
    | -- | > locals.reserve <count: u8> ( -- )
      --
      -- Add @count@ locals after the ones the frame has. Use it first
      -- thing in a function, before pushing anything: the new locals are
      -- the next words down the stack. They are not cleared.
      LocalsReserve l
    | -- | > drop ( value: i32 -- )
      --
      -- Discard the top value, e.g. a result the caller does not need.
      Drop
    | -- | > select ( a: i32, b: i32, condition: i32 -- chosen: i32 )
      --
      -- Push @a@ if the condition is non-zero, @b@ otherwise.
      Select
    | -- | > sp.init ( address: i32 -- )
      --
      -- Move 'stackRoot', where both stacks start, to @address@. Both
      -- stacks restart empty there, so use it before anything is pushed:
      -- the frame's locals and any open scope are lost. Nothing checks the
      -- address.
      SpInit
    | -- | > halt ( -- )
      --
      -- Stop the machine.
      Halt
    deriving (Eq, Functor, Show)

-- | Every mnemonic the 'MnemonicParser' instance accepts.
wasm32Mnemonics :: [Text]
wasm32Mnemonics =
    [ "i32.const"
    , "i32.add"
    , "i32.sub"
    , "i32.mul"
    , "i32.div_s"
    , "i32.rem_s"
    , "i32.and"
    , "i32.or"
    , "i32.xor"
    , "i32.shl"
    , "i32.shr_s"
    , "i32.shr_u"
    , "i32.eqz"
    , "i32.eq"
    , "i32.lt_s"
    , "i32.le_s"
    , "i32.gt_s"
    , "i32.ge_s"
    , "i32.lt_u"
    , "i32.le_u"
    , "i32.gt_u"
    , "i32.ge_u"
    , "i32.load8_u"
    , "i32.load8_s"
    , "i32.load"
    , "i32.store8"
    , "i32.store"
    , "if"
    , "else"
    , "end"
    , "loop"
    , "block"
    , "br"
    , "br_if"
    , "drop"
    , "select"
    , "unreachable"
    , "local.get"
    , "local.set"
    , "local.tee"
    , "locals.reserve"
    , "call"
    , "return"
    , "sp.init"
    , "halt"
    ]

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
                    , cmd0 "i32.eqz" I32Eqz
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
            cmd0 op constructor = string op >> return constructor

            cmd1 op arg = try (string op >> hspace1) >> arg

            memOp op constructor = do
                pos <- getSourcePos
                void $ string op
                constructor <$> (offset <|> pure (zeroAt pos))
                where
                    offset = try (hspace1 >> void (lookAhead digitChar)) >> constantRef
                    zeroAt pos = ValueR{refPrepare = id, refPos = pos, refValue = 0}

instance (IsWord w) => DerefMnemonic (Wasm32Isa w) w where
    derefMnemonic f _offset instruction =
        let fieldErr (what :: Text) x = [i|#{what} #{x} is not in 0..255|]
         in case instruction of
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
                I32Eqz -> I32Eqz
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

instance ByteSize (Wasm32Isa w l) where
    byteSize instruction = opcodeBytes + sum operandBytes
        where
            opcodeBytes = 1
            immediateBytes = 1
            operandBytes = case instruction of
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
                I32Eqz -> []
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
