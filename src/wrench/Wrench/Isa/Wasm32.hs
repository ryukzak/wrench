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

This module is the ISA's reference: the instruction set below, and the prose
that explains the model it belongs to. It holds no code -- the implementation
is split by phase, and each part is worth reading on its own terms:

* "Wrench.Isa.Wasm32.Instruction" -- the instruction type, the assembly
  syntax, the field guards, and what each instruction costs in bytes.
* "Wrench.Isa.Wasm32.State" -- the machine state, the memory map, and the
  two stacks, control-record format included.
* "Wrench.Isa.Wasm32.Execute" -- resolving a loaded program's control flow,
  then running it.
* "Wrench.Isa.Wasm32.Report" -- the @stack@, @locals@, @layout@ and @dump@
  views a @.yaml@ report can ask for.
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

import Wrench.Isa.Wasm32.Execute ()
import Wrench.Isa.Wasm32.Instruction (Wasm32Isa (..))
import Wrench.Isa.Wasm32.Report ()
import Wrench.Isa.Wasm32.State (Wasm32St (..))

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
-- the same notation "Wrench.Isa.Wasm32.State" uses for a control
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
-- See "Wrench.Isa.Wasm32.Report" for the full reading guide.
--
-- [operand stack]: Holds a function's locals and its values. It descends
-- from the root, the classic hardware convention: @sp@ is the address of
-- the most recently pushed word, decremented /before/ a push writes and
-- incremented /after/ a pop reads. Its wall is 'operandFloor' -- 'memTop',
-- raised past any memory-mapped I\/O port that would otherwise sit inside
-- the stack region.
--
-- [control stack]: Holds one fixed-size record per open @block@, @loop@ or
-- call -- see "Wrench.Isa.Wasm32.State". It ascends from the root.
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
