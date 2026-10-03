# Wasm32 Instruction Set Architecture (ISA) Documentation

The Wasm32 ISA is a 32-bit stack-based instruction set designed for educational purposes. This documentation provides an overview of the instructions available in the Wasm32 ISA, their syntax, and their semantics, and -- in detail -- how control flow and the stack actually work underneath.

## Architecture Overview

The Wasm32 architecture is a 32-bit stack-based architecture. It features:

- Two stacks, both in ordinary memory: an *operand stack* holding locals and values, and a *control stack* holding the bookkeeping for open `block`/`loop`/call scopes -- no general-purpose registers
- Structured control flow (`block`, `loop`, `if`/`else`) instead of arbitrary jumps, with every scope's target resolved at assembly time
- Function calls where a function address is just an ordinary value, so a direct call and a call through a value computed at runtime are the same instruction
- Memory-mapped I/O

This stack-based architecture offers a compact structured-control-flow model, making it useful for studying function calls, local variables, loops, and low-level memory access.

Comments in Wasm32 assembly code are denoted by the `;` character.

Inspired by [WebAssembly](https://webassembly.github.io/spec/core/)

## Program Structure

A Wasm32 program uses the normal `.data` and `.text` sections. Execution starts at the `_start` label. A function is nothing more than an ordinary label followed by instructions; there is no directive that declares a function, no header instruction at its entry point, and no footer instruction at its exit -- a function ends wherever its last `return` (or its last instruction, falling through) happens to be.

```assembly
    .text

_start:
    i32.const 5
    i32.const factorial
    call 1, 1
    halt

factorial:
    local.get 0
    i32.const 1
    i32.le_s
    if
        i32.const 1
        return
    end
    local.get 0
    local.get 0
    i32.const 1
    i32.sub
    i32.const factorial
    call 1, 1
    i32.mul
    return
```

## The Two Stacks

Memory is split in half: the lower half holds code and `.data`; the upper half is where the two stacks live. Both grow from a single **root**, in opposite directions:

```text
0                     memTop                  root            memorySize
|-- .text, .data ------ | -------------------- R ----------------- |
                        |    <- operand stack  |  control stack -> |
                        ^ wall          sp <-- R --> ctrlSp        ^ wall
```

- The **operand stack** holds a function's locals and its values. It descends from the root, the classic hardware convention: `sp` is the address of the most recently pushed word, decremented *before* a push writes and incremented *after* a pop reads. Its wall is `memTop`.
- The **control stack** holds one fixed-size record per open `block`, `loop`, or call. It ascends from the root. `ctrlSp` is its frontier: one past the innermost live record. Its wall is the end of memory.

Because they grow *apart*, neither can ever reach the other: an arithmetic instruction cannot pop into a live control record, and pushing a scope cannot scribble over a local. What each can reach is its own wall, and the machine checks every push against it:

```text
operand stack overflow: push to 0xfc would reach below the stack region at 0x100
control stack overflow: scope record ending at 0x208 would reach past the end of memory at 0x200
```

Two messages rather than one, because the fixes differ: too much on the operand stack, or too deep a nesting of scopes and calls. This is the one place the ISA stops a malformed program instead of letting it corrupt itself.

The root defaults to three quarters of the way up the stack region -- a program pushes far more operands than it opens scopes, and a record is eight bytes. `sp.init` moves it, which is how a program that needs the other balance asks for it: a deeply recursive function wants the root low, a value-heavy loop wants it high. Nothing checks that the split leaves either stack enough room.

Code and `.data` spilling past `memTop` is *not* checked, so a program whose code and data exceed half the configured memory will have the operand stack descend into its own globals.

`if` is the exception among scopes: it pushes nothing. Nothing branches out of a taken branch early, so there is nothing to remember.

### Locals

A function's locals start with its parameters. They occupy a contiguous run of words starting at `frameBase`, one per index, addressed directly (`frameBase - index * 4` -- the operand stack descends, so local `0`, the first argument pushed, ends up at the *highest* address and each later one lower): whatever values the caller pushed just before `call` are, from the callee's perspective, its locals `0` through `paramCount - 1`. `frameBase` is not fixed; it moves to wherever the current function's locals happen to start, and is saved and restored across calls (see Functions, below).

`locals.reserve n` claims `n` more, zeroed, at the indices directly after the parameters. Since the operand stack descends, the words just below the current locals are exactly where the next ones belong, so this is a stack-pointer decrement and nothing more -- `call` never needed to know how wide a callee's frame is, and `return` already discards everything below `frameBase` in one step. Written as a function's first instruction:

```assembly
fact:
    locals.reserve 1       ; local 1: scratch, this activation only
    local.get 0
    local.set 1
```

The point is that a reserved local is private to the activation. A recursive function that keeps scratch in a `.data` cell has one cell shared by every level, so the nested call overwrites what the caller was holding -- and the result is silently wrong, not a trap. `_start` can reserve locals too, which matters because `_start` is never called and so has no parameters at all.

Being a stack-pointer decrement and nothing more, `locals.reserve` is not checked against the two ways to misuse it, and both fail silently:

- **Reserving after pushing operands** claims the words *below* them, so values already on the stack get renumbered as locals. `i32.const 42`, `locals.reserve 1`, `local.get 0` pushes `42` -- the constant became local `0`, and the zeroed word it reserved became an operand. Reserve before pushing anything.
- **Reserving inside a loop body** reserves again on every iteration, so the frame grows until the operand stack hits its wall. Reserve once, at the function's entry.

`local.get i` pushes local `i`'s value onto the operand stack; `local.set i` pops the top of the stack into local `i`; `local.tee i` does the same as `local.set` but pushes the value back afterward, leaving the stack depth unchanged. A local index is one byte, and nothing checks it against the active function's actual local count -- `local.get 7` in a one-parameter function that reserved nothing reads whatever word happens to sit there.

### Operand values

Every arithmetic, comparison, and memory instruction reads its operands from the top of the operand stack and pushes its result back. For a two-operand instruction, the *second* operand pushed ends up on top and is popped first -- so `i32.const 10`, `i32.const 3`, `i32.sub` computes `10 - 3 = 7`, not `3 - 10`. This matters for every non-commutative binary instruction: subtraction, division, remainder, shifts, and every comparison read their two operands as (first-pushed, second-pushed) in that order.

`dup` duplicates whatever is currently on top and `drop` discards it, without needing a local either way. A value pushed before a `block` or `loop` is opened stays reachable inside it: scope records are on their own stack, so nothing gets in the way of reaching down the operand stack. A counter can live on the operand stack across loop iterations the same way it can live in a local or a `.data` cell.

Nothing checks for operand-stack *underflow*. Popping more than the active frame pushed reads into its locals, and past those into the caller's frame -- whatever is physically there.

## Control Flow

`block`, `loop`, and `if` each open a scope; `end` closes the innermost one still open. All three are written bare in source -- the assembler finds each one's matching `end` and writes the byte distance into the instruction, so nothing is scanned for at run time and an unbalanced `block`, `else` or `end` is a translation error rather than something discovered mid-execution.

`br`/`br_if` refer to a scope not by name but by *depth*: how many enclosing scopes out to reach, counting the innermost currently-open one as `0`. An `if` does not count as a level -- only `block` and `loop` do.

```assembly
block                  ; depth 1 from inside the loop below
    loop                ; depth 0 from inside its own body
        local.get 0
        i32.const 0
        i32.eq
        br_if 1          ; exit the block: "break"
        local.get 0
        i32.const 1
        i32.sub
        local.set 0
        br 0             ; jump back to the loop's own start: "continue"
    end
end
```

Reaching a `loop` by depth jumps back to just after the `loop` instruction and leaves the scope open -- this is how a loop continues. Reaching a `block` by depth jumps forward to just after its matching `end` and closes it, along with every scope nested between the branch and it -- this is how a program breaks out of one or more levels at once. `br` always branches; `br_if` pops a condition first and only branches if it is non-zero, otherwise falling through to the next instruction.

`if` pops a condition and, if it is non-zero, falls straight into the body that follows. If the condition is zero, it skips to the matching `else` (if there is one) or past the matching `end` (if there isn't). `else` marks the alternative body, reached only by the taken `if`-body falling through to it -- at which point it unconditionally skips past the matching `end`, so the `else`-body never runs after the `if`-body already did.

### Control records, precisely

Each open `block`, `loop`, or call is a two-word record on the control stack, written at the moment the scope is entered. Every kind is the same width, whatever it needs to store:

- a `block` records where its own matching `end` is (`CsEnd`), so a `br` to it knows where to jump and its `end` knows it is the one that closes it;
- a `loop` records both its re-entry point (`CsStart`, just after the `loop` instruction) and its `CsEnd`;
- a call records the caller's `frameBase` and local count, to restore on return, plus its own return address and declared result count.

A `block` leaves its second word unused. Paying that word is what makes the control stack a fixed-stride array: the record `n` scopes out starts exactly `n + 1` strides below `ctrlSp`. That is why a `br` finds its target by arithmetic -- no chain of stored links to walk, and no memory read just to discover how wide the next record is.

The exact byte layout, in Erlang bit syntax (illustrative notation only -- the project is Haskell -- but a precise, standard way to say exactly which bits are which), most-significant field first:

```erlang
%% block -- 8 bytes
<<Tag:2, 0:8, CsEnd:22>>, <<0:32>>

%% loop -- 8 bytes
<<Tag:2, 0:8, CsStart:22>>, <<0:10, CsEnd:22>>

%% call -- 8 bytes
<<Tag:2, CsCallerLocalCount:8, CsCallerFrameBase:22>>, <<0:2, CsResultCount:8, CsReturnPc:22>>
```

Every address-shaped field -- `CsStart`/`CsEnd`/`CsReturnPc` (code addresses) and `CsCallerFrameBase` (a stack address) -- gets the same uniform 22 bits, and every count the same 8, in the same bit positions. There is no ISA-level reason to trust one part of the address space more than another, and a uniform layout means one decoder rather than three. 22 bits covers 4 MiB, far past the 64 KiB memory limit; 8 bits matches the one-byte counts `call` itself carries, so what a call site can declare and what its record can hold agree exactly.

Nothing is silently truncated. A field value that does not fit stops the program with the field's name, and an instruction immediate out of range for its own encoding is rejected at translate time.

`Tag` needs only 2 bits for three kinds; the one unused pattern is not a valid record, and reading it reports a corrupted control stack rather than guessing at a shape.

A `br`/`br_if` naming depth `n` steps out `n` records, closes every record strictly between the branch and the target (regardless of that record's own kind), and finally either re-enters the target (if it is a loop) or closes it too (if it is a block). Closing a record is nothing but retreating `ctrlSp`; the operand stack is untouched, so anything a scope's body pushed and never popped survives past the scope closing.

A `br`'s depth can never reach across a function call boundary: stepping outward stops with an error the moment it would pass through a call's own record. A structured branch is scoped to the function it appears in; leaving a function is `return`'s job, not a branch's, and the diagnostic says so.

## Functions

`i32.const some_function` produces a function's address exactly the way it produces any other label's address -- a function is just data once you have its address. `call` always pops its target off the top of the stack; there is no separate way to call a statically-known target that skips this. A call whose target is known when the program is written is simply `i32.const target` immediately followed by `call` -- and because the target is *just* a value, a program can equally well compute it (read it out of a local, say, if that local was set to a function's address earlier) and call through that instead. Both are the same instruction; only where the value came from differs.

`call` takes two more operands, written directly after it: how many values pushed just before the target are this call's arguments, and how many values the call is expected to leave behind. Neither is looked up anywhere -- both are simply written at the call site, and nothing checks that a callee's actual behavior matches what a particular call site declared. Getting this wrong (calling with the wrong argument count, or a callee returning a different number of values than a caller expects) is not caught; it corrupts the stack silently, the same way any other malformed program does.

At the moment `call` executes, the values pushed immediately before the popped target -- as many as its declared argument count -- become the callee's locals `0` downward (the operand stack descends, so the first argument pushed ends up at the highest address), without being copied anywhere: `call` simply records where they already are as the new `frameBase` and jumps to the target. A record describing the call (the caller's own `frameBase`, its own local count, the return address, and the declared result count) goes on the control stack.

`return` finds the nearest enclosing call (closing any `block`/`loop` still open along the way -- a `return` from inside a loop must still unwind it first), pops exactly as many values as that call declared it would return, discards the callee's entire frame -- its locals and its own record -- in a single step, and pushes the popped results back for the caller, at exactly the depth they would be at had the call consumed its arguments and produced its results in place. Falling off the end of a function's instructions without an explicit `return` is only correct if nothing is left open; an explicit `return` is needed for any exit before a function's last instruction.

## Differences from WebAssembly

This ISA borrows WebAssembly's shape, not its semantics, and the gaps are deliberate. The ones worth knowing:

- **Blocks have no result arity.** Real WebAssembly gives every `block`/`loop`/`if` a block type, and a `br` carries exactly that many values out, dropping the rest. Here a scope has no type at all: whatever the body pushed and did not pop simply stays on the operand stack after the scope closes. That makes `br` cheap and the model smaller, but it means a `br` out of a half-finished computation leaves its partial results behind.
- **`if` is not a branch target.** In real WebAssembly an `if` is a label like any other block, so `br 0` inside one exits the `if`. Here only `block` and `loop` count toward a depth.
- **A call's arity lives at the call site.** Real WebAssembly takes both counts from the callee's declared type and validates every call against it. Here the caller writes both numbers itself (`call <params>, <results>`) and nothing checks them against the callee. A mismatch is not caught -- it corrupts the stack silently, the same way any other malformed program does.
- **Locals are reserved by an instruction, not declared in a header.** Real WebAssembly encodes a function's extra locals in a declaration vector ahead of its body. Here `locals.reserve n` does the same job at run time, as the function's first instruction -- same zeroing, same per-activation privacy, but nothing validates that a function reserves before it indexes.
- **The target of a `call` is an address, not an index.** Real WebAssembly has `call` by function index and `call_indirect` through a table. Here a function address is an ordinary value, so one instruction covers both.
- **A lot is simply absent, on purpose.** There is no `i32.clz`/`i32.ctz`/`i32.popcnt`, no `i32.rotl`/`i32.rotr`, no `i32.div_u`/`i32.rem_u`, no 16-bit loads or stores, no `br_table`, no `nop`, and neither `i32.eqz` nor `i32.ne` -- `i32.const 0`, `i32.eq` covers both. The bit-counting and rotate instructions are left out precisely *because* counting leading zeros, counting set bits, computing parity and rotating a word are exercises in this course -- an instruction that is the whole answer to an assignment teaches nothing. The rest are out because no other ISA here has them: a capability absent from risc-iv and m68k is one the course has already decided its students don't need. Rotates are `i32.shl`, `i32.shr_u` and `i32.or`; a switch is a chain of `i32.eq` and `br_if`; a halfword is two byte accesses.
- **Operand order and arithmetic follow the spec.** Where this ISA does implement something WebAssembly has, it matches: `i32.store` takes its value above its address, shift amounts mask to 5 bits, `i32.div_s` truncates toward zero and traps on `INT_MIN / -1`, and `i32.rem_s` traps only on a zero divisor.

## ISA Specific State Views

- `stack:dec`, `stack:hex` -- every word of the operand stack from `sp` up to the end of the active function's locals, most recent first.
- `locals:dec`, `locals:hex` -- the active function's locals, in index order.
- `memAccesses:dec` -- how many memory accesses the last executed instruction made, its own fetch included. Useful for seeing what an instruction actually costs: `i32.const` is two (fetch, push), `i32.add` four (fetch, two pops, push), `block` three (fetch, two record words).
- `layout:dec`, `layout:hex` -- an annotated dump of both live stacks, broken into per-call frames, from the outermost active call down to the innermost/live one. Add a frame limit (`layout:hex:2`) to show only the most recent N frames and summarise the rest as `(N earlier frame(s) omitted)`.
- `dump:dec`, `dump:hex` -- the whole configured memory in address order: code and `.data`, the free space below the operand stack, the same annotated frame-by-frame view `layout` produces, then the free space above the control stack. Also takes a frame limit.

Within `layout`, every *suspended* frame (one that made a call and is waiting on it) gets a `#N <function> (pc=<pc>)` header. Both parts genuinely describe something in memory: that `pc` is exactly the `csReturnPc` sitting in the `CallScope` that suspended the frame, and the function name is the nearest label at or before it. The innermost/live frame gets no header -- neither its `pc` nor the name derived from it is stored anywhere (they live only in the interpreter's state), and `pc`/`pc:label` already show the live `pc` separately.

Each frame's spans then follow the same shape:

- its locals and its operand values each render as a `mem[a..b]: locals` / `mem[a..b]: (operands)` header with one indented `index: value` line per word, highest address (index `0`) first -- which is parameter-index order for locals and push order for operands. An empty operand span shows as `(operands): empty, base=<addr>` rather than being silently omitted.
- each of its open control records renders as one line: its `mem[a..b]` range followed by a Haskell-record-literal rendering of the decoded fields, e.g.

  ```text
  mem[0x108..0x10f]: CallScope { csCallerFrameBase = 0x1f8, csCallerLocalCount = 3, csReturnPc = 0x023, csResultCount = 1 }
  ```

  The decoded fields are shown rather than the raw words, which aren't independently readable once packed (see "Control records, precisely" above).

`:hex` formats every *address* the view mentions -- `mem[a..b]` ranges, a frame's `pc=`, and a record's `csStart`/`csEnd`/`csReturnPc`/`csCallerFrameBase` -- and uses only as many hex digits as this run's configured memory could need (a 512-byte memory needs 3, not the 8 an arbitrary 32-bit *value* gets). A record's counts stay decimal in both formats, since hex doesn't make a count more readable.

## Instructions

### Constants and Stack Manipulation

- **Constant**
    - **Syntax:** `i32.const <value>`
    - **Description:** Push an immediate value onto the stack.
    - **Operation:** `stack.push(<value>)`

- **Duplicate**
    - **Syntax:** `dup`
    - **Description:** Duplicate the top value on the stack, without needing a local.
    - **Operation:** `x <- stack.pop(); stack.push(x); stack.push(x)`

- **Drop**
    - **Syntax:** `drop`
    - **Description:** Discard the top value -- `dup` the other way round, and the only way to get rid of a value the program does not want (a callee result the caller ignores, say). Nothing else can do it: `local.set` needs a spare parameter to land in, and `i32.store` needs its destination address pushed *under* the value.
    - **Operation:** `stack.pop()`

- **Select**
    - **Syntax:** `select`
    - **Description:** Pop a condition, then two values; push the first-pushed of the two if the condition is non-zero, the second otherwise. A branchless two-way choice.
    - **Operation:** `c <- stack.pop(); y <- stack.pop(); x <- stack.pop(); stack.push(if c != 0 then x else y)`

### Arithmetic Instructions

- **Add**
    - **Syntax:** `i32.add`
    - **Description:** Add the top two values on the stack.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(x + y)`

- **Subtract**
    - **Syntax:** `i32.sub`
    - **Description:** Subtract the second-pushed value from the first-pushed value.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(x - y)`

- **Multiply**
    - **Syntax:** `i32.mul`
    - **Description:** Multiply the top two values on the stack.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(x * y)`

- **Signed Divide**
    - **Syntax:** `i32.div_s`
    - **Description:** Divide two signed values, truncating toward zero (so `-7 / 2` is `-3`, not `-4`). Division by zero and signed overflow (dividing the most negative representable value by `-1`) trap.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(trunc(signed(x) / signed(y)))`

- **Signed Remainder**
    - **Syntax:** `i32.rem_s`
    - **Description:** Compute the signed remainder, whose sign follows the dividend (so `-7 % 2` is `-1`). Only division by zero traps: unlike `i32.div_s`, the most negative value remainder `-1` is defined, and is `0`.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(signed(x) % signed(y))`

### Bitwise Instructions

- **And**
    - **Syntax:** `i32.and`
    - **Description:** Bitwise AND of the top two values.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(x & y)`

- **Or**
    - **Syntax:** `i32.or`
    - **Description:** Bitwise OR of the top two values.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(x | y)`

- **Xor**
    - **Syntax:** `i32.xor`
    - **Description:** Bitwise XOR of the top two values.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(x ^ y)`

- **Shift Left**
    - **Syntax:** `i32.shl`
    - **Description:** Shift left, masking the shift amount to its low 5 bits.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(x << (y & 0x1F))`

- **Signed Shift Right**
    - **Syntax:** `i32.shr_s`
    - **Description:** Arithmetic (sign-extending) shift right, masking the shift amount to its low 5 bits.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(x >>a (y & 0x1F))`

- **Unsigned Shift Right**
    - **Syntax:** `i32.shr_u`
    - **Description:** Logical (zero-filling) shift right, masking the shift amount to its low 5 bits.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(unsigned(x) >>l (y & 0x1F))`

### Comparison Instructions

Every comparison pushes `1` for true, `0` for false, and reads its two operands as (first-pushed, second-pushed) -- e.g. `i32.lt_s` computes first-pushed `<` second-pushed.

There is no dedicated zero test and no inequality test. `i32.const 0`, `i32.eq` serves as both: against a value it asks "is this zero", and against a comparison's own `0`/`1` result it negates it. That is how the comparisons this ISA does not have are written:

```assembly
    i32.gt_s                  ; a <= b  ==  not (a > b)
    i32.const 0
    i32.eq
```

The same idiom turns `br_if` (which branches on non-zero) into a branch-if-zero.

- **Equal**
    - **Syntax:** `i32.eq`
    - **Description:** Push `1` if the two values are equal, else `0`.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if x == y then 1 else 0)`

- **Signed Less Than**
    - **Syntax:** `i32.lt_s`
    - **Description:** Signed ordering comparison.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if signed(x) < signed(y) then 1 else 0)`

- **Signed Less Than or Equal**
    - **Syntax:** `i32.le_s`
    - **Description:** Signed ordering comparison.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if signed(x) <= signed(y) then 1 else 0)`

- **Signed Greater Than**
    - **Syntax:** `i32.gt_s`
    - **Description:** Signed ordering comparison.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if signed(x) > signed(y) then 1 else 0)`

- **Signed Greater Than or Equal**
    - **Syntax:** `i32.ge_s`
    - **Description:** Signed ordering comparison.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if signed(x) >= signed(y) then 1 else 0)`

- **Unsigned Less Than**
    - **Syntax:** `i32.lt_u`
    - **Description:** Unsigned ordering comparison.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if unsigned(x) < unsigned(y) then 1 else 0)`

- **Unsigned Less Than or Equal**
    - **Syntax:** `i32.le_u`
    - **Description:** Unsigned ordering comparison.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if unsigned(x) <= unsigned(y) then 1 else 0)`

- **Unsigned Greater Than**
    - **Syntax:** `i32.gt_u`
    - **Description:** Unsigned ordering comparison.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if unsigned(x) > unsigned(y) then 1 else 0)`

- **Unsigned Greater Than or Equal**
    - **Syntax:** `i32.ge_u`
    - **Description:** Unsigned ordering comparison.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if unsigned(x) >= unsigned(y) then 1 else 0)`

### Memory Instructions

Every load and store takes a static `<offset>` immediate, added to the popped address: the base is computed at run time and pushed, the displacement into it is written into the instruction. That makes indexing a struct field or a small array one instruction rather than three. The offset is unsigned and one byte, so `0`-`255`; omit it entirely when it is zero (`i32.load` means `i32.load 0`), and reach further by computing the address with `i32.add`, as every access had to before.

```assembly
    i32.const point       ; base address, computed at run time in general
    i32.load 4            ; point.y, say -- no i32.add needed
```

- **Load**
    - **Syntax:** `i32.load [<offset>]`
    - **Description:** Pop an address, push the 4-byte word stored at address plus offset.
    - **Operation:** `addr <- stack.pop(); stack.push(mem[addr + offset])`

- **Store**
    - **Syntax:** `i32.store [<offset>]`
    - **Description:** Pop a value, then an address (value on top, pushed last), and write the value's 4 bytes at address plus offset.
    - **Operation:** `value <- stack.pop(); addr <- stack.pop(); mem[addr + offset] <- value`

- **Load Byte Unsigned**
    - **Syntax:** `i32.load8_u [<offset>]`
    - **Description:** Pop an address, push the byte stored there, zero-extended to a full value.
    - **Operation:** `addr <- stack.pop(); stack.push(zeroExtend(mem8[addr + offset]))`

- **Load Byte Signed**
    - **Syntax:** `i32.load8_s [<offset>]`
    - **Description:** Pop an address, push the byte stored there, sign-extended to a full value.
    - **Operation:** `addr <- stack.pop(); stack.push(signExtend(mem8[addr + offset]))`

- **Store Byte**
    - **Syntax:** `i32.store8 [<offset>]`
    - **Description:** Pop a value, then an address, and write the value's low byte there.
    - **Operation:** `value <- stack.pop(); addr <- stack.pop(); mem8[addr + offset] <- value & 0xFF`

### Local Instructions

- **Local Get**
    - **Syntax:** `local.get <i>`
    - **Description:** Push local `i`'s value.
    - **Operation:** `stack.push(locals[i])`

- **Local Set**
    - **Syntax:** `local.set <i>`
    - **Description:** Pop the top of the stack into local `i`.
    - **Operation:** `locals[i] <- stack.pop()`

- **Local Tee**
    - **Syntax:** `local.tee <i>`
    - **Description:** Like Local Set, but also pushes the value back, leaving the stack depth unchanged.
    - **Operation:** `x <- stack.pop(); locals[i] <- x; stack.push(x)`

- **Reserve Locals**
    - **Syntax:** `locals.reserve <n>`
    - **Description:** Claim `n` more locals, zeroed, at the indices directly after the ones the frame already has. Belongs at a function's entry, before anything is pushed and outside any loop -- see "Locals" above for what happens otherwise. `n` is one byte, so `0`-`255`, and reserving past the operand stack's wall is an `operand stack overflow`. `return` reclaims them along with the parameters.
    - **Operation:** `sp <- sp - n*4; mem[sp .. sp + n*4 - 4] <- 0; localCount <- localCount + n`

### Control Flow Instructions

- **Block**
    - **Syntax:** `block`
    - **Description:** Open a scope that `br`/`br_if` can jump forward past, to its matching `end` -- see "Control Flow" above.
    - **Operation:** push a control record for this scope

- **Loop**
    - **Syntax:** `loop`
    - **Description:** Open a scope that `br`/`br_if` can jump backward into, to just after this instruction.
    - **Operation:** push a control record for this scope

- **If**
    - **Syntax:** `if`
    - **Description:** Pop a condition; fall into the following body if non-zero, otherwise skip to the matching `else` or past the matching `end`.
    - **Operation:** `c <- stack.pop(); if c != 0 then continue else pc <- matching else-or-end`

- **Else**
    - **Syntax:** `else`
    - **Description:** Marks the alternative body of an `if`. Reached only by the taken `if`-body falling through to it, at which point it unconditionally skips past the matching `end`.
    - **Operation:** `pc <- matching end + 1`

- **End**
    - **Syntax:** `end`
    - **Description:** Closes the innermost open `block`/`loop`/`if`.
    - **Operation:** pop the innermost control record

- **Branch**
    - **Syntax:** `br <depth>`
    - **Description:** Branch unconditionally to the scope `depth` levels out (`0` = innermost). Reaching a `loop` re-enters it and leaves it open; reaching a `block` exits it, closing everything nested between the branch and it.
    - **Operation:** unwind to the control record `depth` levels out; jump to its re-entry point (`loop`) or past its `end` (`block`)

- **Branch If**
    - **Syntax:** `br_if <depth>`
    - **Description:** Pop a condition; branch like `br` only if it is non-zero, otherwise fall through.
    - **Operation:** `c <- stack.pop(); if c != 0 then br(depth)`

### Function Instructions

- **Call**
    - **Syntax:** `call <paramCount>, <resultCount>`
    - **Description:** Pop a target address (an ordinary value, produced the same way any label reference is -- see "Functions" above); the `paramCount` values pushed just before it become the callee's parameters (its locals `0` downward); jump to the target, expecting `resultCount` values back. Neither count is checked against the callee's actual behavior.
    - **Operation:** `target <- stack.pop(); frameBase <- address of the paramCount values pushed before target; push a call record; pc <- target`

- **Return**
    - **Syntax:** `return`
    - **Description:** Close the nearest enclosing call (unwinding any `block`/`loop`/`if` still open along the way), popping and forwarding its declared result values to the caller.
    - **Operation:** `results <- stack.pop(resultCount); discard the callee's frame; stack.push(results); pc <- return address`

### Other

- **Initialize Stack Pointer**
    - **Syntax:** `sp.init`
    - **Description:** Pop an address and make it the root both stacks grow from: `sp`, `ctrlSp` and `frameBase` all move to it, so operands descend below it and control records ascend above it. This is how a program chooses the split between operand space and scope/call depth -- leave room *below* it for the deepest the operand stack will get, and room *above* it for the deepest nesting of `block`, `loop` and `call`. A push decrements before writing, so the address itself is never written to. Only meaningful before anything has been pushed (typically the first thing `_start` does), and nothing checks that either side has enough room.
    - **Operation:** `addr <- stack.pop(); sp <- addr; ctrlSp <- addr; frameBase <- addr - 4`

- **Unreachable**
    - **Syntax:** `unreachable`
    - **Description:** Trap. Marks a point the program believes it can never reach, and says so loudly if it does.
    - **Operation:** `trap`

- **Halt**
    - **Syntax:** `halt`
    - **Description:** Stop execution.
    - **Operation:** `stop`
