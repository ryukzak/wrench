# Wasm32 Instruction Set Architecture (ISA) Documentation

The Wasm32 ISA is a 32-bit stack-based instruction set designed for educational purposes. This documentation provides an overview of the instructions available in the Wasm32 ISA, their syntax, and their semantics, and -- in detail -- how control flow and the stack actually work underneath.

## Architecture Overview

The Wasm32 architecture is a 32-bit stack-based architecture. It features:

- One operand stack, shared by locals, operand values, and structured-control/call bookkeeping alike -- no general-purpose registers
- Structured control flow (`block`, `loop`, `if`/`else`) instead of arbitrary jumps
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

## The Stack

There is exactly one stack, and it holds three different kinds of things at once: a function's locals, its operand values, and bookkeeping records for open `block`/`loop`/`if`/call scopes. Nothing about the instruction encoding distinguishes them -- a local is just a word at a fixed address, an operand is just a word `sp` currently points above, and a control record is just a handful of words that happen to describe a scope. Reading the raw stack (as the `stack` state view does) shows all of it, interleaved in whatever order it was actually pushed.

Memory is split in half: the lower half holds code and `.data`; the upper half is where the stack lives, starting empty at program start. `sp` (the stack pointer) marks the top of everything currently on it and only ever moves by pushing or popping.

### Locals

A function's locals are exactly its parameters -- there is no instruction to declare more. They occupy a contiguous run of words starting at `frameBase`, one per index, addressed directly (`frameBase + index * 4`): whatever values the caller pushed just before `call` are, from the callee's perspective, its locals `0` through `paramCount - 1`. `frameBase` is not fixed; it moves to wherever the current function's locals happen to start, and is saved and restored across calls (see Functions, below).

A function that needs a scratch word beyond its own parameters -- a loop counter, say -- reaches for a `.data` cell instead: its address is fixed and global, so it survives a `block`/`loop`/call boundary exactly the way a local would (see "Why a value has to live in a local..." below), without needing a parameter to carry it in. `_start` in particular is never called, so it has no parameters at all -- every one of its example programs that needs to keep a value alive across a loop uses `.data` for it.

`local.get i` pushes local `i`'s value onto the operand stack; `local.set i` pops the top of the stack into local `i`; `local.tee i` does the same as `local.set` but pushes the value back afterward, leaving the stack depth unchanged.

### Operand values

Every arithmetic, comparison, and memory instruction reads its operands from the top of the stack and pushes its result back. For a two-operand instruction, the *second* operand pushed ends up on top and is popped first -- so `i32.const 10`, `i32.const 3`, `i32.sub` computes `10 - 3 = 7`, not `3 - 10`. This matters for every non-commutative binary instruction: subtraction, division, remainder, shifts, and every comparison read their two operands as (first-pushed, second-pushed) in that order.

`dup` duplicates whatever is currently on top, without needing a local: it is the minimal way to keep a value alive across an instruction that would otherwise consume it (checking a value and then using it again, for instance), as long as that value was pushed *after* whichever scopes are currently open -- see the next section for why that qualifier matters.

## Control Flow

`block`, `loop`, and `if` each open a scope; `end` closes the innermost one still open. All three scope-opening instructions are bare -- they take no operand of their own -- and `br`/`br_if` refer to a scope not by name but by *depth*: how many enclosing scopes out to reach, counting the innermost currently-open one as `0`.

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

### Why a value has to live in a local (or `.data`), not just on the stack, across a loop

`block` and `loop` push a bookkeeping record onto the very same stack values live on, the moment they are entered -- not somewhere separate. That record physically sits between whatever was pushed *before* the scope opened and whatever gets pushed *inside* it. An instruction that needs an operand only ever reaches upward from the current top of the stack; it has no way to reach past a record sitting in the way to a value that was pushed earlier.

Concretely: `i32.const 1`, then `loop`, then (inside the loop body) `i32.const 2`, `i32.add` -- the `i32.add` needs two operands, but only one value (`2`) has been pushed since the loop's record went on top of the `1`. It ends up reading part of the record's own bookkeeping words as if they were the second operand, silently producing garbage. The fix is to keep the value that needs to survive the scope boundary somewhere with a fixed address instead -- a local if the function has a spare parameter, a `.data` cell otherwise -- reading and writing it explicitly on each iteration, rather than leaving it sitting on the raw stack underneath the loop's record.

This is not a problem for a value pushed *inside* a scope and consumed later in the same or a nested scope -- `dup`, or any value produced during one iteration and consumed before the scope closes, works exactly as expected, because everything involved sits above the same record the whole time. The problem is specifically about reaching *underneath* a scope's own record to something that predates it.

### Control records, precisely

Each open `block`, `loop`, or call is a fixed-size record written directly into the stack at the moment it is entered, holding: a link back to whatever record was innermost before it, a tag saying which kind it is, and up to four more words of kind-specific payload (a loop's re-entry address, a block's or loop's own exit address, or a call's saved caller state). The most recently opened record's address is tracked separately; every other one is only reachable by following the chain of links outward from it.

A `br`/`br_if` naming depth `n` walks that chain outward `n` steps from the innermost record to find its target, then closes every record strictly between the branch and the target (regardless of that record's own kind), and finally either re-enters the target (if it is a loop) or closes it too (if it is a block). Closing a `block`/`loop` record removes exactly its own words from the stack, sliding whatever was pushed above it down to take their place -- so anything a scope's body pushed and never popped survives past the scope closing, right where it would have been if the record had never been there.

A `br`/`br_if`'s depth can never reach across a function call boundary: walking the chain outward stops with an error the moment it would have to pass through a call's own record. A structured branch is scoped to the function it appears in; leaving a function is `return`'s job, not a branch's.

## Functions

`i32.const some_function` produces a function's address exactly the way it produces any other label's address -- a function is just data once you have its address. `call` always pops its target off the top of the stack; there is no separate way to call a statically-known target that skips this. A call whose target is known when the program is written is simply `i32.const target` immediately followed by `call` -- and because the target is *just* a value, a program can equally well compute it (read it out of a local, say, if that local was set to a function's address earlier) and call through that instead. Both are the same instruction; only where the value came from differs.

`call` takes two more operands, written directly after it: how many values below the target address are this call's arguments, and how many values the call is expected to leave behind. Neither is looked up anywhere -- both are simply written at the call site, and nothing checks that a callee's actual behavior matches what a particular call site declared. Getting this wrong (calling with the wrong argument count, or a callee returning a different number of values than a caller expects) is not caught; it corrupts the stack silently, the same way any other malformed program does.

At the moment `call` executes, the values immediately below the popped target -- as many as its declared argument count -- become the callee's locals `0` upward, without being copied anywhere: `call` simply records where they already are as the new `frameBase` and jumps to the target. A record describing the call (the caller's own `frameBase`, its own local count, the return address, and the declared result count) is pushed immediately above those arguments, on the same stack every other record and every operand lives on.

`return` finds the nearest enclosing call (closing any `block`/`loop`/`if` still open along the way -- a `return` from inside a loop must still unwind it first), pops exactly as many values as that call declared it would return, discards the callee's entire frame -- its locals and its own record -- in a single step, and pushes the popped results back for the caller, at exactly the depth they would be at had the call consumed its arguments and produced its results in place. Falling off the end of a function's instructions without an explicit `return` is only correct if nothing is left open; an explicit `return` is needed for any exit before a function's last instruction.

## ISA Specific State Views

- `stack:dec`, `stack:hex` -- every word from the top of the stack down to the end of the active function's locals, most recent first. Includes any open control records' own words as raw data, exactly as they sit in memory -- this view has never distinguished "a value a program pushed" from "a word a record happens to occupy."
- `locals:dec`, `locals:hex` -- the active function's locals, in index order.
- `layout:dec`, `layout:hex` -- an annotated dump of the whole live stack, address-ascending, from the outermost active call down to the innermost/live one. Each frame gets a `#N <function> (pc=<pc>)` header (the function name is the nearest label at or before that frame's own pc), followed by one line per contiguous span of its own visible range: its locals (`@locals`), each of its own open control records decoded (`@loop(start=..,end=..)`, `@block(end=..)`, `@call(return=..,results=..,savedFrameBase=..,savedLocalCount=..)`), and its genuine operand values (`@stack`) -- the same information `stack`/`locals` give, but with the control-record noise labeled instead of left to look like data.

## Instructions

### Constants and Stack Manipulation

- **Constant**
    - **Syntax:** `i32.const <value>`
    - **Description:** Push an immediate value onto the stack.
    - **Operation:** `stack.push(<value>)`

- **Duplicate**
    - **Syntax:** `dup`
    - **Description:** Duplicate the top value on the stack, without needing a local -- valid only as long as the duplicated value was pushed after whichever scopes are currently open (see "Why a value has to live in a local, not just on the stack, across a loop" above).
    - **Operation:** `x <- stack.pop(); stack.push(x); stack.push(x)`

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
    - **Description:** Divide two signed values. Division by zero and signed overflow (dividing the most negative representable value by `-1`) trap.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(signed(x) / signed(y))`

- **Unsigned Divide**
    - **Syntax:** `i32.div_u`
    - **Description:** Divide two unsigned values. Division by zero traps.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(unsigned(x) / unsigned(y))`

- **Signed Remainder**
    - **Syntax:** `i32.rem_s`
    - **Description:** Compute the signed remainder. Division by zero and signed overflow trap.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(signed(x) % signed(y))`

- **Unsigned Remainder**
    - **Syntax:** `i32.rem_u`
    - **Description:** Compute the unsigned remainder. Division by zero traps.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(unsigned(x) % unsigned(y))`

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

- **Equal to Zero**
    - **Syntax:** `i32.eqz`
    - **Description:** Push `1` if the top of the stack is zero, else `0`.
    - **Operation:** `x <- stack.pop(); stack.push(if x == 0 then 1 else 0)`

- **Equal**
    - **Syntax:** `i32.eq`
    - **Description:** Push `1` if the two values are equal, else `0`.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if x == y then 1 else 0)`

- **Not Equal**
    - **Syntax:** `i32.ne`
    - **Description:** Push `1` if the two values are not equal, else `0`.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if x != y then 1 else 0)`

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

There is no base-plus-offset addressing mode; a program that wants one computes the sum explicitly with `i32.add` before the load or store.

- **Load**
    - **Syntax:** `i32.load`
    - **Description:** Pop an address, push the 4-byte word stored there.
    - **Operation:** `addr <- stack.pop(); stack.push(mem[addr])`

- **Store**
    - **Syntax:** `i32.store`
    - **Description:** Pop a value, then an address (value on top, pushed last), and write the value's 4 bytes there.
    - **Operation:** `value <- stack.pop(); addr <- stack.pop(); mem[addr] <- value`

- **Load Byte Unsigned**
    - **Syntax:** `i32.load8_u`
    - **Description:** Pop an address, push the byte stored there, zero-extended to a full value.
    - **Operation:** `addr <- stack.pop(); stack.push(zeroExtend(mem8[addr]))`

- **Load Byte Signed**
    - **Syntax:** `i32.load8_s`
    - **Description:** Pop an address, push the byte stored there, sign-extended to a full value.
    - **Operation:** `addr <- stack.pop(); stack.push(signExtend(mem8[addr]))`

- **Store Byte**
    - **Syntax:** `i32.store8`
    - **Description:** Pop a value, then an address, and write the value's low byte there.
    - **Operation:** `value <- stack.pop(); addr <- stack.pop(); mem8[addr] <- value & 0xFF`

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
    - **Description:** Pop a target address (an ordinary value, produced the same way any label reference is -- see "Functions" above); the `paramCount` values below it become the callee's parameters (its locals `0` upward); jump to the target, expecting `resultCount` values back. Neither count is checked against the callee's actual behavior.
    - **Operation:** `target <- stack.pop(); frameBase <- address of the paramCount values below target; push a call record; pc <- target`

- **Return**
    - **Syntax:** `return`
    - **Description:** Close the nearest enclosing call (unwinding any `block`/`loop`/`if` still open along the way), popping and forwarding its declared result values to the caller.
    - **Operation:** `results <- stack.pop(resultCount); discard the callee's frame; stack.push(results); pc <- return address`

### Other

- **Halt**
    - **Syntax:** `halt`
    - **Description:** Stop execution.
    - **Operation:** `stop`
