# Wasm32 Instruction Set Architecture (ISA) Documentation

Wasm32 is a 32-bit stack-based instruction set. Every value lives on one operand stack; there are no general-purpose registers. This document covers its structure, its instructions, and -- in detail -- how control flow and the stack actually work underneath.

Comments in Wasm32 assembly are denoted by the `;` character.

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

A function's locals -- both its parameters and any extra locals it declares -- occupy a contiguous run of words starting at `frameBase`, one per index, addressed directly (`frameBase + index * 4`). `frameBase` is not fixed; it moves to wherever the current function's locals happen to start, and is saved and restored across calls (see Functions, below).

Parameters become locals automatically: whatever values the caller pushed just before `call` are, from the callee's perspective, its locals `0` through `paramCount - 1`, addressed exactly the same way any other local is. A function that needs more locals than it has parameters declares them with `locals n`, which reserves and zero-fills `n` more words right after the existing ones -- so a function with two parameters and one extra local reaches the extra one as local index `2`. `locals` must be the first instruction in a function body if present, because it is the one instruction that changes where things after it in the frame live; running it later would relocate locals a preceding instruction had already addressed.

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

### Why a value has to live in a local, not just on the stack, across a loop

`block` and `loop` push a bookkeeping record onto the very same stack values live on, the moment they are entered -- not somewhere separate. That record physically sits between whatever was pushed *before* the scope opened and whatever gets pushed *inside* it. An instruction that needs an operand only ever reaches upward from the current top of the stack; it has no way to reach past a record sitting in the way to a value that was pushed earlier.

Concretely: `i32.const 1`, then `loop`, then (inside the loop body) `i32.const 2`, `i32.add` -- the `i32.add` needs two operands, but only one value (`2`) has been pushed since the loop's record went on top of the `1`. It ends up reading part of the record's own bookkeeping words as if they were the second operand, silently producing garbage. The fix is to keep the value that needs to survive the scope boundary in a local instead, reading and writing it explicitly with `local.get`/`local.set` on each iteration, rather than leaving it sitting on the raw stack underneath the loop's record.

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

## Instructions

### Constants

- **`i32.const <value>`** -- push an immediate value.

### Arithmetic

- **`i32.add`**, **`i32.sub`**, **`i32.mul`** -- pop two, push the result. `sub` computes first-pushed minus second-pushed.
- **`i32.div_s`**, **`i32.rem_s`** -- signed division/remainder. Division by zero traps; so does the one signed overflow case (dividing the most negative representable value by `-1`).
- **`i32.div_u`**, **`i32.rem_u`** -- unsigned division/remainder. Division by zero traps.

### Bitwise

- **`i32.and`**, **`i32.or`**, **`i32.xor`** -- bitwise AND/OR/XOR.
- **`i32.shl`** -- shift left. **`i32.shr_s`** -- arithmetic (sign-extending) shift right. **`i32.shr_u`** -- logical (zero-filling) shift right. All three mask the shift amount to its low 5 bits.

### Comparison

- **`i32.eqz`** -- push `1` if the top is zero, else `0`.
- **`i32.eq`**, **`i32.ne`** -- equality/inequality.
- **`i32.lt_s`**, **`i32.le_s`**, **`i32.gt_s`**, **`i32.ge_s`** -- signed ordering.
- **`i32.lt_u`**, **`i32.le_u`**, **`i32.gt_u`**, **`i32.ge_u`** -- unsigned ordering.

Every comparison pushes `1` for true, `0` for false, and reads its two operands as (first-pushed, second-pushed) -- `i32.lt_s` computes first-pushed `<` second-pushed.

### Memory

- **`i32.load`** -- pop an address, push the 4-byte word stored there.
- **`i32.store`** -- pop a value, then an address (value on top, pushed last), and write the value's 4 bytes there.
- **`i32.load8_u`**, **`i32.load8_s`** -- like `i32.load`, but read one byte, zero- or sign-extending it to a full value.
- **`i32.store8`** -- like `i32.store`, but writes only the value's low byte.

There is no base-plus-offset addressing mode; a program that wants it computes the sum explicitly with `i32.add` before the load or store.

### Stack manipulation

- **`dup`** -- duplicate the top value.

### Local Instructions

- **`locals <n>`** -- reserve and zero-fill `n` extra locals, right after any parameters. Must be the first instruction of a function body if present.
- **`local.get <i>`** -- push local `i`.
- **`local.set <i>`** -- pop into local `i`.
- **`local.tee <i>`** -- pop into local `i`, then push the same value back.

### Control flow

- **`block`** -- open a scope that `br`/`br_if` can jump forward past (to its matching `end`).
- **`loop`** -- open a scope that `br`/`br_if` can jump backward into (to just after the `loop` instruction itself).
- **`if`** -- pop a condition; fall into the following body if non-zero, otherwise skip to the matching `else` or past the matching `end`.
- **`else`** -- marks the alternative body of an `if`; reached only by the taken body falling through, at which point it skips past the matching `end`.
- **`end`** -- closes the innermost open `block`/`loop`/`if`.
- **`br <depth>`** -- branch unconditionally to the scope `depth` levels out (`0` = innermost).
- **`br_if <depth>`** -- pop a condition; branch like `br` only if it is non-zero.

### Function Instructions

- **`call <paramCount>, <resultCount>`** -- pop a target address; the `paramCount` values below it become the callee's parameters; jump to the target, expecting `resultCount` values back.
- **`return`** -- close the nearest enclosing call, popping and forwarding its declared result values to the caller.

### Other

- **`halt`** -- stop execution.
