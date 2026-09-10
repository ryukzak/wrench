# Wasm32 Instruction Set Architecture (ISA) Documentation

The Wasm32 ISA is a 32-bit stack-based instruction set inspired by WebAssembly. This documentation provides an overview of the instructions available in the Wasm32 ISA, their syntax, and their semantics.

## Architecture Overview

The Wasm32 architecture is a 32-bit stack-based architecture inspired by WebAssembly. It features:

- An operand stack for `i32` values
- Function frames with parameters, locals, and return values
- Structured control flow with `block`, `loop`, `if`, `else`, `end`, `br`, and `br_if`
- Linear memory and memory-mapped I/O
- Traps for invalid execution, such as stack underflow, bad memory access, division by zero, and `unreachable`

This stack-based architecture offers a compact structured-control-flow model, making it useful for studying function calls, local variables, loops, and low-level memory access within the Wrench assembly model.

Comments in Wasm32 assembly code are denoted by the `;` character.

Inspired by [WebAssembly](https://webassembly.github.io/spec/core/)

## Program Structure

Wasm32 programs are written in Wrench assembly syntax and use the normal Wrench `.data` and `.text` sections. Execution starts at the `_start` label. Function entry points are ordinary assembler labels followed by `.func`, and function bodies end with `.endfunc`.

```assembly
    .text

_start:
    .func locals $result
    i32.const 0x80
    i32.load
    call double
    local.set $result
    i32.const 0x84
    local.get $result
    i32.store
    halt
    .endfunc

double:
    .func params $x result i32
    local.get $x
    i32.const 2
    i32.mul
    return
    .endfunc
```

## ISA Specific State Views

- `stack:dec`, `stack:hex` -- operand stack, top first.
- `locals:dec`, `locals:hex` -- locals of the current function frame.
- `local:<name>:dec`, `local:<name>:hex` -- one local from the current function frame.
- `frames` -- number of active function frames.
- `frame`, `frame:<n>`, `frame:<n>:dec`, `frame:<n>:hex` -- one line per active call frame, innermost first (default `<n>` is 1, default format is `dec`): `#<i> <name>: locals=[...] stack=[...]`. Each frame's `stack` is that frame's own operand region, using the same raw-dump convention as `stack:*` above -- for the innermost frame this is everything above its locals up to the live `sp`; for an outer frame waiting on a call, it's everything up to the point where it pushed the callee's arguments (frozen while the callee runs). Requesting more frames than are active just stops early instead of erroring.
- `ctrl` -- active structured control ids, innermost first.

### Runtime statistics

The simulator tracks the high-water mark (deepest level reached) of the Wasm32 runtime stacks and exposes them through summary view variables. They are run-totals, so use them with `slice: last`.

- `wasm32:operand-stack-max` -- maximum operand stack depth reached during execution.
- `wasm32:frames-max` -- maximum active function frame count reached during execution.
- `wasm32:control-stack-max` -- maximum active structured control label count reached during execution.

All three lines are also emitted together by the generic `{isa-specific}` summary block, which lets a single report template stay uniform across ISAs.

## Functions and Locals

Function metadata is declared by `.func`, which is a Wrench source directive that describes the next function body. During translation Wrench records it in a function table and does not emit an executable instruction cell for it. `.endfunc` lowers to the same runtime instruction as `return`.

```assembly
_start:
    .func
    i32.const 5
    call factorial
    halt
    .endfunc

factorial:
    .func params $n result i32 locals $acc
    i32.const 1
    local.set $acc
    ; ...
    local.get $acc
    .endfunc
```

- `.func` declares a function with no parameters, no locals, and no return values.
- `.func locals $x $y` declares local variables initialized to zero.
- `.func params $n result i32 locals $acc` declares one parameter, one return value, and one extra local.
- `func 1, 1, 1` is also accepted as a compact numeric form: one parameter, one extra local, one result.

The named form is easier to read in examples and should be preferred in hand-written programs. The numeric form is useful for generated code:

```assembly
sum:
    .func params $x $y result i32
    local.get $x
    local.get $y
    i32.add
    .endfunc

sum_generated:
    func 2, 0, 1
    local.get 0
    local.get 1
    i32.add
    .endfunc
```

In `func 2, 0, 1`, locals `0` and `1` are parameters. The second number is the number of additional zero-initialized locals, so `func 1, 1, 1` creates parameter `0`, extra local `1`, and one return value.

Parameters are popped from the operand stack and bound to local names in declaration order. Return values are popped from the operand stack before the current function frame is removed, then pushed back for the caller. `.endfunc` returns from the current function; `return` does the same explicitly and is only needed for early returns before the function end. Returning from `_start` stops the machine.

## Operand Stack

Most instructions pop their operands from the stack and push the result back.

For binary operations, the right operand is popped first. This Wasm32 code leaves `7` on the stack:

```assembly
i32.const 10
i32.const 3
i32.sub
```

## Memory and I/O

Wasm32 uses the same byte-addressed memory model as the other Wrench ISAs. Memory addresses are `i32` values. `i32.load` and `i32.store` read and write four bytes, while `i32.load8_u` and `i32.store8` read and write one byte.

```assembly
    .data

value:           .word  42
byte_value:      .byte  65

    .text

_start:
    .func locals $tmp
    i32.const value
    i32.load
    local.set $tmp

    i32.const value
    local.get $tmp
    i32.const 1
    i32.add
    i32.store

    i32.const byte_value
    i32.load8_u
    drop
    halt
    .endfunc
```

Memory-mapped I/O is configured through the normal Wrench configuration file. For example, the existing examples use address `0x80` for input and address `0x84` for output:

```assembly
_start:
    .func
    i32.const 0x84
    i32.const 0x80
    i32.load
    i32.store
    halt
    .endfunc
```

With a configuration that maps `0x80` to input and `0x84` to output, this reads one 32-bit value from input and writes it to output.

## Structured Control Flow

Wasm32 uses structured control instructions instead of arbitrary jumps. `block`, `loop`, and `if` introduce labeled control regions. `br <label>` branches to a region unconditionally, and `br_if <label>` branches only when the popped condition is non-zero.

An `if` executes its body when the condition is non-zero:

```assembly
    local.get $n
    i32.const 0
    i32.lt_s
    if negative
        i32.const -1
        return
    end
```

Use `else` for the alternative branch:

```assembly
    local.get $flag
    if choose
        i32.const 1
    else
        i32.const 2
    end
```

A loop is normally wrapped in an outer block. Branching to the loop label continues the loop, while branching to the block label exits it:

```assembly
    block done
        loop again
            local.get $n
            i32.const 1
            i32.le_s
            br_if done

            local.get $n
            i32.const 1
            i32.sub
            local.set $n

            br again
        end
    end
```

Control labels are ordinary label tokens. The examples use bare labels to distinguish them from locals, while the parser still accepts labels such as `$loop` for compatibility with existing sources.

## Execution Model

Structured control and calls are tracked with a small set of registers and one linear stack, sharing memory with the operand stack:

- `sp` -- top of the stack (locals, operand values, and control records all live here)
- `frame_base` -- start of the active function's locals
- `ctrl_top` -- address of the innermost open `block`/`loop`/`if`/call record

No instruction encodes a jump address. `block`, `loop`, `if`, and `call` each push a small record describing where control goes on exit; `br`, `br_if`, `end`, and `return` resolve their target by reading that record chain, never an encoded offset.

### Control Records

Every open `block`, `loop`, `if`, or function call is represented the same way: one fixed-shape record, pushed onto the stack alongside locals and operand values, and linked into a chain through `ctrl_top`:

Records live in the same stack as everything else, so this is what it looks like as actual stack contents (growing upward), for a function that has one open `block` and, inside it, one open `loop`:

```
addr   contents
----   -------------------------------------
 108   [ Loop  record: link = 101, ... ]      <- ctrl_top
 107   ...loop body's own operand values...
 101   [ Block record: link = 100, ... ]
 100   [ Call  record: link = NULL, ... ]
  99   ...locals...
```

`ctrl_top` only ever points at the topmost record (108 here). Each record's `link` field skips directly to the *previous* record's address, regardless of how much ordinary stack data (locals, operand values) sits in between -- the `Loop` record's `link = 101` jumps straight past everything at 107 to the `Block` record, and `Block`'s `link = 100` reaches the `Call` record at the bottom, whose `link = NULL` marks the end of the chain.

Each record's `link` field points at the one before it -- that chain *is* the control stack; there is no separate structure for it.

A record holds:

`Block` and `If` only ever need `label`; `Loop` additionally needs `startPc`; `Call` needs neither, but needs `savedFrameBase`/`entryPc` instead -- three genuinely different shapes, each only ever used by the `kind` it belongs to:

```c
struct ControlRecord {
    ControlRecord *link;        // previous record in the chain (NULL at the bottom)
    Kind           kind;        // Block, Loop, If, or Call
    Addr           endPc;       // normal-exit target; for Call, the return address
    int            resultCount; // stack values kept when the record closes
    union {
        struct { int label; }                         block_if; // Block, If: branch target id
        struct { int label; Addr startPc; }            loop;    // Loop: branch target id, loop re-entry point
        struct { Addr savedFrameBase; Addr entryPc; }  call;    // Call: caller's frame_base to restore, and this
                                                                 // call's own FuncEnter address (report views
                                                                 // only -- see Function Call and Return)
        struct {}                                      unused;  // never actually selected -- `kind` always picks
                                                                 // one of the three above; a plain union like this
                                                                 // one carries no tag of its own to enforce that
    };
};
```

Two operations cover everything that touches a record:

- **enter** -- push a new record at the top of the stack and point `ctrl_top` at it. Used by `block`, `loop`, `if` (once a branch is chosen to run), and `call`.
- **collapse** -- the one operation behind `end`, a taken `br`/`br_if`, and `return`. Take the record's `resultCount` top-of-stack values, discard the record and everything pushed above it (or, if the record is being kept open, everything above it but not the record itself), write those values back, and continue either at `loop.startPc` (kept open -- a loop repeating) or at `endPc` (closed -- every other case), restoring `ctrl_top` from `link` in the closed case. Closing a call record additionally restores `frame_base` from `call.savedFrameBase`.

The sections below are all instances of these two operations: `if`/`else`/`end` decide *when* to enter and which record to close; `block`/`loop`/`br`/`br_if` decide *which* record a branch resolves to; `call`/`return` add the locals/`frame_base` bookkeeping on top.

### If, Else, and End

`if`, `else`, and `end` are matched structurally in the bytecode -- no address for "the else" or "the end" is stored anywhere. `if` locates them itself by scanning forward, counting nested `block`/`loop`/`if` opens against `end` closes, until it finds the matching `else` (if present) and `end`.

`if <label>` pops the condition. When a branch is actually entered (either branch, as long as one exists to run), `if` also pushes a control record tagged `<label>` so `br`/`br_if <label>` can target it later (see [Block and Loop](#block-and-loop)):

```
bytecode, one copy, in order:

    if L
    ...then-branch...
    else                ; only reached by falling through the then-branch
    ...else-branch...
    end

condition != 0:                             condition == 0 (else present):
    push a record, enter right after `if`   push a record, jump to right after `else`
    run the then-branch                     run the else-branch
    fall into `else` -> jump to `end`       fall into `end`

condition == 0, no else:
    push nothing -- the construct is never entered,
    continue directly after `end`
```

Reaching `else` always means the condition was non-zero -- the else-branch must not also run, so `else` unconditionally jumps to right after the matching `end`. The "condition == 0, no else" path is not a branch into a live scope: nothing was pushed, so there's nothing to close either -- it behaves as if the whole construct were absent. `end` closes whichever record is on top of the control chain: it pops that record and continues right after the matching `end`. `block`/`loop`/`if` carry no declared result type in Wasm32, so closing one never moves any stack values -- it is exactly that pop, nothing more. (A function's `return`/`.endfunc` is the one case that *does* carry a result count, covered in [Function Call and Return](#function-call-and-return).)

### Block and Loop

`block` and `loop` never pop anything -- unlike `if`, entering one is unconditional. Each pushes a control record tagged with its label; the only difference between them is what a `br`/`br_if` targeting that record does:

- targeting a `block` (or an `if`): discard the record, continue right after its `end`
- targeting a `loop`: keep the record, jump back to right after `loop` itself

That asymmetry is the entire looping mechanism in Wasm32 -- a `loop` by itself does not repeat anything; it only becomes a loop because something inside it branches back with `br`/`br_if`.

`br <label>`/`br_if <label>` search the control chain from `ctrl_top` outward for the record tagged `<label>`, then unwind to it one record at a time: each record strictly above the target is closed individually (as an ordinary, unconditional close) before the target itself closes. Closing a record one at a time, rather than discarding the whole span above the target in one step, is what lets each intervening scope's own body keep whatever it left on the stack -- exactly as it would closing on its own -- instead of that being silently swept away by the branch skipping past it. The cost is proportional to how many scopes are skipped, not the constant-time pointer move a single splice would be; see [Control Records](#control-records) for what closing one record does.

Using the loop from [Structured Control Flow](#structured-control-flow):

```assembly
    block done
        loop again
            ...
            br_if done   ; A
            ...
            br again     ; B
        end
    end
```

At both `A` and `B` the control chain holds two open records:

```
ctrl_top -> [ loop again ]
            [ block done ]
```

- At `A` (`br_if done`, taken): the search skips past `loop again` and matches `block done` -- both records are discarded (the `loop` one only because it happened to sit above the match), and execution continues right after the outer `end`.
- At `B` (`br again`): the search matches `loop again` immediately, at the top -- it is kept, and execution jumps back to right after `loop again`.

### Function Call and Return

A function call is just another kind of record on the chain -- `Call` -- with its own way of entering (`call`) and its own way of being found and closed (`return`, or simply falling off the end of the function).

Function metadata is not a separate table -- it is a `FuncEnter` instruction placed right before the function's body in code, holding paramCount, declaredLocalCount, and resultCount as its immediate fields. It is never reached by ordinary fallthrough (nothing falls into a function entry except via `call`, which always jumps past it); `call` reads it the same way it reads any other instruction, via `readInstruction`, rather than through a separate lookup:

```
target:   FuncEnter paramCount declaredLocalCount resultCount
target+k: ...function body...        ; k = byteSize(FuncEnter)
```

`call target` reads that header, then enters a `Call` record:

```
frame_base' <- sp - paramCount        ; arguments are already on the stack -- alias them in, no copy
push declaredLocalCount zero words    ; the rest of the locals, zero-initialized
enter Call record:
    endPc               <- pc right after this `call`   ; the return address
    resultCount         <- resultCount (read from the header)
    call.savedFrameBase <- frame_base                    ; caller's, to restore later
    call.entryPc        <- target                        ; this call's own FuncEnter address (report views only)
frame_base <- frame_base'
pc <- target + byteSize(FuncEnter)                       ; skip the header, start the body
```

The caller's pushed arguments never move -- the same aliasing trick from [Control Records](#control-records), just applied to locals instead of a branch target: whatever already sat on top of the stack becomes `locals[0..paramCount-1]` simply because `frame_base'` starts there.

`return` finds the nearest enclosing `Call` record and collapses it -- the same search-then-collapse `br`/`br_if` use, just searching by `kind == Call` instead of by label:

```
return:
    r <- walk the chain from ctrl_top via `link` until kind == Call
    collapse(r, keepOpen = false)
```

Because unwinding closes one record at a time (see [Block and Loop](#block-and-loop)), a `return` issued from inside an open `block`/`loop`/`if` closes those scopes too on its way out, each keeping whatever its own body left, just as it would closing normally. Falling off the end of a function is not a special case either: with nothing else open, `ctrl_top` already *is* the `Call` record, so the ordinary `end` operation, `collapse(ctrl_top, keepOpen=false)`, has exactly the effect of a `return`. That is why `.endfunc` lowers to the same instruction as `return`.

Using `sum` from [Functions and Locals](#functions-and-locals), called as `i32.const 3`, `i32.const 4`, `call sum`:

```
before call:  frame_base = F, sp = S           ; 3 and 4 already sit at S-2, S-1

call sum:
    frame_base' = S - 2                         ; aliases the 3 and 4 in place
    (sum declares no extra locals -- nothing to zero-fill)
    enter Call record: endPc = return address, resultCount = 1, call.savedFrameBase = F
    frame_base = S - 2, pc = sum's body

sum's body pushes 3 + 4 = 7, then falls off the end:
    collapse(ctrl_top, keepOpen = false):
        results = [7]
        reclaim locals, scratch, and the record itself in one step
        frame_base = F                          ; restored
        pc = return address
```

From the caller's side this looks identical to any other instruction: two values consumed, one produced, nothing about the call boundary is visible in the stack's shape.

## Instructions

Instruction sizes are implementation sizes used by the Wrench translator and trace:

- 5 bytes: `i32.const`, `call`
- 4 bytes: `.func`'s embedded `FuncEnter` header (see "Function Call and Return")
- 2 bytes: `local.get`, `local.set`, `local.tee`, `block`, `loop`, `if`, `br`, `br_if`
- 1 byte: all other instructions

`.func` and `.endfunc` are assembly directives, not `Isa` instructions in their own right: `.func` lowers to a `FuncEnter` header, and `.endfunc` lowers to a plain one-byte `return` (the function's implicit return, same as falling off the end in real Wasm).

### Constants and Stack Operations

- **I32 Const**
    - **Syntax:** `i32.const <value>`
    - **Description:** Push an immediate `i32` value onto the operand stack.
    - **Operation:** `stack.push(<value>)`

- **Drop**
    - **Syntax:** `drop`
    - **Description:** Remove the top value from the operand stack.
    - **Operation:** `stack.pop()`

- **Select**
    - **Syntax:** `select`
    - **Description:** Select one of two values based on a non-zero condition.
    - **Operation:** `condition <- stack.pop(); falseValue <- stack.pop(); trueValue <- stack.pop(); stack.push(if condition != 0 then trueValue else falseValue)`

### Local Instructions

- **Local Get**
    - **Syntax:** `local.get <name>`
    - **Description:** Push the value of a local variable onto the operand stack.
    - **Operation:** `stack.push(local[<name>])`

- **Local Set**
    - **Syntax:** `local.set <name>`
    - **Description:** Store the top value of the operand stack into a local variable.
    - **Operation:** `local[<name>] <- stack.pop()`

- **Local Tee**
    - **Syntax:** `local.tee <name>`
    - **Description:** Store the top value of the operand stack into a local variable and keep that value on the stack.
    - **Operation:** `value <- stack.pop(); local[<name>] <- value; stack.push(value)`

### Arithmetic Instructions

- **Add**
    - **Syntax:** `i32.add`
    - **Description:** Add two `i32` values.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(x + y)`

- **Subtract**
    - **Syntax:** `i32.sub`
    - **Description:** Subtract the second operand from the first operand.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(x - y)`

- **Multiply**
    - **Syntax:** `i32.mul`
    - **Description:** Multiply two `i32` values.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(x * y)`

- **Signed Divide**
    - **Syntax:** `i32.div_s`
    - **Description:** Divide two signed `i32` values. Division by zero and signed overflow trap.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(signed(x) / signed(y))`

- **Unsigned Divide**
    - **Syntax:** `i32.div_u`
    - **Description:** Divide two unsigned `i32` values. Division by zero traps.
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
    - **Description:** Perform a bitwise AND on two `i32` values.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(x & y)`

- **Or**
    - **Syntax:** `i32.or`
    - **Description:** Perform a bitwise OR on two `i32` values.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(x | y)`

- **Exclusive Or**
    - **Syntax:** `i32.xor`
    - **Description:** Perform a bitwise XOR on two `i32` values.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(x ^ y)`

- **Shift Left**
    - **Syntax:** `i32.shl`
    - **Description:** Shift the first operand left by the lower 5 bits of the second operand.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(x << (y & 0x1F))`

- **Signed Shift Right**
    - **Syntax:** `i32.shr_s`
    - **Description:** Shift the first operand right by the lower 5 bits of the second operand, preserving the sign.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(x >> (y & 0x1F))`

- **Unsigned Shift Right**
    - **Syntax:** `i32.shr_u`
    - **Description:** Shift the first operand right by the lower 5 bits of the second operand, filling with zero bits.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(unsigned(x) >> (y & 0x1F))`

### Comparison Instructions

- **Equal to Zero**
    - **Syntax:** `i32.eqz`
    - **Description:** Test whether the top value is zero.
    - **Operation:** `stack.push(if stack.pop() == 0 then 1 else 0)`

- **Equal**
    - **Syntax:** `i32.eq`
    - **Description:** Test whether two values are equal.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if x == y then 1 else 0)`

- **Not Equal**
    - **Syntax:** `i32.ne`
    - **Description:** Test whether two values are not equal.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if x != y then 1 else 0)`

- **Signed Less Than**
    - **Syntax:** `i32.lt_s`
    - **Description:** Compare two values as signed `i32` values.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if signed(x) < signed(y) then 1 else 0)`

- **Signed Less Than or Equal**
    - **Syntax:** `i32.le_s`
    - **Description:** Compare two values as signed `i32` values.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if signed(x) <= signed(y) then 1 else 0)`

- **Signed Greater Than**
    - **Syntax:** `i32.gt_s`
    - **Description:** Compare two values as signed `i32` values.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if signed(x) > signed(y) then 1 else 0)`

- **Signed Greater Than or Equal**
    - **Syntax:** `i32.ge_s`
    - **Description:** Compare two values as signed `i32` values.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if signed(x) >= signed(y) then 1 else 0)`

- **Unsigned Less Than**
    - **Syntax:** `i32.lt_u`
    - **Description:** Compare two values as unsigned `i32` values.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if unsigned(x) < unsigned(y) then 1 else 0)`

- **Unsigned Less Than or Equal**
    - **Syntax:** `i32.le_u`
    - **Description:** Compare two values as unsigned `i32` values.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if unsigned(x) <= unsigned(y) then 1 else 0)`

- **Unsigned Greater Than**
    - **Syntax:** `i32.gt_u`
    - **Description:** Compare two values as unsigned `i32` values.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if unsigned(x) > unsigned(y) then 1 else 0)`

- **Unsigned Greater Than or Equal**
    - **Syntax:** `i32.ge_u`
    - **Description:** Compare two values as unsigned `i32` values.
    - **Operation:** `y <- stack.pop(); x <- stack.pop(); stack.push(if unsigned(x) >= unsigned(y) then 1 else 0)`

### Memory Instructions

- **Load Word**
    - **Syntax:** `i32.load`
    - **Description:** Load a 32-bit word from memory.
    - **Operation:** `stack.push(mem[stack.pop()])`

- **Store Word**
    - **Syntax:** `i32.store`
    - **Description:** Store a 32-bit word into memory.
    - **Operation:** `value <- stack.pop(); address <- stack.pop(); mem[address] <- value`

- **Load Signed Byte**
    - **Syntax:** `i32.load8_s`
    - **Description:** Load a byte from memory and sign-extend it to `i32`.
    - **Operation:** `stack.push(signext(mem[stack.pop()][7:0]))`

- **Load Unsigned Byte**
    - **Syntax:** `i32.load8_u`
    - **Description:** Load a byte from memory and zero-extend it to `i32`.
    - **Operation:** `stack.push(zeroext(mem[stack.pop()][7:0]))`

- **Store Byte**
    - **Syntax:** `i32.store8`
    - **Description:** Store the lower 8 bits of a value into memory.
    - **Operation:** `value <- stack.pop(); address <- stack.pop(); mem[address] <- value & 0xFF`

### Control Flow Instructions

- **Block**
    - **Syntax:** `block <label>`
    - **Description:** Start a structured block. Branching to the block label exits the block.
    - **Operation:** `control.push(block <label>)`

- **Loop**
    - **Syntax:** `loop <label>`
    - **Description:** Start a structured loop. Branching to the loop label continues at the start of the loop body.
    - **Operation:** `control.push(loop <label>)`

- **If**
    - **Syntax:** `if <label>`
    - **Description:** Start a conditional structured block.
    - **Operation:** `if stack.pop() != 0 then enter then-branch else enter else-branch or continue after end`

- **Else**
    - **Syntax:** `else`
    - **Description:** Separate the main branch of an `if` from its alternative branch. Executing `else` skips the alternative branch.
    - **Operation:** `pc <- after matching end`

- **End**
    - **Syntax:** `end`
    - **Description:** End the current `block`, `loop`, or `if`.
    - **Operation:** `control.pop()`

- **Branch**
    - **Syntax:** `br <label>`
    - **Description:** Branch to an active structured control label.
    - **Operation:** `pc <- target(<label>)`

- **Branch If**
    - **Syntax:** `br_if <label>`
    - **Description:** Branch to an active structured control label when the condition is non-zero.
    - **Operation:** `if stack.pop() != 0 then pc <- target(<label>)`

- **Call**
    - **Syntax:** `call <label>`
    - **Description:** Call a function at the specified label.
    - **Operation:** `call <label>`

- **Return**
    - **Syntax:** `return`
    - **Description:** Return from the current function.
    - **Operation:** `return`

- **Halt**
    - **Syntax:** `halt`
    - **Description:** Stop execution.
    - **Operation:** `halt`

- **Unreachable**
    - **Syntax:** `unreachable`
    - **Description:** Raise an execution trap.
    - **Operation:** `trap`

- **No Operation**
    - **Syntax:** `nop`
    - **Description:** Do nothing.
    - **Operation:** `pc <- pc + 1`

Branching to a `block` or `if` label exits that construct and continues after its `end`. Branching to a `loop` label jumps back to the start of the loop body. `br_if` pops a condition and branches when it is non-zero. `call` target labels must point to a function label with `.func` metadata.
