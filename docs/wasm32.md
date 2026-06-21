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
- `ctrl` -- active structured control labels, innermost first.

### Runtime statistics

The simulator tracks the high-water mark (deepest level reached) of the Wasm32 runtime stacks and exposes them through summary view variables. They are run-totals, so use them with `slice: last`.

- `wasm32:operand-stack-max` -- maximum operand stack depth reached during execution.
- `wasm32:frames-max` -- maximum active function frame count reached during execution.
- `wasm32:control-stack-max` -- maximum active structured control label count reached during execution.

All three lines are also emitted together by the generic `{isa-specific}` summary block, which lets a single report template stay uniform across ISAs.

## Functions and Locals

Function metadata is declared by `.func`, which is a Wrench directive that describes the next function body. It is not executed as a normal stack instruction by the program author; it marks how Wrench should enter the function, bind parameters, allocate locals, and collect return values.

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
    return
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
    return
    .endfunc

sum_generated:
    func 2, 0, 1
    local.get 0
    local.get 1
    i32.add
    return
    .endfunc
```

In `func 2, 0, 1`, locals `0` and `1` are parameters. The second number is the number of additional zero-initialized locals, so `func 1, 1, 1` creates parameter `0`, extra local `1`, and one return value.

Parameters are popped from the operand stack and bound to local names in declaration order. Return values are popped from the operand stack before the current function frame is removed, then pushed back for the caller. `.endfunc` returns from the current function; `return` does the same explicitly. Returning from `_start` stops the machine.

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
    if $negative
        i32.const -1
        return
    end
```

Use `else` for the alternative branch:

```assembly
    local.get $flag
    if $choose
        i32.const 1
    else
        i32.const 2
    end
```

A loop is normally wrapped in an outer block. Branching to the loop label continues the loop, while branching to the block label exits it:

```assembly
    block $done
        loop $loop
            local.get $n
            i32.const 1
            i32.le_s
            br_if $done

            local.get $n
            i32.const 1
            i32.sub
            local.set $n

            br $loop
        end
    end
```

## Instructions

Instruction sizes are implementation sizes used by the Wrench translator and trace:

- 5 bytes: `i32.const`, `call`
- 4 bytes: `.func`
- 2 bytes: `local.get`, `local.set`, `local.tee`, `block`, `loop`, `if`, `br`, `br_if`
- 1 byte: all other instructions

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

Branching to a `block` or `if` label exits that construct and continues after its `end`. Branching to a `loop` label jumps back to the start of the loop body. `br_if` pops a condition and branches when it is non-zero. `call` target labels must point to a `.func` instruction.
