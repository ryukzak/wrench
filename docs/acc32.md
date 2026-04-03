# Acc32 Instruction Set Architecture (ISA) Documentation

The Acc32 ISA is a simple accumulator-based instruction set designed for educational purposes. This documentation provides an overview of the instructions available in the Acc32 ISA, their syntax, and their semantics.

## Architecture Overview

The Acc32 architecture is a 32-bit accumulator-based architecture. It features:

- A single general-purpose accumulator register (Acc)
- Two status flags: overflow (V) and carry (C)
- Memory-mapped I/O
- Simple instruction set focused on accumulator operations

This architecture is excellent for beginners to understand the basics of computer architecture without the complexity of multiple registers.

Comments in Acc32 assembly code are denoted by the `;` character.

## ISA Specific State Views

- `Acc:dec`, `Acc:hex` -- `Acc` register.
- `V` -- Overflow flag.
- `C` -- Carry flag.

## Instructions

## Opcode
- **Size:** 1 byte

### Instructions using Absolute Addressing
- **Operand size:** 4 bytes
- **Total Instruction Size:** 5 bytes
- **Used by:**
  - Control flow
  - Load (Immediate / Indirect / Address)
  - Store (Immediate / Indirect / Address)

### Instructions using Relative Addressing
- **Operand size:** 2 bytes
- **Total Instruction Size:** 3 bytes
- **Used by:**
  - All other instructions


### Data Movement Instructions

- **Load Immediate**
    - **Syntax:** `load_imm <value>`
    - **Description:** Load an immediate value into the accumulator.
    - **Operation:** `acc <- <value>`

- **Load**
    - **Syntax:** `load <offset>`
    - **Description:** Load a value from a relative address into the accumulator.
    - **Operation:** `acc <- mem[pc + <offset>]`

- **Store**
    - **Syntax:** `store <offset>`
    - **Description:** Store the accumulator value into a relative address.
    - **Operation:** `mem[pc + <offset>] <- acc`

- **Load Address**
    - **Syntax:** `load_addr <address>`
    - **Description:** Load a value from a specific address into the accumulator.
    - **Operation:** `acc <- mem[<address>]`

- **Store Address**
    - **Syntax:** `store_addr <address>`
    - **Description:** Store the accumulator value into a specific address.
    - **Operation:** `mem[<address>] <- acc`

- **Load Acc**
    - **Syntax:** `load_acc`
    - **Description:** Load a value from an address in acc into the accumulator.
    - **Operation:** `acc <- mem[acc]`

- **Store Indirect**
    - **Syntax:** `store_ind <address>`
    - **Description:** Store the accumulator value into an indirect address.
    - **Operation:** `mem[mem[<address>]] <- acc`

### Arithmetic Instructions

- **Add**
    - **Syntax:** `add <offset>`
    - **Description:** Add a value from a specific address to the accumulator.
    - **Operation:** `acc <- acc + mem[pc + <offset>]` and set `C` and `V` flags.

- **Subtract**
    - **Syntax:** `sub <offset>`
    - **Description:** Subtract a value from a specific address from the accumulator.
    - **Operation:** `acc <- acc - mem[pc + <offset>]` and set `V` flags.

- **Multiply**
    - **Syntax:** `mul <offset>`
    - **Description:** Multiply the accumulator by a value from a specific address.
    - **Operation:** `acc <- acc * mem[pc + <offset>]` and set `V` flags.

- **Divide**
    - **Syntax:** `div <offset>`
    - **Description:** Divide the accumulator by a value from a specific address.
    - **Operation:** `acc <- acc / mem[pc + <offset>]`

- **Remainder**
    - **Syntax:** `rem <offset>`
    - **Description:** Compute the remainder of the accumulator divided by a value from a specific address.
    - **Operation:** `acc <- acc % mem[pc + <offset>]`

- **Clear Overflow**
    - **Syntax:** `clv`
    - **Description:** Clear the overflow flag.
    - **Operation:** `overflow <- 0`

- **Clear Carry**
    - **Syntax:** `clc`
    - **Description:** Clear the carry flag.
    - **Operation:** `carry <- 0`

### Bitwise Instructions

- **Shift Left**
    - **Syntax:** `shiftl <offset>`
    - **Description:** Shift the accumulator left by a number of bits from a specific address (Carry flag will not be used).
    - **Operation:** `acc <- acc << mem[pc + <offset>]`

- **Shift Right**
    - **Syntax:** `shiftr <offset>`
    - **Description:** Shift the accumulator right by a number of bits from a specific address, preserving the sign (Carry flag will not be used).
    - **Operation:** `acc <- acc >> mem[pc + <offset>]`

- **Bitwise AND**
    - **Syntax:** `and <offset>`
    - **Description:** Perform a bitwise AND on the accumulator with a value from a specific address.
    - **Operation:** `acc <- acc & mem[pc + <offset>]`

- **Bitwise OR**
    - **Syntax:** `or <offset>`
    - **Description:** Perform a bitwise OR on the accumulator with a value from a specific address.
    - **Operation:** `acc <- acc | mem[pc + <offset>]`

- **Bitwise XOR**
    - **Syntax:** `xor <offset>`
    - **Description:** Perform a bitwise XOR on the accumulator with a value from a specific address.
    - **Operation:** `acc <- acc ^ mem[pc + <offset>]`

- **Bitwise NOT**
    - **Syntax:** `not`
    - **Description:** Perform a bitwise NOT on the accumulator.
    - **Operation:** `acc <- ~acc`

### Control Flow Instructions

- **Jump**
    - **Syntax:** `jmp <address>`
    - **Description:** Jump to a specific address.
    - **Operation:** `pc <- <address>`

- **Branch if Equal to Zero**
    - **Syntax:** `beqz <address>`
    - **Description:** Jump to a specific address if the accumulator is zero.
    - **Operation:** `if acc == 0 then pc <- <address>`

- **Branch if Not Equal to Zero**
    - **Syntax:** `bnez <address>`
    - **Description:** Jump to a specific address if the accumulator is not zero.
    - **Operation:** `if acc != 0 then pc <- <address>`

- **Branch if Greater Than Zero**
    - **Syntax:** `bgt <address>`
    - **Description:** Jump to a specific address if the accumulator is greater than zero.
    - **Operation:** `if acc > 0 then pc <- <address>`

- **Branch if Less Than Zero**
    - **Syntax:** `ble <address>`
    - **Description:** Jump to a specific address if the accumulator is less than zero.
    - **Operation:** `if acc < 0 then pc <- <address>`

- **Branch if Overflow Set**
    - **Syntax:** `bvs <address>`
    - **Description:** Jump to a specific address if the overflow flag is set.
    - **Operation:** `if overflow == 1 then pc <- <address>`

- **Branch if Overflow Clear**
    - **Syntax:** `bvc <address>`
    - **Description:** Jump to a specific address if the overflow flag is clear.
    - **Operation:** `if overflow == 0 then pc <- <address>`

- **Branch if Carry Set**
    - **Syntax:** `bcs <address>`
    - **Description:** Jump to a specific address if the carry flag is set.
    - **Operation:** `if carry == 1 then pc <- <address>`

- **Branch if Carry Clear**
    - **Syntax:** `bcc <address>`
    - **Description:** Jump to a specific address if the carry flag is clear.
    - **Operation:** `if carry == 0 then pc <- <address>`

- **Halt**
    - **Syntax:** `halt`
    - **Description:** Halt the machine.
