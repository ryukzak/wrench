# Acc32 Instruction Set Architecture (ISA) Documentation

The Acc32 ISA is a simple accumulator-based instruction set designed for educational purposes. This documentation provides an overview of the instructions available in the Acc32 ISA, their syntax, and their semantics.

Comments in Acc32 assembly code are denoted by the `;` character.

## ISA Specific State Views

- `Acc:dec`, `Acc:hex` -- `Acc` register.
- `V` -- Overflow flag.

## Instructions

Instruction size: 1 byte for opcode, 4 bytes for absolute operand, 2 bytes for relative operand for relative. Control flow, Load/Store Immidiate/Indirect/Addr use absolute address, other -- relative.

### Data Movement Instructions

- **Load Immediate**
    - **Syntax:** `load_imm <address>`
    - **Description:** Load an immediate value into the accumulator.
    - **Operation:** `acc <- <address>`

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

- **Load Indirect**
    - **Syntax:** `load_ind <address>`
    - **Description:** Load a value from an indirect address into the accumulator.
    - **Operation:** `acc <- mem[mem[<address>]]`

- **Store Indirect**
    - **Syntax:** `store_ind <address>`
    - **Description:** Store the accumulator value into an indirect address.
    - **Operation:** `mem[mem[<address>]] <- acc`

### Arithmetic Instructions

- **Add**
    - **Syntax:** `add <address>`
    - **Description:** Add a value from a specific address to the accumulator.
    - **Operation:** `acc <- acc + mem[<address>]`

- **Subtract**
    - **Syntax:** `sub <address>`
    - **Description:** Subtract a value from a specific address from the accumulator.
    - **Operation:** `acc <- acc - mem[<address>]`

- **Multiply**
    - **Syntax:** `mul <address>`
    - **Description:** Multiply the accumulator by a value from a specific address.
    - **Operation:** `acc <- acc * mem[<address>]`

- **Divide**
    - **Syntax:** `div <address>`
    - **Description:** Divide the accumulator by a value from a specific address.
    - **Operation:** `acc <- acc / mem[<address>]`

- **Remainder**
    - **Syntax:** `rem <address>`
    - **Description:** Compute the remainder of the accumulator divided by a value from a specific address.
    - **Operation:** `acc <- acc % mem[<address>]`

- **Clear Overflow**
    - **Syntax:** `clv`
    - **Description:** Clear the overflow flag.
    - **Operation:** `overflow <- 0`

### Bitwise Instructions

- **Shift Left**
    - **Syntax:** `shiftl <address>`
    - **Description:** Shift the accumulator left by a number of bits from a specific address.
    - **Operation:** `acc <- acc << mem[<address>]`

- **Shift Right**
    - **Syntax:** `shiftr <address>`
    - **Description:** Shift the accumulator right by a number of bits from a specific address.
    - **Operation:** `acc <- acc >> mem[<address>]`

- **Bitwise AND**
    - **Syntax:** `and <address>`
    - **Description:** Perform a bitwise AND on the accumulator with a value from a specific address.
    - **Operation:** `acc <- acc & mem[<address>]`

- **Bitwise OR**
    - **Syntax:** `or <address>`
    - **Description:** Perform a bitwise OR on the accumulator with a value from a specific address.
    - **Operation:** `acc <- acc | mem[<address>]`

- **Bitwise XOR**
    - **Syntax:** `xor <address>`
    - **Description:** Perform a bitwise XOR on the accumulator with a value from a specific address.
    - **Operation:** `acc <- acc ^ mem[<address>]`

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
    - **Syntax:** `bsc <address>`
    - **Description:** Jump to a specific address if the overflow flag is clear.
    - **Operation:** `if overflow == 0 then pc <- <address>`

- **Halt**
    - **Syntax:** `halt`
    - **Description:** Halt the machine.
