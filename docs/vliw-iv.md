# VLIW-IV Instruction Set Architecture (ISA) Documentation

The VLIW ISA is a simple register-based instruction set inspired by VLIW (Very Long Instruction Word) principles and RISC-V. This documentation provides an overview of the instructions available in the VLIW ISA, their syntax, and their semantics.

## Architecture Overview

The VLIW architecture is a 32-bit VLIW (Very Long Instruction Word) architecture inspired by RISC-V and classic VLIW designs. It features:

- 32 general-purpose registers (including one hardwired zero register)
- Fixed-length 16-byte (128-bit) instruction bundles, divided into 4 slots for parallel execution
- Load-store architecture (memory access only through specific instructions in dedicated slots)
- Simple addressing modes
- Memory-mapped I/O
- Support for function calls and returns through jump-and-link instructions
- Arithmetic, logical, and control flow operations executed in parallel where possible
- Static scheduling: The compiler/assembler bundles independent operations; hardware executes them in lockstep

This architecture emphasizes instruction-level parallelism (ILP) through wide instructions, making it ideal for educational exploration of VLIW concepts like compiler scheduling, slot utilization, and parallelism trade-offs, while maintaining RISC-like simplicity in individual operations.

Comments in VLIW assembly code are denoted by the `;` character.

Inspired by [RISC-V](https://riscv.org/wp-content/uploads/2017/05/riscv-spec-v2.2.pdf) and classic VLIW designs.

## Immediate Value Relocation Directives

The VLIW assembly language provides special directives for handling larger immediate values that don't fit within the standard instruction formats similar to RISC-IV implementation:

- **%hi(symbol)**
    - **Description:** Used to extract the upper 20 bits of a 32-bit address or immediate value
    - **Usage:** `lui rd, %hi(symbol)`
    - **Operation:** `rd <- (symbol & 0xFFFFF000)`

- **%lo(symbol)**
    - **Description:** Used to extract the lower 12 bits of a 32-bit address or immediate value
    - **Usage:** `addi rd, rs, %lo(symbol)`
    - **Operation:** `rd <- rs + (symbol & 0x00000FFF)`

These directives are typically used together to load a full 32-bit address into a register:

```assembly
lui  a0, %hi(address)    ; Load upper 20 bits into a0
addi a0, a0, %lo(address) ; Add lower 12 bits to a0
```

## Instructions

Instruction size: 16 bytes (128-bit bundle).

Each instruction is a bundle with 4 slots: Slot 0 (Memory), Slot 1 (ALU1), Slot 2 (ALU2), Slot 3 (Control). Operations in slots execute in parallel. Unused slots are NOP (no operation). Assembly syntax uses `|` to separate slots:

```assembly
lw rd, offset(rs1) | add rd, rs1, rs2 | addi rd, rs1, k | beq rs1, rs2, k
```

### Slot 0: Memory Operations

- **Load Word**
    - **Syntax:** `lw <rd>, <offset>(<rs1>)`
    - **Description:** Load a word from memory at the address computed by adding the offset to the base register into the destination register.
    - **Operation:** `rd <- M[offset + rs1]`

- **Store Word**
    - **Syntax:** `sw <rs2>, <offset>(<rs1>)`
    - **Description:** Store the value from the source register into memory at the address computed by adding the offset to the base register.
    - **Operation:** `M[offset + rs1] <- rs2`

- **Store Byte**
    - **Syntax:** `sb <rs2>, <offset>(<rs1>)`
    - **Description:** Store the lower 8 bits of the value from the source register into memory at the address computed by adding the offset to the base register.
    - **Operation:** `M[offset + rs1] <- rs2 & 0xFF`

- **NOP**
    - **Syntax:** `nop`
    - **Description:** No operation.

### Slot 1 and Slot 2: ALU Operations (Identical)

- **Load Upper Immediate**
    - **Syntax:** `lui <rd>, <k>`
    - **Description:** Load an immediate value shifted left by 12 bits into the destination register.
    - **Operation:** `rd <- k << 12`

- **Move**
    - **Syntax:** `mv <rd>, <rs>`
    - **Description:** Move the value from the source register to the destination register.
    - **Operation:** `rd <- rs`

- **Add Immediate**
    - **Syntax:** `addi <rd>, <rs1>, <k>`
    - **Description:** Add an immediate value to the source register and store the result in the destination register.
    - **Operation:** `rd <- rs1 + k`

- **Add**
    - **Syntax:** `add <rd>, <rs1>, <rs2>`
    - **Description:** Add the values of two source registers and store the result in the destination register.
    - **Operation:** `rd <- rs1 + rs2`

- **Subtract**
    - **Syntax:** `sub <rd>, <rs1>, <rs2>`
    - **Description:** Subtract the value of the second source register from the first source register and store the result in the destination register.
    - **Operation:** `rd <- rs1 - rs2`

- **Multiply**
    - **Syntax:** `mul <rd>, <rs1>, <rs2>`
    - **Description:** Multiply the values of two source registers and store the result in the destination register.
    - **Operation:** `rd <- rs1 * rs2`

- **Multiply High**
    - **Syntax:** `mulh <rd>, <rs1>, <rs2>`
    - **Description:** Multiply the values of two source registers and store the high part of the result in the destination register.
    - **Operation:** `rd <- (rs1 * rs2) >> (word size)`

- **Divide**
    - **Syntax:** `div <rd>, <rs1>, <rs2>`
    - **Description:** Divide the value of the first source register by the value of the second source register and store the result in the destination register.
    - **Operation:** `rd <- rs1 / rs2`

- **Remainder**
    - **Syntax:** `rem <rd>, <rs1>, <rs2>`
    - **Description:** Compute the remainder of the division of the first source register by the second source register and store the result in the destination register.
    - **Operation:** `rd <- rs1 % rs2`

- **Logical Shift Left**
    - **Syntax:** `sll <rd>, <rs1>, <rs2>`
    - **Description:** Shift the value of the first source register left by the number of bits specified in the second source register and store the result in the destination register.
    - **Operation:** `rd <- rs1 << rs2`

- **Logical Shift Right**
    - **Syntax:** `srl <rd>, <rs1>, <rs2>`
    - **Description:** Shift the value of the first source register right by the number of bits specified in the second source register and store the result in the destination register.
    - **Operation:** `rd <- rs1 >> rs2`

- **Arithmetic Shift Right**
    - **Syntax:** `sra <rd>, <rs1>, <rs2>`
    - **Description:** Shift the value of the first source register right by the number of bits specified in the second source register, preserving the sign, and store the result in the destination register.
    - **Operation:** `rd <- rs1 >> rs2`

- **Bitwise AND**
    - **Syntax:** `and <rd>, <rs1>, <rs2>`
    - **Description:** Perform a bitwise AND on the values of two source registers and store the result in the destination register.
    - **Operation:** `rd <- rs1 & rs2`

- **Bitwise OR**
    - **Syntax:** `or <rd>, <rs1>, <rs2>`
    - **Description:** Perform a bitwise OR on the values of two source registers and store the result in the destination register.
    - **Operation:** `rd <- rs1 | rs2`

- **Bitwise XOR**
    - **Syntax:** `xor <rd>, <rs1>, <rs2>`
    - **Description:** Perform a bitwise XOR on the values of two source registers and store the result in the destination register.
    - **Operation:** `rd <- rs1 ^ rs2`

- **Set Less Than Immediate**
    - **Syntax:** `slti <rd>, <rs1>, <k>`
    - **Description:** Set the destination register to 1 if the source register is less than the immediate (signed), else 0.
    - **Operation:** `rd <- (rs1 < k) ? 1 : 0`

- **NOP**
    - **Syntax:** `nop`
    - **Description:** No operation.

### Slot 3: Control Operations

- **Jump**
    - **Syntax:** `j <k>`
    - **Description:** Jump to the address computed by adding the immediate value to the current program counter.
    - **Operation:** `pc <- pc + k`

- **Jump and Link**
    - **Syntax:** `jal <rd>, <k>`
    - **Description:** Store the address of the next instruction in the destination register and jump to the address computed by adding the immediate value to the current program counter.
    - **Operation:** `rd <- pc + 16, pc <- pc + k`  // Adjusted for bundle size

- **Jump Register**
    - **Syntax:** `jr <rs>`
    - **Description:** Jump to the address stored in the source register.
    - **Operation:** `pc <- rs`

- **Branch if Equal to Zero**
    - **Syntax:** `beqz <rs1>, <k>`
    - **Description:** Jump to the address computed by adding the immediate value to the current program counter if the value in the source register is zero.
    - **Operation:** `if rs1 == 0 then pc <- pc + k`

- **Branch if Not Equal to Zero**
    - **Syntax:** `bnez <rs1>, <k>`
    - **Description:** Jump to the address computed by adding the immediate value to the current program counter if the value in the source register is not zero.
    - **Operation:** `if rs1 != 0 then pc <- pc + k`

- **Branch if Greater Than**
    - **Syntax:** `bgt <rs1>, <rs2>, <k>`
    - **Description:** Jump to the address computed by adding the immediate value to the current program counter if the value in the first source register is greater than the value in the second source register.
    - **Operation:** `if rs1 > rs2 then pc <- pc + k`

- **Branch if Less Than or Equal**
    - **Syntax:** `ble <rs1>, <rs2>, <k>`
    - **Description:** Jump to the address computed by adding the immediate value to the current program counter if the value in the first source register is less than or equal to the value in the second source register.
    - **Operation:** `if rs1 <= rs2 then pc <- pc + k`

- **Branch if Greater Than (Unsigned)**
    - **Syntax:** `bgtu <rs1>, <rs2>, <k>`
    - **Description:** Jump to the address computed by adding the immediate value to the current program counter if the unsigned value in the first source register is greater than the unsigned value in the second source register.
    - **Operation:** `if rs1 > rs2 then pc <- pc + k`

- **Branch if Less Than or Equal (Unsigned)**
    - **Syntax:** `bleu <rs1>, <rs2>, <k>`
    - **Description:** Jump to the address computed by adding the immediate value to the current program counter if the unsigned value in the first source register is less than or equal to the unsigned value in the second source register.
    - **Operation:** `if rs1 <= rs2 then pc <- pc + k`

- **Branch if Equal**
    - **Syntax:** `beq <rs1>, <rs2>, <k>`
    - **Description:** Jump to the address computed by adding the immediate value to the current program counter if the value in the first source register is equal to the value in the second source register.
    - **Operation:** `if rs1 == rs2 then pc <- pc + k`

- **Branch if Not Equal**
    - **Syntax:** `bne <rs1>, <rs2>, <k>`
    - **Description:** Jump to the address computed by adding the immediate value to the current program counter if the value in the first source register is not equal to the value in the second source register.
    - **Operation:** `if rs1 != rs2 then pc <- pc + k`

- **Branch if Less Than**
    - **Syntax:** `blt <rs1>, <rs2>, <k>`
    - **Description:** Jump to the address computed by adding the immediate value to the current program counter if the value in the first source register is less than the value in the second source register (signed).
    - **Operation:** `if rs1 < rs2 then pc <- pc + k`

- **NOP**
    - **Syntax:** `nop`
    - **Description:** No operation.

- **Halt**
    - **Syntax:** `halt`
    - **Description:** Halt the machine.

## ISA Specific State Views

- `<reg>:dec`, `<reg>:hex` -- View the value of a specific register in decimal or hexadecimal format.

Available registers: `Zero`, `Ra`, `Sp`, `Gp`, `Tp`, `T0`, `T1`, `T2`, `S0Fp`, `S1`, `A0`, `A1`, `A2`, `A3`, `A4`, `A5`, `A6`, `A7`, `S2`, `S3`, `S4`, `S5`, `S6`, `S7`, `S8`, `S9`, `S10`, `S11`, `T3`, `T4`, `T5`, `T6`.
