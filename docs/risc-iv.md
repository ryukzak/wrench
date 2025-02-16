# RiscIv Instruction Set Architecture (ISA) Documentation

The RiscIv ISA is a simple register-based instruction set inspired by the RISC-V architecture. This documentation provides an overview of the instructions available in the RiscIv ISA, their syntax, and their semantics.

Comments in RiscIv assembly code are denoted by the `;` character.

Inspired by [RISC-V](https://riscv.org/wp-content/uploads/2017/05/riscv-spec-v2.2.pdf)

## Instructions

Instruction size: 4 bytes.

### Data Movement Instructions

- **Load Upper Immediate**
    - **Syntax:** `lui <rd>, <k>`
    - **Description:** Load an immediate value shifted left by 12 bits into the destination register.
    - **Operation:** `rd <- k << 12`

- **Move**
    - **Syntax:** `mv <rd>, <rs>`
    - **Description:** Move the value from the source register to the destination register.
    - **Operation:** `rd <- rs`

- **Store Word**
    - **Syntax:** `sw <rs2>, <offset>(<rs1>)`
    - **Description:** Store the value from the source register into memory at the address computed by adding the offset to the base register.
    - **Operation:** `M[offset + rs1] <- rs2`

- **Store Byte**
    - **Syntax:** `sb <rs2>, <offset>(<rs1>)`
    - **Description:** Store the lower 8 bits of the value from the source register into memory at the address computed by adding the offset to the base register.
    - **Operation:** `M[offset + rs1] <- rs2 & 0xFF`

- **Load Word**
    - **Syntax:** `lw <rd>, <offset>(<rs1>)`
    - **Description:** Load a word from memory at the address computed by adding the offset to the base register into the destination register.
    - **Operation:** `rd <- M[offset + rs1]`

### Arithmetic Instructions

- **Add Immediate**
    - **Syntax:** `addi <rd>, <rs1>, <k>`
    - **Description:** Add an immediate value to the source register and store the result in the destination register. Only the lower 12 bits of the result are stored (upper 20 bits sets depending on the sign of the immediate value).
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

### Bitwise Instructions

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

### Control Flow Instructions

- **Jump**
    - **Syntax:** `j <k>`
    - **Description:** Jump to the address computed by adding the immediate value to the current program counter.
    - **Operation:** `pc <- pc + k`

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

- **Halt**
    - **Syntax:** `halt`
    - **Description:** Halt the machine.

## ISA Specific State Views

- `<reg>:dec`, `<reg>:hex` -- View the value of a specific register in decimal or hexadecimal format.

Available registers: `Zero`, `Ra`, `Sp`, `Gp`, `Tp`, `T0`, `T1`, `T2`, `S0Fp`, `S1`, `A0`, `A1`, `A2`, `A3`, `A4`, `A5`, `A6`, `A7`, `S2`, `S3`, `S4`, `S5`, `S6`, `S7`, `S8`, `S9`, `S10`, `S11`, `T3`, `T4`, `T5`, `T6`.
