# RISC-IV Instruction Set Architecture (ISA) Documentation

The RISC-IV ISA is a simple register-based instruction set inspired by the RISC-V architecture. This documentation provides an overview of the instructions available in the RISC-IV ISA, their syntax, and their semantics.

## Architecture Overview

The RISC-IV architecture is a 32-bit RISC (Reduced Instruction Set Computer) architecture inspired by the RISC-V specification. It features:

- 32 general-purpose registers (including one hardwired zero register — writes to `Zero` are silently discarded)
- Fixed-length 4-byte instructions
- Load-store architecture (memory access only through specific instructions)
- Simple addressing modes
- Memory-mapped I/O
- Support for function calls and returns through jump-and-link instructions
- Arithmetic, logical, and control flow operations

This architecture provides a clean, orthogonal instruction set that exemplifies RISC design principles, making it excellent for educational purposes while still being powerful enough for practical applications.

Comments in RISC-IV assembly code are denoted by the `;` character.

Inspired by [RISC-V](https://riscv.org/wp-content/uploads/2017/05/riscv-spec-v2.2.pdf)

### Register Usage Conventions

Although most registers can technically be used for any purposes, the following conventions are recommended

| Register(s) | Purpose                                                                       | Convention   |
| ----------- | ----------------------------------------------------------------------------- | ------------ |
| `Zero`      | Constant zero value. Writes are ignored                                       | Preserved    |
| `Ra`        | Return address for function calls                                             | Caller-saved |
| `Sp`        | Stack pointer                                                                 | Callee-saved |
| `Gp`        | Global pointer. Points to a region containing frequently accessed global data | Callee-saved |
| `Tp`        | Thread pointer. Reserved for thread-local data                                | Callee-saved |
| `A0-A7`     | Function arguments and return values                                          | Caller-saved |
| `T0-T6`     | Temporary registers for intermediate calculations                             | Caller-saved |
| `S0Fp`      | Frame pointer or saved register                                               | Callee-saved |
| `S1-S11`    | Saved registers for long-lived values                                         | Callee-saved |

**Caller-saved** registers may be freely modified by the called function. If the caller needs their values after a function call, it must save and restore them

**Callee-saved** registers must retain their values across function calls. A function that modifies a callee-saved register must restore its original value before returning

## Immediate Value Relocation Directives

The RISC-IV assembly language provides special directives for handling larger immediate values that don't fit within the standard instruction formats:

- **%hi(symbol)**
    - **Description:** Extract the upper 20 bits of a 32-bit address or immediate value. The value is first rounded up by half a `%lo` field (`+0x800`) to compensate for the sign extension `%lo` is subject to.
    - **Usage:** `lui rd, %hi(symbol)`
    - **Operation:** `%hi(symbol) = ((symbol + 0x800) >> 12) & 0xFFFFF`

- **%lo(symbol)**
    - **Description:** Extract the lower 12 bits of a 32-bit address or immediate value, **sign-extended from bit 11**, because every instruction that takes a 12-bit immediate sign-extends it. So `%lo(0xFFFFFFFF)` is `-1`, not `0xFFF`, and `addi rd, zero, -1` and `addi rd, zero, %lo(-1)` give the same result.
    - **Usage:** `addi rd, rs, %lo(symbol)`
    - **Operation:** `%lo(symbol) = signext(symbol[11:0])`

These directives are typically used together to load a full 32-bit address into a register. The `+0x800` in `%hi` cancels the borrow caused by a negative `%lo`, so the pair reconstructs any 32-bit value exactly:

```assembly
lui  a0, %hi(address)    ; Load upper 20 bits into a0
addi a0, a0, %lo(address) ; Add lower 12 bits to a0
```

## Instructions

Instruction size: 4 bytes.

### Immediate and Offset Fields

Every immediate, offset and displacement has to fit the 4-byte instruction that carries it, so none
of them can hold a full 32-bit value:

| Field                                                                   | Width           | Accepted values     |
| ----------------------------------------------------------------------- | --------------- | ------------------- |
| `lw`, `lb`, `sw`, `sb` offset                                           | 12-bit signed   | `-2048..2047`       |
| `addi`, `slti`, `andi`, `ori`, `xori` immediate                         | 12-bit signed   | `-2048..2047`       |
| `slli`, `srli`, `srai` shift amount                                     | 5-bit unsigned  | `0..31`             |
| `beqz`, `bnez`, `beq`, `bne`, `bgt`, `ble`, `bgtu`, `bleu` displacement | 13-bit signed   | `-4096..4095`       |
| `j`, `jal` displacement                                                 | 21-bit signed   | `-1048576..1048575` |
| `lui` immediate                                                         | 20-bit unsigned | `0..1048575`        |

A value outside its field has no encoding at all, so it is a translation error rather than a program
that quietly does something else. A memory offset is checked while parsing, so it is reported with
the position of the operand:

```text
error (risc-iv-32): program.s:3:12:
  |
3 |     lw a0, 0x1FFF00(zero)
  |            ^
offset 2096896 doesn't fit the 12-bit signed field of a 4 byte instruction, expected -2048..2047
```

A literal that does not fit the 32-bit machine word is rejected earlier still, before any field is
considered, since it could not reach a register in the first place:

```text
error (risc-iv-32): program.s:3:20:
  |
3 |     addi t0, zero, 4294967296
  |                    ^
literal 4294967296 doesn't fit a 32-bit machine word, expected -2147483648..4294967295
```

Anything the word's bits can spell is allowed there, signed or unsigned, so `0xFFFFFFFF` and `-1`
name the same word and both then face the 12-bit field as `-1`.

Immediates and displacements may be written as labels, so they are checked once the labels are
resolved. The message points at the operand and names the mnemonic and the field:

```text
error (risc-iv-32): program.s:3:17: beq: B-type disp 8192 is not in -4096..4095
```

Use `lui`/`addi` with the `%hi`/`%lo` directives above to build a wide constant or address in a
register, then work through that register. `%lo` also keeps a bit pattern inside the field:
`addi rd, rs, %lo(0x800)` is how the immediate `-2048` is written as the bits `0x800`, since the
bare `0x800` is out of range.

#### Branch and jump displacements are not scaled (RISC-IV specific)

RISC-V encodes a branch or jump displacement in multiples of two: bit 0 is not stored and is assumed
to be zero, which costs nothing there, because no instruction may start at an odd address. This is
where RISC-IV deviates from it: the displacement is stored as a plain signed number, bit 0 included.
So the intervals above are the full signed range of each field -- odd displacements included --
where RISC-V would instead allow only the even values of a range twice as wide.

Nothing requires a displacement to land on an instruction boundary either. Every RISC-IV instruction
is 4 bytes, so a displacement that is not a multiple of 4 points into the middle of one, and the
jump fails when that address is fetched:

```text
0: J {k = 6}
ERROR: memory[0x06]: instruction in memory corrupted
```

### Data Movement Instructions

- **Load Upper Immediate**
    - **Syntax:** `lui <rd>, <k>`
    - **Description:** Load an immediate value shifted left by 12 bits into the destination register.
    - **Operation:** `rd <- (k & 0x000FFFFF) << 12`

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

- **Load Byte**
    - **Syntax:** `lb <rd>, <offset>(<rs1>)`
    - **Description:** Load a byte from memory at the address computed by adding the offset to the base register, sign-extend it to 32 bits, and store in the destination register.
    - **Operation:** `rd <- signext(M[offset + rs1][7:0])`

### Arithmetic Instructions

- **Add Immediate**
    - **Syntax:** `addi <rd>, <rs1>, <k>`
    - **Description:** Add a 12-bit sign-extended immediate value to the source register and store the result in the destination register. The sign comes from bit 11 of the field, so `%lo(0x800)` is `-2048`.
    - **Operation:** `rd <- rs1 + signext(k[11:0])`

- **Set Less Than Immediate**
    - **Syntax:** `slti <rd>, <rs1>, <k>`
    - **Description:** Set the destination register to 1 if the source register is less than the immediate value (signed comparison), else set to 0. As for `addi`, the immediate is sign-extended from bit 11.
    - **Operation:** `rd <- (rs1 < signext(k[11:0])) ? 1 : 0`

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

- **Logical Shift Left Immediate**
    - **Syntax:** `slli <rd>, <rs1>, <k>`
    - **Description:** Shift the value of the source register left by the immediate amount and store the result in the destination register.
    - **Operation:** `rd <- rs1 << (k & 0x1F)`

- **Logical Shift Right Immediate**
    - **Syntax:** `srli <rd>, <rs1>, <k>`
    - **Description:** Shift the value of the source register right (zero-fill) by the immediate amount and store the result in the destination register.
    - **Operation:** `rd <- rs1 >>> (k & 0x1F)`

- **Arithmetic Shift Right Immediate**
    - **Syntax:** `srai <rd>, <rs1>, <k>`
    - **Description:** Shift the value of the source register right by the immediate amount, preserving the sign, and store the result in the destination register.
    - **Operation:** `rd <- rs1 >> (k & 0x1F)`

- **Logical Shift Left**
    - **Syntax:** `sll <rd>, <rs1>, <rs2>`
    - **Description:** Shift the value of the first source register left by the number of bits specified in the lower 5 bits of the second source register and store the result in the destination register.
    - **Operation:** `rd <- rs1 << (rs2 & 0x1F)`

- **Logical Shift Right**
    - **Syntax:** `srl <rd>, <rs1>, <rs2>`
    - **Description:** Shift the value of the first source register right by the number of bits specified in the lower 5 bits of the second source register and store the result in the destination register.
    - **Operation:** `rd <- rs1 >> (rs2 & 0x1F)`

- **Arithmetic Shift Right**
    - **Syntax:** `sra <rd>, <rs1>, <rs2>`
    - **Description:** Shift the value of the first source register right by the number of bits specified in the lower 5 bits of the second source register, preserving the sign, and store the result in the destination register.
    - **Operation:** `rd <- rs1 >> (rs2 & 0x1F)`

- **Bitwise AND**
    - **Syntax:** `and <rd>, <rs1>, <rs2>`
    - **Description:** Perform a bitwise AND on the values of two source registers and store the result in the destination register.
    - **Operation:** `rd <- rs1 & rs2`

- **Bitwise AND Immediate**
    - **Syntax:** `andi <rd>, <rs1>, <k>`
    - **Description:** Perform a bitwise AND of the source register with a 12-bit sign-extended immediate value. Because the field is sign-extended, the widest low-bit mask `andi` can express is the 11 bits of `0x7FF`. A negative immediate clears low bits instead: `-16` is `0xFFFFFFF0`. `%lo(0xFFF)` is `-1`, so it leaves the register unchanged.
    - **Operation:** `rd <- rs1 & signext(k[11:0])`

- **Bitwise OR**
    - **Syntax:** `or <rd>, <rs1>, <rs2>`
    - **Description:** Perform a bitwise OR on the values of two source registers and store the result in the destination register.
    - **Operation:** `rd <- rs1 | rs2`

- **Bitwise OR Immediate**
    - **Syntax:** `ori <rd>, <rs1>, <k>`
    - **Description:** Perform a bitwise OR of the source register with a 12-bit sign-extended immediate value.
    - **Operation:** `rd <- rs1 | signext(k[11:0])`

- **Bitwise XOR**
    - **Syntax:** `xor <rd>, <rs1>, <rs2>`
    - **Description:** Perform a bitwise XOR on the values of two source registers and store the result in the destination register.
    - **Operation:** `rd <- rs1 ^ rs2`

- **Bitwise XOR Immediate**
    - **Syntax:** `xori <rd>, <rs1>, <k>`
    - **Description:** Perform a bitwise XOR of the source register with a 12-bit sign-extended immediate value.
    - **Operation:** `rd <- rs1 ^ signext(k[11:0])`

### Control Flow Instructions

- **Jump**
    - **Syntax:** `j <k>`
    - **Description:** Jump to the address computed by adding the displacement to the current program counter.
    - **Operation:** `pc <- pc + k`

- **Jump and Link**
    - **Syntax:** `jal <rd>, <k>`
    - **Description:** Store the address of the next instruction in the destination register and jump to the address computed by adding the displacement to the current program counter.
    - **Operation:** `rd <- pc + 4, pc <- pc + k`

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
    - **Description:** Jump to the address computed by adding the immediate value to the current program counter if the unsigned interpretation of the first source register is greater than the unsigned interpretation of the second source register.
    - **Operation:** `if unsigned(rs1) > unsigned(rs2) then pc <- pc + k`

- **Branch if Less Than or Equal (Unsigned)**
    - **Syntax:** `bleu <rs1>, <rs2>, <k>`
    - **Description:** Jump to the address computed by adding the immediate value to the current program counter if the unsigned interpretation of the first source register is less than or equal to the unsigned interpretation of the second source register.
    - **Operation:** `if unsigned(rs1) <= unsigned(rs2) then pc <- pc + k`

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
