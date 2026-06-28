# VLIW-IV Instruction Set Architecture (ISA) Documentation

The VLIW ISA is a simple register-based instruction set inspired by VLIW (Very Long Instruction Word) principles and RISC-V. This documentation provides an overview of the instructions available in the VLIW ISA, their syntax, and their semantics.

## Architecture Overview

The VLIW architecture is a 32-bit VLIW (Very Long Instruction Word) architecture inspired by RISC-V and classic VLIW designs. It features:

- 32 general-purpose registers (including one hardwired zero register — writes to `Zero` are silently ignored)
- Fixed-length 14-byte (112-bit) instruction bundles, divided into 4 slots for parallel execution
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
    - **Usage:** `lui rd, %hi(symbol) / nop / nop / nop`
    - **Operation:** `rd <- (symbol & 0xFFFFF000)`

- **%lo(symbol)**
    - **Description:** Used to extract the lower 12 bits of a 32-bit address or immediate value
    - **Usage:** `addi rd, rs, %lo(symbol) / nop / nop / nop`
    - **Operation:** `rd <- rs + (symbol & 0x00000FFF)`

These directives are typically used together to load a full 32-bit address into a register:

```assembly
lui  a0, %hi(address)     / nop / nop / nop ; Load upper 20 bits into a0
addi a0, a0, %lo(address) / nop / nop / nop ; Add lower 12 bits to a0
```

## Instructions

Instruction size: 14 bytes (112-bit bundle).

```text
Bundle: [ALU1:32] [ALU2:32] [MEM:24] [CTRL:24]

[ALU1/ALU2]:
  [opcode:5] [rd:5] [rs1:5] [rs2:5] [reserved:12]   ; add sub mul mulh div rem sll srl sra and or xor
  [opcode:5] [rd:5] [rs1:5] [imm_sign:17]           ; addi slti
  [opcode:5] [rd:5] [imm_unsign:22]                 ; lui
  [opcode:5] [rd:5] [rs1:5] [reserved:17]           ; mv
  [opcode:5] [reserved:27]                          ; nop
[MEM] (24 bits):
  [opcode:3] [rd:5]  [base:5] [offset_sign:11]      ; lw lb        (reg = rd)
  [opcode:3] [rs2:5] [base:5] [offset_sign:11]      ; sw sb        (reg = rs2)
  [opcode:3] [reserved:21]                          ; nop
[CTRL] (24 bits):
  [opcode:4] [rs1:5] [rs2:5] [offset_sign:10]       ; beq bne blt bgt ble bgtu bleu
  [opcode:4] [rs1:5] [offset_sign:15]               ; beqz bnez
  [opcode:4] [rd:5]  [offset_sign:15]               ; jal
  [opcode:4] [rs:5]  [reserved:15]                  ; jr
  [opcode:4] [offset_sign:20]                       ; j
  [opcode:4] [reserved:20]                          ; nop halt
```

The two ALU slots are 32 bits each; MEM and CTRL are 24 bits each, for a 112-bit (14-byte)
bundle. The opcode occupies the high bits of its slot, sized to the number of operations the slot
defines: 5 bits for ALU (17 ops), 4 for CTRL (14 ops), 3 for MEM (5 ops). Register fields
follow the opcode; the remaining low bits hold the immediate/offset.

Every immediate and offset is **silently truncated** to its field width: the value is reduced
to the low bits of the field and sign-extended (`lui`'s `imm_unsign` is zero-extended), with no
error if it does not fit. So `addi`/`slti` see a 17-bit signed immediate, branches a 10-bit
signed offset, `beqz`/`bnez`/`jal` a 15-bit one, `j` a 20-bit one, and memory ops an 11-bit
one. `lui` keeps only the low 20 bits (after the `<< 12` shift the upper bits of the 22-bit
field cannot be represented in a 32-bit word). The `%lo` (12-bit) and `%hi` (20-bit) relocation
values fit within these fields unchanged.

Each instruction is a bundle with 4 slots: Slot 0 (ALU1), Slot 1 (ALU2), Slot 2 (Memory), Slot 3 (Control). Operations in slots execute in parallel. Unused slots are NOP (no operation). Assembly syntax uses `/` to separate slots.

**Execution model:** All source operands are read first, then all results are written back simultaneously. The order of register writes from ALU1, ALU2, and Memory slots is non-deterministic — if multiple slots write to the same register, the result is undefined. The control slot always executes last. The compiler/assembler must ensure that parallel operations in the same bundle are independent (no write-write or read-write conflicts across slots).

```assembly
add rd, rs1, rs2 / addi rd, rs1, k / lw rd, offset(rs1) / beq rs1, rs2, k
```

### Slot 0 and Slot 1: ALU Operations (Identical)

- **Load Upper Immediate**
    - **Syntax:** `lui <rd>, <k>`
    - **Description:** Load an immediate value masked to 20 bits and shifted left by 12 bits into the destination register.
    - **Operation:** `rd <- (k & 0x000FFFFF) << 12`

- **Move**
    - **Syntax:** `mv <rd>, <rs>`
    - **Description:** Move the value from the source register to the destination register.
    - **Operation:** `rd <- rs`

- **Add Immediate**
    - **Syntax:** `addi <rd>, <rs1>, <k>`
    - **Description:** Add a 17-bit sign-extended immediate value to the source register and store the result in the destination register. The immediate `k` is truncated to the 17-bit `imm_sign` field and sign-extended to 32 bits before the addition.
    - **Operation:** `rd <- rs1 + signext(k[16:0])`

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
    - **Description:** Shift the value of the first source register left by the number of bits specified in the lower 5 bits of the second source register and store the result in the destination register.
    - **Operation:** `rd <- rs1 << (rs2 & 0x1F)`

- **Logical Shift Right**
    - **Syntax:** `srl <rd>, <rs1>, <rs2>`
    - **Description:** Shift the value of the first source register right (zero-fill) by the number of bits specified in the lower 5 bits of the second source register and store the result in the destination register.
    - **Operation:** `rd <- rs1 >>> (rs2 & 0x1F)`

- **Arithmetic Shift Right**
    - **Syntax:** `sra <rd>, <rs1>, <rs2>`
    - **Description:** Shift the value of the first source register right by the number of bits specified in the lower 5 bits of the second source register, preserving the sign, and store the result in the destination register.
    - **Operation:** `rd <- rs1 >> (rs2 & 0x1F)`

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

### Slot 2: Memory Operations

- **Load Word**
    - **Syntax:** `lw <rd>, <offset>(<rs1>)`
    - **Description:** Load a word from memory at the address computed by adding the offset to the base register into the destination register.
    - **Operation:** `rd <- M[offset + rs1]`

- **Load Byte**
    - **Syntax:** `lb <rd>, <offset>(<rs1>)`
    - **Description:** Load a byte from memory at the address computed by adding the offset to the base register, sign-extend it to 32 bits, and store in the destination register.
    - **Operation:** `rd <- signext(M[offset + rs1][7:0])`

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

### Slot 3: Control Operations

- **Jump**
    - **Syntax:** `j <k>`
    - **Description:** Jump to the address computed by adding the immediate value to the current program counter.
    - **Operation:** `pc <- pc + k`

- **Jump and Link**
    - **Syntax:** `jal <rd>, <k>`
    - **Description:** Store the address of the next instruction in the destination register and jump to the address computed by adding the immediate value to the current program counter.
    - **Operation:** `rd <- pc + 14, pc <- pc + k`  // Adjusted for bundle size

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

### Slot utilization (parallelism)

Each VLIW bundle has four execution slots (memory, ALU1, ALU2, control). A slot containing the corresponding `nop` is idle; everything else counts as active. The simulator tallies how many slots each executed bundle used and exposes three summary view variables. They're typically used with `slice: last` because the values are run-totals.

- `vliw:load-percent` -- average slot utilization across the run, as an integer percent. `(active_slots * 100) / (bundles * 4)`. `25%` means each bundle used one slot on average; `100%` means every slot of every bundle was active.
- `vliw:avg-load` -- average number of active processing units (slots) per executed bundle, rendered with two decimals. `active_slots / bundles`, a value in `0.00`–`4.00`. This is the same measure as `vliw:load-percent` expressed as units-per-bundle instead of a percent of the four-wide width.
- `vliw:bundles-by-load` -- histogram rendered as comma-separated `K:N (P%)` entries, one per non-empty bucket. `K` is the active-slot count, `N` is the number of bundles in that bucket, `P` is its percent share of all executed bundles. Empty buckets are skipped. A peak at 1 means a serial program; a peak at 3-4 means the compiler / programmer is exploiting the pipeline.

Example -- print a load summary at the end of the simulation:

```yaml
reports:
    - name: vliw-load
      slice: last
      view: |
        vliw:load-percent:    {vliw:load-percent}
        vliw:avg-load:        {vliw:avg-load}
        vliw:bundles-by-load: {vliw:bundles-by-load}
```

For `hello.s` running on this ISA the summary reads:

```text
vliw:load-percent:    81%
vliw:avg-load:        3.24
vliw:bundles-by-load: 1:3 (9%), 3:16 (48%), 4:14 (42%)
```

— 14 fully-packed bundles (`4:14 (42%)`), 16 three-wide (`3:16 (48%)`), 3 with a single active slot (`1:3 (9%)`), no idle or two-slot bundles, for an average of `3.24` active units per bundle.

All three lines are also emitted together by the generic `{isa-specific}` summary block, which lets a single report template stay uniform across ISAs.
