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

Instruction layouts, opcode byte first (`/signed` marks a sign-extended field; these describe the
logical field layout, not literal bit-packing):

- No operand -- 1 byte: `<<Opcode:8>>`
  (`load_acc`, `not`, `clv`, `clc`, `halt`)
- `load`/`store` -- 2 bytes, offset relative to the instruction: `<<Opcode:8, Offset:8/signed>>`
  (the source names an absolute address, see [Relative Addressing](#relative-addressing))
- Arithmetic and bitwise -- 3 bytes, absolute address: `<<Opcode:8, Address:16>>`
  (`add`, `sub`, `mul`, `div`, `rem`, `shiftl`, `shiftr`, `and`, `or`, `xor`)
- Control flow and `load_imm`/`load_addr`/`store_addr`/`store_ind` -- 5 bytes, full word:
  `<<Opcode:8, Address:32/signed>>`

### Instruction Fetch

Memory is read a word (4 bytes) at a time, from any address. The processor keeps the instructions it
read in an instruction register, `ir`. To fetch the instruction at `pc`:

- If `ir` holds an instruction at `pc`, the fetch takes it from there and costs no read.
- Otherwise it reads words starting at `pc`: 1 read for a 1–3 byte instruction, 2 for a 5-byte one.
  `ir` then holds that instruction and every following one that ends inside the first word read, so
  a 5-byte instruction comes alone.

So `mem:reads` and `mem:per-instruction` show 0, 1 or 2 fetch reads on top of the instruction's own
data accesses. A jump to an instruction already in `ir` costs no read either. Data reads never come
from `ir`, and stores don't change it.

The loop body of `example/acc32/factorial.s` (`store result`, `load n`, `sub const_1`, `store n`)
for `n = 5`:

```text
Address  Exec  Read  Write  Instruction
0x033       5     1      1  Store (-39)
0x035       5     1      0  Load (-45)
0x037       5     2      0  Sub 16
0x03a       5     1      1  Store (-50)
```

`store result` reads the word `0x033..0x036`, which also holds `load n`, so `load n` costs only its
data read. `sub const_1` reads `0x037..0x03a`, where `store n` starts but doesn't end, so `store n`
needs a read of its own.

### Relative Addressing

`load` and `store` are written with an absolute address, the same as `load_addr` and `store_addr`.
A label and a number literal mean the same thing: `load cell` and `load 0x10` both read the word at
`cell` when `cell` is at `0x10`.

Only the machine code is relative. The translator encodes the distance from the instruction to that
address, `<address> - pc`, where `pc` is the address of the `load`/`store` itself, and the processor
adds it back at run time:

```asm
    .data
    .org 0x10
cell:   .word 7

    .text
    .org 0x20
_start:
    load    cell        ; = load 0x10, encoded as offset 0x10 - 0x20 = -16
    store   0x30        ; encoded as offset 0x30 - 0x22 = 14
```

The step-by-step report shows the instruction as the machine sees it, so `load cell` above appears
as `Load (-16)`.

### Operand Fields

An operand has to fit the instruction that carries it, and the two relative instructions carry the
narrowest field in the ISA:

| Operand                                                                           | Width          | Accepted values  |
|-----------------------------------------------------------------------------------|----------------|------------------|
| `load`, `store` distance from the instruction to the address                      | 8-bit signed   | `-128..127`      |
| `add`, `sub`, `mul`, `div`, `rem`, `shiftl`, `shiftr`, `and`, `or`, `xor` address | 16-bit address | `0..65535`       |
| `load_imm`, `load_addr`, `store_addr`, `store_ind` and control-flow address       | full word      | any machine word |

`load`/`store` reach -128..+127 bytes from the instruction itself, so what is rejected is the
*distance*, not the address written in the source. An operand out of range has no encoding, so it is a
translation error that points at the operand and names the mnemonic, the field and its range:

```text
error (acc32): program.s:3:10: load: relative offset 200 is not in -128..127
```

Here `load` sits at address `0` and names address `200`, which is 200 bytes away.

Move the data nearer the code, or switch that access to the absolute `load_addr`/`store_addr`, which
carry a full word and are unaffected. `example/acc32/hello.s` mixes both.

### Data Movement Instructions

- **Load Immediate**
    - **Syntax:** `load_imm <value>`
    - **Description:** Load an immediate value into the accumulator.
    - **Operation:** `acc <- <value>`

- **Load**
    - **Syntax:** `load <address>`
    - **Description:** Load a value from an address near the instruction into the accumulator.
      Encoded as `offset = <address> - pc`, which has to fit in -128..127.
    - **Operation:** `acc <- mem[pc + offset]`, i.e. `acc <- mem[<address>]`

- **Store**
    - **Syntax:** `store <address>`
    - **Description:** Store the accumulator value into an address near the instruction.
      Encoded as `offset = <address> - pc`, which has to fit in -128..127.
    - **Operation:** `mem[pc + offset] <- acc`, i.e. `mem[<address>] <- acc`

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
    - **Syntax:** `add <address>`
    - **Description:** Add a value from a specific address to the accumulator.
    - **Operation:** `acc <- acc + mem[<address>]` and set `C` and `V` flags.

- **Subtract**
    - **Syntax:** `sub <address>`
    - **Description:** Subtract a value from a specific address from the accumulator.
    - **Operation:** `acc <- acc - mem[<address>]` and set `C` and `V` flags.

- **Multiply**
    - **Syntax:** `mul <address>`
    - **Description:** Multiply the accumulator by a value from a specific address.
    - **Operation:** `acc <- acc * mem[<address>]` and set `C` and `V` flags.

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

- **Clear Carry**
    - **Syntax:** `clc`
    - **Description:** Clear the carry flag.
    - **Operation:** `carry <- 0`

### Bitwise Instructions

- **Shift Left**
    - **Syntax:** `shiftl <address>`
    - **Description:** Shift the accumulator left by a number of bits from a specific address (Carry flag will not be used).
    - **Operation:** `acc <- acc << mem[<address>]`

- **Shift Right**
    - **Syntax:** `shiftr <address>`
    - **Description:** Shift the accumulator right by a number of bits from a specific address, preserving the sign (Carry flag will not be used).
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
    - **Syntax:** `bgtz <address>`
    - **Description:** Jump to a specific address if the accumulator is greater than zero.
    - **Operation:** `if acc > 0 then pc <- <address>`

- **Branch if Greater Than or Equal to Zero**
    - **Syntax:** `bgez <address>`
    - **Description:** Jump to a specific address if the accumulator is greater than or equal to zero.
    - **Operation:** `if acc >= 0 then pc <- <address>`

- **Branch if Less Than Zero**
    - **Syntax:** `bltz <address>`
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

### Why we don't have

- **Add Immediate**
    - **Syntax:** `addi <const>`
    - **Description:** Add an immediate value to the accumulator.
    - **Operation:** `acc <- acc + <const>`
    - Why not: if you have `const_1`, why not just `add const_1`?
- **Load Indirect**
    - **Syntax:** `load_ind <address>`
    - **Description:** Load a value from memory using an indirect address.
    - **Operation:** `acc <- mem[mem[<address>]]`
    - Why not: `load_addr` + `load_acc` looks more natural for accumulator
- **Compare**
    - **Syntax:** `cmp <address>`
    - **Description:** Compare the accumulator and set only flags.
    - **Operation:** `acc - mem[<address>]` and set `C` and `V` flags
    - Why not: create a lot of inconsistent states
