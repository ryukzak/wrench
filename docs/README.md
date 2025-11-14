# Wrench Documentation

Wrench is an educational project designed to explore different types of processor architectures. This documentation explains how to write assembly code and configure simulations in a way that works across all supported Instruction Set Architectures (ISAs).

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Wrench Documentation](#wrench-documentation)
  - [File Types and Formats](#file-types-and-formats)
  - [Supported Architectures](#supported-architectures)
  - [Assembly Program Structure](#assembly-program-structure)
    - [Comments](#comments)
    - [Labels](#labels)
    - [Data Section](#data-section)
    - [Text Section](#text-section)
    - [Setting Section Addresses](#setting-section-addresses)
  - [Configuration Files](#configuration-files)
    - [Configuration File Structure](#configuration-file-structure)
    - [Configuration Options](#configuration-options)
      - [`limit`](#limit)
      - [`memory_size`](#memory_size)
      - [`memory_mapped_io`](#memory_mapped_io)
      - [`reports`](#reports)
        - [`name`](#name)
        - [`slice`](#slice)
        - [`view`](#view)
        - [`assert`](#assert)

<!-- markdown-toc end -->

## File Types and Formats

Wrench uses several types of files:

- **Assembly files** (`.s` extension) - Source code written in ISA-specific assembly language
- **Configuration files** (`.yaml` extension) - YAML files that control simulation parameters
- **Output** - Results and reports are printed to stdout (standard output)

For examples of fully working programs, see:

- [Example directory](/example)
- [Test golden directory](/test/golden)

## Supported Architectures

Wrench currently supports the following architectures, each with its own instruction set and semantics:

- [Acc32](./acc32.md) - Accumulator-based 32-bit architecture
- [F32a](./f32a.md) - Stack-based 32-bit architecture
- [M68k](./m68k.md) - Motorola 68000-inspired architecture
- [RISC-IV](./risc-iv.md) - RISC-V-inspired 32-bit architecture
- [VLIW-IV](./vliw-iv.md) - RISC-V-inspired VLIW 32-bit architecture

## Assembly Program Structure

Assembly programs in Wrench follow a common structure regardless of the specific ISA being used. This section describes the general components that all assembly programs should have.

Assembly programs consist of **data** (containing variables, constants, and other data) and **text** (instructions) sections that can be defined in any order and multiple times throughout the file. The assembler will merge all sections of the same type into a single program. All assembly programs should be saved with a `.s` file extension.

### Comments

Comments are used to add explanatory notes to your code. The comment character may vary depending on the ISA:

- Most architectures use `;` for comments
- Some architectures may use other characters (check the specific ISA documentation)

Everything after the comment character on the same line is ignored by the assembler:

```assembly
    load_addr input_addr      ; This is a comment explaining the instruction
```

### Labels

Labels are used to mark specific locations in your code or data section. They serve as symbolic names for memory addresses and make your code more readable and maintainable.

- Labels are defined by a name followed by a colon `:`
- All labels defined in the program must be unique
- Labels can be used in instructions or data declarations to reference memory locations

```assembly
loop_start:       ; This defines a label called "loop_start"
    instruction
    instruction
    jmp loop_start ; Jump back to the label
```

### Data Section

The data section is where you define variables, constants, and other data used by your program. It starts with the `.data` directive.

A data line in the data section typically consists of a label, a directive, and one or more values:

```assembly
label:    directive    value(s)
```

- **label**: An identifier followed by a colon `:` that marks the memory location
- **directive**: Specifies the type and size of data being defined
- **value(s)**: The actual data to be stored (multiple values can be separated by commas)

Common data directives include:

- `.word` - Define word-sized data (typically 32 bits)
- `.byte` - Define byte-sized data (8 bits)

Examples:

```assembly
.data
counter:        .word 0        ; Define a word-sized variable initialized to 0
message:        .byte 'H', 'e', 'l', 'l', 'o', 0  ; Null-terminated string
large_number:   .word 0xFFFFFFFF  ; Hexadecimal value
neg_one:        .word -1       ; Negative value
c_string:       .byte 'Hello, World!\0'  ; C-style string with null terminator
pascal_string:  .byte 13, 'Hello, World!'  ; Pascal-style string with length prefix
```

<!-- TODO: Add space directive support
- `.space` - Reserve a block of memory with specified size
buffer:         .space 64      ; Reserve 64 bytes of space
-->

### Text Section

The text section contains the executable code (instructions) of your program. It starts with the `.text` directive.

The specific instructions available depend on the ISA you're using, but the general structure is:

```assembly
.text
label:                    ; Optional label
    instruction operands  ; Instruction with its operands
    instruction operands
    ...
```

For ISA-specific details (instructions, registers, etc.), refer to the respective documentation:

- [Acc32](./acc32.md) - Accumulator-based architecture
- [RISC-IV](./risc-iv.md) - RISC-V-inspired architecture
- [F32a](./f32a.md) - Stack-based architecture
- [M68k](./m68k.md) - Motorola 68000-inspired architecture
- [VLIW-IV](./vliw-iv.md) - RISC-V-inspired VLIW 32-bit architecture

If you can't find certain details in the documentation, the [source code](/src/wrench/Wrench/Isa) contains the most up-to-date information.

### Setting Section Addresses

The `.org` directive sets the starting address for a section. This is useful when you need to place code or data at specific memory addresses:

```assembly
.data
.org 0x1000       ; Data section starts at address 0x1000
value:  .word 42

.text
.org 0x2000       ; Text section starts at address 0x2000
_start:
    load_ind value
```

If `.org` is not specified, the assembler will automatically assign addresses starting from 0x0000 for the first section and place other sections sequentially.

## Configuration Files

Wrench uses YAML configuration files (`.yaml` extension) to control simulation parameters. These files allow you to:

1. Set execution limits
2. Define input/output streams
3. Configure memory
4. Generate and check reports

### Configuration File Structure

A configuration file typically contains the following sections:

```yaml
# Execution limits
limit: 1000            # Maximum instructions to execute
memory_size: 8192      # Memory size in bytes

# I/O configuration
memory_mapped_io:
  0x80: [5, 6, 7]      # Input values at address 0x80
  0x84: []             # Output-only port at address 0x84

# Reports configuration
reports:
  - name: "Execution trace"
    slice: all
    view: |
      {pc}: {instruction} {pc:label}

  - name: "Result verification"
    slice: last
    view: |
      numio[0x84]: {io:0x84:dec}
    assert: |
      numio[0x84]: [] >>> [120]
```

### Configuration Options

#### `limit`

- **Type:** Integer
- **Description:** Specifies the maximum number of instructions the simulation can execute. If the simulation exceeds this limit, it will be terminated.
- **CLI Override:** `--instruction-limit LIMIT` sets the upper limit for that option.
- **Example:**

  ```yaml
  limit: 40
  ```

#### `memory_size`

- **Type:** Integer
- **Description:** Specifies the memory size in bytes.
- **CLI Override:** `--memory-limit SIZE` sets the upper limit for that option.
- **Example:**

  ```yaml
  memory_size: 40
  ```

#### `memory_mapped_io`

- **Type:** Map of decimal or hexadecimal addresses to lists of inputs
- **Description:** Defines the memory-mapped IO streams for the simulation. Each key is a memory address, and the value is a list of inputs that should be fed into the simulation at that address. To define an output port only, leave the list empty.
- **Example:**

  ```yaml
  memory_mapped_io:
    0x80: [5]  # Input port at address 0x80 with value 5
    132: []    # Output-only port at address 132 (decimal)
  ```

#### `reports`

- **Type:** List of report configurations
- **Description:** Specifies the reports to generate during the simulation. Each report configuration includes settings such as the name, slice, filter, inspector, and assertions.
- **Example:**

  ```yaml
  reports:
    - name: Step-by-step log
      slice: all
      view: |
        {pc}: {instruction} {pc:label}
  ```

Each report configuration can include the following fields:

##### `name`

- **Type:** String (optional)
- **Description:** The name of the report, used as a header in the generated output.
- **Example:**

  ```yaml
  name: Step-by-step log
  ```

##### `slice`

- **Type:** String or List
- **Description:** Specifies which part of the simulation records should be included in the report. Possible values are:
    - `"all"`: Include all records.
    - `["head", n]`: Include the first `n` records.
    - `["tail", n]`: Include the last `n` records.
    - `"last"`: Include only the last record.
- **Example:**

  ```yaml
  slice: all
  ```

##### `view`

- **Type:** String (template)
- **Description:** Text template to print log records. In the template, you can use state view expressions in curly brackets.
- **Example:** `program counter: {pc}`

General state view expressions implemented for all ISAs:

- `pc:dec`, `pc:hex` -- Print program counter in decimal or hexadecimal format.
- `pc:label` -- Print `@label-name` if current program counter is assigned with a label.
- `instruction` -- Print current instruction.
- `memory:<a>:<b>` -- Print memory dump between addresses `<a>` and `<b>`.
- `io:<a>:dec`, `io:<a>:sym`, `io:<a>:hex` -- Print input-output stream state for the specific address in decimal, symbol, or hexadecimal format. Printable char codes: [32, 126]. Also `\0`, `\n` will be printed as is. Other non-printable characters will be replaced with `?`.

For ISA-specific state views, see the respective architecture documentation.

##### `assert`

- **Type:** String (optional)
- **Description:** Specifies the expected final state of the simulation. If the actual final state does not match, an assertion failure will be reported.
- **Example:**

  ```yaml
  assert: |
    numio[0x80]: [] >>> []
    numio[0x84]: [] >>> [120]
  ```
