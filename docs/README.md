# Wrench Assembler

This assembler supports multiple Instruction Set Architectures (ISAs) and provides a flexible way to write and translate assembly code for different machine architectures.

## Writing Assembly Programs

Data and text sections can be defined in any order and multiple times. The assembler will merge them into a single program (dump).

### Comments

Comments can be added using the `;` or `\` character (depends on ISA). Everything after that symbol on the same line is considered a comment.

Example:

```assembly
    load_ind        input_addr       ; Load the value from input address
```

### Labels

Labels are used to mark locations in the code or data section. They are defined by a name followed by a colon `:`. All labels defined in the code must be unique.

### Data Section

The data section is used to define variables, constants, and memory addresses. It starts with the `.data` directive.

A data line in the data section typically consists of a label, a directive, and one or more values. The syntax is as follows:

```test
label:    directive    value(s)
```

- **label**: An identifier followed by a colon `:`. It marks the memory location where the data is stored.
- **directive**: Specifies the type of data being defined. Common directives include `.word` for word-sized data and `.byte` for byte-sized data.
- **value(s)**: The actual data to be stored at the label's memory location. Multiple values can be separated by commas.

Example:

```assembler
neg_one:         .word -1        ;; Define label and assign value to memory
zero:            .word 0
byte             .byte 0x12
c_string:        .byte 'Hello, World!\0'
pascal_string:   .byte 13, 'Hello, World!'
```

### Text Section

The text section contains the executable code. It starts with the `.text` directive.

Example (Acc32):

```assembly
.text
_start:
    load_ind        input_addr       ; acc <- mem[*input_addr]
    store_addr      n                ; mem[n] <- acc
    ; ... rest of the code
```

To see ISA specific details (instructions, registers, etc.), refer to the respective documentation:

- [Acc32](./acc32.md)
- [RiscIv](./risc-iv.md)
- [F32a](./f32a.md)

If you can't find something in documentation, check the [source code](/src/Isa). It is actually the best and most up-to-date documentation.

For example see:

- [/example](/example)
- [/test/golden](/test/golden)
