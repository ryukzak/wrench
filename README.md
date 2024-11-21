# Wrench

This is an educational project designed to explore different types of processor architectures. It includes simple CPU models and assemblers for them.

## Supported ISAs

### RISC-V-like 32-bit

- See [RiscIv.hs](./src/Isa/RiscIv.hs) for the `data Risc` definition.
- For usage examples, check [Spec.hs](./test/Spec.hs).

## Build Locally

1. Clone the repository.
2. Install Haskell Stack via [GHCup](https://www.haskell.org/ghcup/).
3. Run `stack build` to build the project.
4. You have two options to run the project:
    - Run `stack exec wrench -- <ARGS>` to execute the project without installation.
    - Install the project with `stack install` to run it from the command line using `wrench <ARGS>`.

## Install from a Binary Release

1. Open the last master build on the [Actions](https://github.com/ryukzak/wrench/actions).
2. Download the binary for your platform.
3. Add the binary to your `PATH`.
4. Run `wrench <ARGS>` to execute the project.

## Use it as a Service

This service will be used to send laboratory works to check.

1. Open [wrench.edu.swampbuds.me](wrench.edu.swampbuds.me).
2. Fill the form and submit.
3. Check the results.

## Usage

```shell
Usage: wrench [INPUT] [--isa ISA] [-c|--conf FILENAME] [-S] [-v|--verbose]

  App for laboratory course of computer architecture.

Available options:
  INPUT                    Input assembler file (.s)
  --isa ISA                ISA (default: "risc-v-32-like")
  -c,--conf FILENAME       Configuration file
  -S                       Only run preprocess and translation steps
  -v,--verbose             Verbose output
  -h,--help                Show this help text
  --version                Show version information
```

## Configuration

The configuration file is a YAML file that specifies various settings and parameters for the simulation. Below is a detailed explanation of the fields and their usage.

### Fields

#### `limit`

- **Type:** Integer
- **Description:** Specifies the maximum number of instructions the simulation can execute. If the simulation exceeds this limit, it will be terminated.
- **Example:**

  ```yaml
  limit: 40
  ```

#### `memorySize`

- **Type:** Integer
- **Description:** Specifies the memory size.
- **Example:**

  ```yaml
  memorySize: 40
  ```

#### `input_streams`

- **Type:** Map of decimal or hexadecimal addresses to lists of inputs
- **Description:** Defines the memory-mapped IO streams for the simulation. Each key is a memory address, and the value is a list of inputs that should be fed into the simulation at that address. To define an output port only, leave the list empty.
- **Example:**

  ```yaml
  input_streams:
    0x80: [5]
    132: []
  ```

#### `reports`

- **Type:** List of report configurations
- **Description:** Specifies the reports to generate during the simulation. Each report configuration includes settings such as the name, slice, filter, inspector, and assertions.
- **Example:**

  ```yaml
  reports:
    - name: Step-by-step log
      slice: all
      filter:
        - instruction
  ```

### Report Configuration

Each report configuration can include the following fields:

#### `name`

- **Type:** String (optional)
- **Description:** The name of the report, used as a header in the generated output.
- **Example:**

  ```yaml
  name: Step-by-step log
  ```

#### `slice`

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

#### `filter`

- **Type:** List of strings
- **Description:** Specifies the types of records to include in the report. Possible values are:
    - `"instruction"`: Include instruction records.
    - `"state"`: Include state records.
- **Example:**

  ```yaml
  filter:
    - instruction
    - state
  ```

#### `inspector`

- **Type:** List of lists
- **Description:** Specifies how to inspect and format the state records. Each inspector is a list of tokens that define what to include in the report. Upper-level lists represent different lines in the report, and inner lists represent tokens in each line. Possible tokens are:
    - `[label, name]` or `name`: Include a label with the given text (`name`).
    - `[register, name]`: Include the value of the register with the given name.
    - `[memory_cells, from, to]`: Include the value of the memory at the given address.
    - `[number_io_stream, address]`: Include the number of tokens in the input stream at the given address.
    - `[symbol_io_stream, address]`: Include the symbols in the input stream at the given address.
- **Example:**

  ```yaml
  inspector:
    - - [label, T0]
      - [register, T0]
    - - [memory_cells, 0x80, 0x84]
    - [[label, "0x80"], [number_io_stream, 0x0080]]
    - [[label, "0x84"], [number_io_stream, 0x0084]]
  ```

#### `assert`

- **Type:** String (optional)
- **Description:** Specifies the expected final state of the simulation. If the actual final state does not match, an assertion failure will be reported.
- **Example:**

  ```yaml
  assert: |
    numio[0x80]: [] >>> []
    numio[0x84]: [] >>> [120]
  ```

## Example

Task: Factorial computation in RISC-V-like 32-bit ISA: `n!`.

- `n` should be read from the input at address 0x80.
- The result should be written to the output at address 0x84.

Results:

- Source code with comments: [factorial.s](./example/risc-iv-32/factorial.s) (RISC).
- Configuration file: [factorial-5.yaml](./example/risc-iv-32/factorial-5.yaml), which:
    - Limits the simulation to 40 instructions.
    - Defines memory-mapped IO:
        - `0x80`: Input token for `n = 5`.
        - `0x84`: Output without any tokens for reading.
    - Defines the following reports:
        - `Step-by-step log`, which is useful for debugging as it includes instructions and register states.
        - `Check results`, which verifies the processor's state on the last step and compares it with the expected outcome.

To see the translation result:

```shell
stack exec wrench -- example/risc-iv-32/factorial.s -c example/risc-iv-32/factorial-5.yaml -S
```

To execute the simulation and see reports:

```shell
stack exec wrench -- example/risc-iv-32/factorial.s -c example/risc-iv-32/factorial-5.yaml
```

For other results see [./test/golden](./test/golden) directory.
