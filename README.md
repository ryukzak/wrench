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

## Use by docker image

```shell
docker run -it --rm ryukzak/wrench:latest --help
```

## Use it as a Service

This service will be used to send laboratory works to check.

1. Open [wrench.edu.swampbuds.me](wrench.edu.swampbuds.me).
2. Fill the form and submit.
3. Check the results.

## Usage

```shell
$ wrench --help
Usage: wrench INPUT [--isa ISA] (-c|--conf CONF) [-S] [-v|--verbose]

  App for laboratory course of computer architecture.

Available options:
  INPUT                    Input assembler file (.s)
  --isa ISA                ISA (risc-iv-32, f32a, acc32) (default: "risc-iv-32")
  -c,--conf CONF           Configuration file (.yaml)
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
      view: |
        {pc}:	{instruction}	{pc:label}
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

#### `view`

Text template to print log record. In template a user can use state view in curly brackets. E.g.: `program counter: {pc}`.

General state view implemented for all ISA:

- `pc:dec`, `pc:hex` -- print program counter in dec or hex format.
- `pc:label` -- print `@label-name` if current program counter assigned with label.
- `instruction` -- print current instruction.
- `memory:<a>:<b>` -- print memory dump between `<a>` and `<b>` address.
- `io:<a>:dec`, `io:<a>:sym`, `io:<a>:hex` -- print input-output stream state for the specific address in dec, sym or hex format.

##### RiscIv Specific State Views

All registers in `dec` or `hex` format. Registers: `Zero`, `Ra`, `Sp`, `Gp`, `Tp`, `T0`, `T1`, `T2`, `S0Fp`, `S1`, `A0`, `A1`, `A2`, `A3`, `A4`, `A5`, `A6`, `A7`, `S2`, `S3`, `S4`, `S5`, `S6`, `S7`, `S8`, `S9`, `S10`, `S11`, `T3`, `T4`, `T5`, `T6`.

##### F32a Specific State Views

- `A:dec`, `A:hex` -- `A` register.
- `B:dec`, `B:hex` -- `B` register.
- `T:dec`, `T:hex` -- top of the stack.
- `S:dec`, `S:hex` -- second of the stack.
- `R:dec`, `R:hex` -- top of the return stack.
- `stack:dec`, `stack:hex` -- the stack.
- `rstack:dec`, `rstack:hex` -- the return stack.

##### Acc32 Specific State Views

- `Acc:dec`, `Acc:hex` -- `Acc` register.

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
