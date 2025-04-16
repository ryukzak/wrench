# Wrench

![Wrench CI](https://github.com/ryukzak/wrench/actions/workflows/ci.yml/badge.svg?branch=master)

This is an educational project designed to explore different types of processor architectures. It includes simple CPU models and assemblers for them.

- `wrench` -- translator/simulator itself
- `wrench-fmt` -- formatter for assembly files
- `wrench-serv` -- service for uploading and running testcases

Join our development channel: [Zed Channel](https://zed.dev/channel/wrench-20237)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Wrench](#wrench)
    - [How to Run](#how-to-run)
        - [Build Locally](#build-locally)
        - [Install from a Binary Release](#install-from-a-binary-release)
        - [Via Docker Image](#via-docker-image)
        - [Use it as a Service](#use-it-as-a-service)
    - [Usage](#usage)
        - [Assembly File](#assembly-file)
        - [Configuration File](#configuration-file)
            - [`limit`](#limit)
            - [`memory_size`](#memory_size)
            - [`input_streams`](#input_streams)
            - [`reports`](#reports)
                - [`name`](#name)
                - [`slice`](#slice)
                - [`view`](#view)
                - [`assert`](#assert)
    - [Example](#example)

<!-- markdown-toc end -->

## How to Run

### Build Locally

1. Clone the repository.
2. Install Haskell Stack via [GHCup](https://www.haskell.org/ghcup/).
3. Run `stack build` to build the project.
4. You have two options to run the project:
    - Run `stack exec wrench -- <ARGS>` to execute the project without installation.
    - Install the project with `stack install` to run it from the command line using `wrench <ARGS>`.

### Install from a Binary Release

1. Open the last master build on the [Actions](https://github.com/ryukzak/wrench/actions).
2. Download the binary for your platform: windows-x64, linux-x64, linux-arm64, macos-intel, macos-arm64.
3. Add the binary to your `PATH`.
4. Run `wrench <ARGS>` to execute the project.

### Via Docker Image

```shell
docker run -it --rm ryukzak/wrench:latest --help
```

### Use it as a Service

This service will be used to send laboratory works to check.

1. Open service:
    - Last release: [wrench.edu.swampbuds.me](https://wrench.edu.swampbuds.me).
    - Edge version (master branch): [wrench-edge.edu.swampbuds.me](https://wrench-edge.edu.swampbuds.me)
    - Service usage statistics: [PostHog](https://eu.posthog.com/shared/UAxD9XvX9pnOjWOah6l_AHCO36zPnA)
2. Fill the form and submit.
3. Check the results.

## Usage

```shell
$ wrench --help
Usage: wrench INPUT [--isa ISA] [-c|--conf CONF] [-S] [-v|--verbose]
              [--instruction-limit LIMIT] [--memory-limit SIZE]
              [--state-log-limit LIMIT]

  App for laboratory course of computer architecture.

Available options:
  INPUT                    Input assembler file (.s)
  --isa ISA                ISA (risc-iv-32, f32a, acc32, m68k) (default: "risc-iv-32")
  -c,--conf CONF           Configuration file (.yaml)
  -S                       Only run preprocess and translation steps
  -v,--verbose             Verbose output
  --instruction-limit LIMIT
                           Maximum number of instructions to execute
                           (default: 8000000)
  --memory-limit SIZE      Maximum memory size in bytes (default: 8192)
  --state-log-limit LIMIT  Maximum number of state records to log (default: 10000)
  -h,--help                Show this help text
  --version                Show version information
```

The `wrench` app requires an input assembler file and optionally a configuration file. The assembler file should contain the source code in the ISA-specific assembly language. The configuration file is a YAML file that specifies various settings and parameters for the simulation. Alternatively, you can specify execution limits directly via command-line arguments.

### Assembly File

See [docs](/docs) for the specific assembly language for each ISA.

### Configuration File

#### `limit`

- **Type:** Integer
- **Description:** Specifies the maximum number of instructions the simulation can execute. If the simulation exceeds this limit, it will be terminated.
- **CLI Override:** `--instruction-limit LIMIT` limit this configuration option.
- **Example:**

  ```yaml
  limit: 40
  ```

#### `memory_size`

- **Type:** Integer
- **Description:** Specifies the memory size in bytes.
- **CLI Override:** `--memory-limit SIZE` limit this configuration option.
- **Example:**

  ```yaml
  memory_size: 40
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

Text template to print log record. In template a user can use state view in curly brackets. E.g.: `program counter: {pc}`.

General state view implemented for all ISA:

- `pc:dec`, `pc:hex` -- print program counter in dec or hex format.
- `pc:label` -- print `@label-name` if current program counter assigned with label.
- `instruction` -- print current instruction.
- `memory:<a>:<b>` -- print memory dump between `<a>` and `<b>` address.
- `io:<a>:dec`, `io:<a>:sym`, `io:<a>:hex` -- print input-output stream state for the specific address in dec, sym or hex format. Printable char code: [32, 126]. Also `\0`, `\n` will be printed as is. Other non-printable characters will be replaced with `?`, others will raise an error.

For ISA specific state views see [docs](/docs).

##### `assert`

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

You can also run the simulation without a configuration file by specifying limits directly:

```shell
stack exec wrench -- example/risc-iv-32/factorial.s --instruction-limit 100 --memory-limit 1024
```

To see all available command-line options:

```shell
stack exec wrench -- --help
```

For other results see [./test/golden](./test/golden) directory.
