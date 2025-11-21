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
    - [Documentation](#documentation)
    - [How to Run](#how-to-run)
        - [Build Locally](#build-locally)
        - [Install from a Binary Release](#install-from-a-binary-release)
        - [Via Docker Image](#via-docker-image)
        - [Use it as a Service](#use-it-as-a-service)
    - [Usage](#usage)
    - [Examples](#examples)
        - [Factorial Calculation Example (RISC-IV)](#factorial-calculation-example-risc-iv)
        - [More Examples](#more-examples)

<!-- markdown-toc end -->

## Documentation

- [General Assembly Documentation](./docs/README.md) -- Explanation of how assembly source code and simulation configuration files should be structured (ISA-agnostic)
- Architecture specific documentation:
    - [Acc32](./docs/acc32.md) -- Accumulator-based 32-bit architecture
    - [F32a](./docs/f32a.md) -- Stack-based 32-bit architecture
    - [M68k](./docs/m68k.md) -- Motorola 68000-inspired architecture
    - [RISC-IV](./docs/risc-iv.md) -- RISC-V-inspired 32-bit architecture
    - [VLIW-IV](./docs/vliw-iv.md) -- RISC-V-inspired VLIW 32-bit architecture

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
docker run -it --rm ryukzak/wrench:latest wrench --help
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
Usage: wrench INPUT --isa ISA [-c|--conf CONF] [-S] [-v|--verbose]
              [--instruction-limit LIMIT] [--memory-limit SIZE]
              [--state-log-limit LIMIT]

  App for laboratory course of computer architecture.

Available options:
  INPUT                    Input assembler file (.s)
  --isa ISA                ISA (risc-iv-32, f32a, acc32, m68k, vliw-iv)
  -c,--conf CONF           Configuration file (.yaml)
  -S                       Only run preprocess and translation steps
  -v,--verbose             Verbose output
  --instruction-limit LIMIT
                           Maximum number of instructions to execute
                           (default: 8000000)
  --memory-limit SIZE      Maximum memory size in bytes (default: 8192)
  --state-log-limit LIMIT  Maximum number of state records to log
                           (default: 10000)
  -h,--help                Show this help text
  --version                Show version information
```

The `wrench` app requires an input assembler file and optionally a configuration file. The assembler file should contain the source code in the ISA-specific assembly language. The configuration file is a YAML file that specifies various settings and parameters for the simulation. Alternatively, you can specify execution limits directly via command-line arguments.

See our [documentation](./docs/README.md) for detailed information about:

- Generic assembly structure
- Configuration file format and options
- Architecture-specific details

## Examples

### Factorial Calculation Example (RISC-IV)

Task: Calculate the factorial of a number `n` (`n!`) in RISC-IV architecture.

- Input: Read `n` from memory-mapped I/O address 0x80
- Output: Write the result to memory-mapped I/O address 0x84
- Source Code: [factorial.s](./example/risc-iv-32/factorial.s)
- Configuration: [factorial-5.yaml](./example/risc-iv-32/factorial-5.yaml)
- Run the example:

    ```shell
    # Translation only
    stack exec wrench -- example/risc-iv-32/factorial.s -c example/risc-iv-32/factorial-5.yaml -S

    # Full simulation
    stack exec wrench -- example/risc-iv-32/factorial.s -c example/risc-iv-32/factorial-5.yaml
    ```

### More Examples

For more examples and test cases, see:

- [Example directory](./example/) - Contains documented example programs
- [Test golden directory](./test/golden) - Contains test cases with expected outputs
