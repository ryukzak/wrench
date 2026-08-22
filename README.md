# Wrench

![Wrench CI](https://github.com/ryukzak/wrench/actions/workflows/ci.yml/badge.svg?branch=master)
![License](https://img.shields.io/github/license/ryukzak/wrench)

Wrench is a teaching platform for computer architecture: one assembler/simulator toolchain shared across five deliberately different CPU paradigms, plus a formatter and a grading service for running real coursework. Every architecture uses the same assembly conventions, YAML-driven configuration, and report/assertion language, so a single lab exercise can be solved once and compared instruction-for-instruction across an accumulator machine, a stack machine, a load-store RISC, a register-memory CISC, a VLIW design, etc.

- `wrench` -- translator/simulator itself
- `wrench-fmt` -- formatter for assembly files
- `wrench-serv` -- service for uploading and running testcases

Join our development channel: [Zed Channel](https://zed.dev/channel/wrench-20237)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Wrench](#wrench)
    - [Why Simplified Architectures?](#why-simplified-architectures)
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

## Why Simplified Architectures?

None of Wrench's ISAs are real hardware. Each is a small, from-scratch design "inspired by" a real family -- RISC-IV by RISC-V, M68k by the Motorola 68000, F32a by the GreenArrays F18a, VLIW-IV by RISC-V, classic VLIW designs, etc. That's deliberate:

- **Right altitude of complexity.** Real ISAs carry decades of backward-compatibility cruft: extension zoos, privileged/CSR specs, addressing-mode edge cases, bundle templates and predication. None of that teaches the underlying paradigm faster; a simplified ISA keeps the concept and drops the incidental history.
- **One toolchain instead of five.** Because Wrench owns every ISA, all five share the same directives, config schema, and report language (see [Documentation](./docs/README.md)). Wrapping five real toolchains instead would mean learning five sets of toolchain quirks, not five architectural paradigms.
- **Deterministic enough to auto-grade.** The variant generator (`script/variants.py`) produces a unique, auto-checkable assignment per student, which requires fully-specified semantics with no inherited hardware errata or undefined behavior.
- **No hardware or licensing barrier.** A classroom doesn't need real 68000s or GreenArrays chips -- the spec is the simulator.
- **A whole ISA fits in one sitting.** Each architecture doc is a few thousand words, not a multi-hundred-page reference manual.

This tradeoff is scoped to teaching -- it isn't a claim that simplified ISAs are better for production compiler work or real hardware bring-up, just that they fit a course better where students should be reasoning about architecture, not toolchain trivia.

| Architecture | Paradigm | Registers | Inspired by |
| ----------------------------- | --------------------------------------- | ------------------------------------ | ------------------ |
| [Acc32](./docs/acc32.md) | Accumulator | 1 (`Acc`) | -- (from scratch) |
| [F32a](./docs/f32a.md) | Stack (dual-stack) | 2 (`A`, `B`) + data/return stacks | [GreenArrays F18a](https://www.greenarraychips.com/home/documents/greg/DB001-221113-F18a.pdf) |
| [RISC-IV](./docs/risc-iv.md) | Load/store RISC | 32 general-purpose | [RISC-V](https://riscv.org/wp-content/uploads/2017/05/riscv-spec-v2.2.pdf) |
| [M68k](./docs/m68k.md) | Register-memory CISC | 8 data + 8 address | [Motorola 68000](https://nguillaumin.github.io/perihelion-m68k-tutorials/appendixes/m68k-instruction-set.txt) |
| [VLIW-IV](./docs/vliw-iv.md) | Static-scheduled VLIW (4-wide bundles) | 32 general-purpose | [RISC-V](https://riscv.org/wp-content/uploads/2017/05/riscv-spec-v2.2.pdf) + classic VLIW |

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
Usage: wrench INPUT --isa ISA [-c|--conf CONF] [-S] [--stats] [-v|--verbose]
              [--instruction-limit LIMIT] [--memory-limit SIZE]
              [--state-log-limit LIMIT]

  App for laboratory course of computer architecture.

Available options:
  INPUT                    Input assembler file (.s)
  --isa ISA                ISA (risc-iv-32, f32a, acc32, m68k, vliw-iv)
  -c,--conf CONF           Configuration file (.yaml)
  -S                       Only run preprocess and translation steps
  --stats                  Append a built-in `Overview` report
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

The [General Assembly Documentation](./docs/README.md) explains how assembly source code and simulation configuration files should be structured (ISA-agnostic), including:

- Generic assembly structure
- Configuration file format and options
- Architecture-specific details

### Execution and memory statistics

Reports can include opt-in stat variables that summarize the run -- instructions executed, declared section sizes, and the address ranges actually touched at runtime. Add them to any report's `view` template (typically with `slice: last`):

```yaml
reports:
    - name: stats
      slice: last
      view: |
        sim:instruction-count: {sim:instruction-count}
        layout:sections-size:  {layout:sections-size}
        mem:instr-ranges:      {mem:instr-ranges}
        mem:data-ranges:       {mem:data-ranges}
        mem:io-ranges:         {mem:io-ranges}
```

Comparing `layout:*-size` against `mem:*-ranges` shows which declared bytes the program actually touched and which addresses it accessed outside any declared section (the stack region is the typical case).

For the same picture in one shot, drop `{memory:table}` into a `view` -- it renders the whole address space as a single table (one row per declared section, IO cluster, or free span) with a `Coverage` column:

```yaml
reports:
    - name: memory-map
      slice: last
      view: |
        {memory:table}
```

The full list of variables, including the byte-count vs. range conventions and the `:dec`/`:hex` suffix on range variables, is in the [configuration documentation](./docs/README.md#view).

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
