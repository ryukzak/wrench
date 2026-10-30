# Report views

<!-- Generated from the source by `make test-accept`. Do not edit. -->

Every expression a report `view` can use inside `{...}`. `[...]` is optional, `a|b` is a choice, `<name>` is a number (decimal or `0x` hex). See [Reports configuration](README.md#reports-configuration).

## All ISAs

| View                                     | Description                                                                      |
| ---------------------------------------- | -------------------------------------------------------------------------------- |
| `{pc[:dec\|hex]}`                        | Program counter.                                                                 |
| `{pc:label}`                             | `@label` if the program counter is at a label, otherwise empty.                  |
| `{instruction:prev}`                     | The instruction that produced this state, `-` for the initial state.             |
| `{instruction:next}`                     | The instruction to execute next, `-` once halted.                                |
| `{sim:instruction-count[:dec\|hex]}`     | Instructions executed so far.                                                    |
| `{mem:accesses[:dec\|hex]}`              | Memory accesses of the instruction that produced this state, its fetch included. |
| `{mem:reads[:dec\|hex]}`                 | Reads among `mem:accesses`; a fetch is a read.                                   |
| `{mem:writes[:dec\|hex]}`                | Writes among `mem:accesses`.                                                     |
| `{mem:accesses-total[:dec\|hex]}`        | Memory accesses of the run so far.                                               |
| `{mem:reads-total[:dec\|hex]}`           | Reads of the run so far.                                                         |
| `{mem:writes-total[:dec\|hex]}`          | Writes of the run so far.                                                        |
| `{layout:sections-size[:dec\|hex]}`      | Bytes in all sections.                                                           |
| `{layout:text-sections-size[:dec\|hex]}` | Bytes in `.text` sections.                                                       |
| `{layout:data-sections-size[:dec\|hex]}` | Bytes in `.data` sections.                                                       |
| `{layout:text-ranges[:dec\|hex]}`        | Address ranges of `.text` sections.                                              |
| `{layout:data-ranges[:dec\|hex]}`        | Address ranges of `.data` sections.                                              |
| `{mem:instr-ranges[:dec\|hex]}`          | Address ranges the run fetched instructions from.                                |
| `{mem:data-ranges[:dec\|hex]}`           | Address ranges the run read or wrote data at.                                    |
| `{mem:io-ranges[:dec\|hex]}`             | Memory-mapped IO addresses the run accessed.                                     |
| `{mem:table}`                            | Address-space table: sections, IO, free spans and their access coverage.         |
| `{profile}`                              | How often each instruction ran, with its reads and writes, over the whole run.   |
| `{mem:dump:<from>:<to>}`                 | Memory dump from `<from>` to `<to>`, inclusive.                                  |
| `{io:<addr>[:dec\|hex\|sym]}`            | IO port at `<addr>`: `input >>> output` so far; `sym` shows characters.          |
| `{isa-specific}`                         | The ISA's run-wide stats, one `name: value` per line; empty if it has none.      |

## acc32

| View               | Description                |
| ------------------ | -------------------------- |
| `{Acc[:dec\|hex]}` | Accumulator.               |
| `{V}`              | Overflow flag, `1` or `0`. |
| `{C}`              | Carry flag, `1` or `0`.    |

## f32a

| View                      | Description                                                  |
| ------------------------- | ------------------------------------------------------------ |
| `{A[:dec\|hex]}`          | Register `A`.                                                |
| `{B[:dec\|hex]}`          | Register `B`.                                                |
| `{T[:dec\|hex]}`          | Top of the data stack, `0` if empty.                         |
| `{S[:dec\|hex]}`          | Second on the data stack, `0` if absent.                     |
| `{R[:dec\|hex]}`          | Top of the return stack, `0` if empty.                       |
| `{stack[:dec\|hex]}`      | Data stack, top first: `a:b:c`.                              |
| `{rstack[:dec\|hex]}`     | Return stack, top first: `a:b:c`.                            |
| `{EAM}`                   | Extended arithmetic mode, `1` or `0`.                        |
| `{C}`                     | Carry flag, `1` or `0`.                                      |
| `{f32a:data-stack-max}`   | Deepest data stack of the run. Listed in `{isa-specific}`.   |
| `{f32a:return-stack-max}` | Deepest return stack of the run. Listed in `{isa-specific}`. |

## m68k

| View              | Description                           |
| ----------------- | ------------------------------------- |
| `{SR[:bin]}`      | Status register flags `NZVC` as bits. |
| `{D0[:dec\|hex]}` | Data register `D0`.                   |
| `{D1[:dec\|hex]}` | Data register `D1`.                   |
| `{D2[:dec\|hex]}` | Data register `D2`.                   |
| `{D3[:dec\|hex]}` | Data register `D3`.                   |
| `{D4[:dec\|hex]}` | Data register `D4`.                   |
| `{D5[:dec\|hex]}` | Data register `D5`.                   |
| `{D6[:dec\|hex]}` | Data register `D6`.                   |
| `{D7[:dec\|hex]}` | Data register `D7`.                   |
| `{A0[:dec\|hex]}` | Address register `A0`.                |
| `{A1[:dec\|hex]}` | Address register `A1`.                |
| `{A2[:dec\|hex]}` | Address register `A2`.                |
| `{A3[:dec\|hex]}` | Address register `A3`.                |
| `{A4[:dec\|hex]}` | Address register `A4`.                |
| `{A5[:dec\|hex]}` | Address register `A5`.                |
| `{A6[:dec\|hex]}` | Address register `A6`.                |
| `{A7[:dec\|hex]}` | Address register `A7`.                |

## risc-iv-32

| View                                 | Description         |
| ------------------------------------ | ------------------- |
| `{zero[:dec\|hex]}`                  | Register `zero`.    |
| `{ra[:dec\|hex]}`                    | Register `ra`.      |
| `{sp[:dec\|hex]}`                    | Register `sp`.      |
| `{gp[:dec\|hex]}`                    | Register `gp`.      |
| `{tp[:dec\|hex]}`                    | Register `tp`.      |
| `{t0[:dec\|hex]}`                    | Register `t0`.      |
| `{t1[:dec\|hex]}`                    | Register `t1`.      |
| `{t2[:dec\|hex]}`                    | Register `t2`.      |
| `{s0[:dec\|hex]}`, `{fp[:dec\|hex]}` | Register `s0`/`fp`. |
| `{s1[:dec\|hex]}`                    | Register `s1`.      |
| `{a0[:dec\|hex]}`                    | Register `a0`.      |
| `{a1[:dec\|hex]}`                    | Register `a1`.      |
| `{a2[:dec\|hex]}`                    | Register `a2`.      |
| `{a3[:dec\|hex]}`                    | Register `a3`.      |
| `{a4[:dec\|hex]}`                    | Register `a4`.      |
| `{a5[:dec\|hex]}`                    | Register `a5`.      |
| `{a6[:dec\|hex]}`                    | Register `a6`.      |
| `{a7[:dec\|hex]}`                    | Register `a7`.      |
| `{s2[:dec\|hex]}`                    | Register `s2`.      |
| `{s3[:dec\|hex]}`                    | Register `s3`.      |
| `{s4[:dec\|hex]}`                    | Register `s4`.      |
| `{s5[:dec\|hex]}`                    | Register `s5`.      |
| `{s6[:dec\|hex]}`                    | Register `s6`.      |
| `{s7[:dec\|hex]}`                    | Register `s7`.      |
| `{s8[:dec\|hex]}`                    | Register `s8`.      |
| `{s9[:dec\|hex]}`                    | Register `s9`.      |
| `{s10[:dec\|hex]}`                   | Register `s10`.     |
| `{s11[:dec\|hex]}`                   | Register `s11`.     |
| `{t3[:dec\|hex]}`                    | Register `t3`.      |
| `{t4[:dec\|hex]}`                    | Register `t4`.      |
| `{t5[:dec\|hex]}`                    | Register `t5`.      |
| `{t6[:dec\|hex]}`                    | Register `t6`.      |

## vliw-iv

| View                     | Description                                                                              |
| ------------------------ | ---------------------------------------------------------------------------------------- |
| `{zero[:dec\|hex]}`      | Register `zero`.                                                                         |
| `{ra[:dec\|hex]}`        | Register `ra`.                                                                           |
| `{sp[:dec\|hex]}`        | Register `sp`.                                                                           |
| `{gp[:dec\|hex]}`        | Register `gp`.                                                                           |
| `{tp[:dec\|hex]}`        | Register `tp`.                                                                           |
| `{t0[:dec\|hex]}`        | Register `t0`.                                                                           |
| `{t1[:dec\|hex]}`        | Register `t1`.                                                                           |
| `{t2[:dec\|hex]}`        | Register `t2`.                                                                           |
| `{s0fp[:dec\|hex]}`      | Register `s0fp`.                                                                         |
| `{s1[:dec\|hex]}`        | Register `s1`.                                                                           |
| `{a0[:dec\|hex]}`        | Register `a0`.                                                                           |
| `{a1[:dec\|hex]}`        | Register `a1`.                                                                           |
| `{a2[:dec\|hex]}`        | Register `a2`.                                                                           |
| `{a3[:dec\|hex]}`        | Register `a3`.                                                                           |
| `{a4[:dec\|hex]}`        | Register `a4`.                                                                           |
| `{a5[:dec\|hex]}`        | Register `a5`.                                                                           |
| `{a6[:dec\|hex]}`        | Register `a6`.                                                                           |
| `{a7[:dec\|hex]}`        | Register `a7`.                                                                           |
| `{s2[:dec\|hex]}`        | Register `s2`.                                                                           |
| `{s3[:dec\|hex]}`        | Register `s3`.                                                                           |
| `{s4[:dec\|hex]}`        | Register `s4`.                                                                           |
| `{s5[:dec\|hex]}`        | Register `s5`.                                                                           |
| `{s6[:dec\|hex]}`        | Register `s6`.                                                                           |
| `{s7[:dec\|hex]}`        | Register `s7`.                                                                           |
| `{s8[:dec\|hex]}`        | Register `s8`.                                                                           |
| `{s9[:dec\|hex]}`        | Register `s9`.                                                                           |
| `{s10[:dec\|hex]}`       | Register `s10`.                                                                          |
| `{s11[:dec\|hex]}`       | Register `s11`.                                                                          |
| `{t3[:dec\|hex]}`        | Register `t3`.                                                                           |
| `{t4[:dec\|hex]}`        | Register `t4`.                                                                           |
| `{t5[:dec\|hex]}`        | Register `t5`.                                                                           |
| `{t6[:dec\|hex]}`        | Register `t6`.                                                                           |
| `{vliw:load-percent}`    | Average slot use over the run, in percent of the four slots. Listed in `{isa-specific}`. |
| `{vliw:avg-load}`        | Average active slots per executed bundle, `0.00`-`4.00`. Listed in `{isa-specific}`.     |
| `{vliw:bundles-by-load}` | Executed bundles by active slot count: `K:N (P%)`. Listed in `{isa-specific}`.           |

## wasm32

| View                                   | Description                                                                                            |
| -------------------------------------- | ------------------------------------------------------------------------------------------------------ |
| `{stack[:dec\|hex]}`                   | Operand stack of the current frame, top first: `a:b:c`.                                                |
| `{locals[:dec\|hex]}`                  | Locals of the current frame, `$0` first.                                                               |
| `{stack:layout[:dec\|hex][:<frames>]}` | Call frames, outermost first: locals, open scopes, operands; `<frames>` keeps only the innermost ones. |
| `{stack:dump[:dec\|hex][:<frames>]}`   | Stack memory word by word, by address, with pointer tags; `<frames>` keeps only the innermost frames.  |
| `{sp[:dec\|hex]}`                      | Address of the topmost operand.                                                                        |
| `{frame-base[:dec\|hex]}`              | Address of local 0 of the current frame.                                                               |
| `{ctrl-sp[:dec\|hex]}`                 | Top of the control stack.                                                                              |
| `{stack-root[:dec\|hex]}`              | Where both stacks start, set by `sp.init`.                                                             |
| `{cache:scan[:dec\|hex]}`              | The last four `else`/`end` scans, most recent first: `start->end`.                                     |
