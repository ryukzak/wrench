# Wasm32 Instruction Set Architecture (ISA) Documentation

The Wasm32 documentation lives in Haddock, next to the code it describes:

- [**Wrench.Isa.Wasm32**](https://ryukzak.github.io/wrench/Wrench-Isa-Wasm32.html) -- architecture, program structure, the two stacks, locals, control flow, functions, differences from WebAssembly, and the per-instruction reference
- [Wrench.Isa.Wasm32.Instruction](https://ryukzak.github.io/wrench/Wrench-Isa-Wasm32-Instruction.html) -- the instruction type, assembly syntax, field guards and instruction sizes
- [Wrench.Isa.Wasm32.State](https://ryukzak.github.io/wrench/Wrench-Isa-Wasm32-State.html) -- the machine state, the memory map, and both stacks including the control-record format
- [Wrench.Isa.Wasm32.Execute](https://ryukzak.github.io/wrench/Wrench-Isa-Wasm32-Execute.html) -- resolving a loaded program's control flow, then running it
- [Wrench.Isa.Wasm32.Report](https://ryukzak.github.io/wrench/Wrench-Isa-Wasm32-Report.html) -- the ISA-specific report views
