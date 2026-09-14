    ;; Demonstrates the default memory initialization: bytes not covered
    ;; by any section are filled with pseudo-random data instead of
    ;; zeros, unless `zero_memory_init: true` is set in the config.
    .data

marker: .word 0xDEADBEEF   ; explicitly initialized -- never randomized

    .text

_start:
    halt
