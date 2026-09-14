    ;; Demonstrates random_memory_init: bytes not covered by any section
    ;; are filled with pseudo-random data instead of zeros.
    .data

marker: .word 0xDEADBEEF   ; explicitly initialized -- never randomized

    .text

_start:
    halt
