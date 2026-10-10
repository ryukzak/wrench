    .text

_start:
    ;; should not be simulated due to memory errors, but should compile:
    ;; load_addr carries a full machine word, load an 8-bit relative offset
    load_addr    0x12345678
    load         0x7f
    halt
