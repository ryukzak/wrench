    .text

_start:
    ;; should not be simulated due to memory errors, but should compile with cropped values
    load_addr    0x12345678
    load         0x12345678
    halt
