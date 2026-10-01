    .text

_start:
    ;; should not be simulated due to memory errors, but should compile with a cropped value
    load_addr    0x12345678
    halt
