    .data

one:             .word  1
minus_one:       .word  -1
thirty_three:    .word  33

    .text

_start:
    load_addr    one
    shiftl       minus_one                   ; -> 1 << 31
    shiftr       thirty_three                ; -> (1 << 31) >> 1
    halt
