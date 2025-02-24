    .data
max_pos:         .word  0x7fff_ffff
max_word:        .word  0xffff_ffff
const_1:         .word  1
const_neg_1:     .word  -1


    .text

_start:
    ;; should not be simulated due to memory errors, but should compile with cropped values
    load         max_pos
    add          const_1
    bvs          clean_overflow
    halt                                     ;; should be skipped
clean_overflow:
    clv

    add          const_neg_1
    clv
    clc

    load         max_word
    add          const_1
    bcs          clean_carry
    halt

clean_carry:
    clc

    halt
