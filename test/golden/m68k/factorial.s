    .data

input_addr:      .word  0x80               ; Input address where the number 'n' is stored
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text

_start:
    movea.l  input_addr, A6
    movea.l  (A6), A6
    movea.l  output_addr, A7
    movea.l  (A7), A7

    move.l   (A6), D0
    beq      factorial_0
    ble      factorial_not_in_domain

    move.l   D0, D1
factorial_while:
    sub.l    1, D1
    beq      factorial_end
    mul.l    D1, D0
    bvs      factorial_overflow
    jmp      factorial_while

factorial_0:
    move.l   1, D0
    jmp      factorial_end
factorial_not_in_domain:
    move.l   0xFFFF_FFFF, D0
    jmp      factorial_end
factorial_overflow:
    move.l   0xCCCC_CCCC, D0
    jmp      factorial_end
factorial_end:
    move.l   D0, (A7)
    halt
