    .data

input_addr:      .word  0x80
output_addr:     .word  0x84
stack_top:       .word  0x200

    .text

_start:
    movea.l  stack_top, A7
    movea.l  (A7), A7
    movea.l  input_addr, A0
    movea.l  (A0), A0
    movea.l  output_addr, A1
    movea.l  (A1), A1

    move.l   (A0), D0
    jsr      factorial
    move.l   D0, (A1)

    halt

factorial:
    link     A6, 4
    cmp.l    1, D0
    ble      base_case
    move.l   D0, -4(A6)

    sub.l    1, D0
    jsr      factorial

    move.l   -4(A6), D1
    mul.l    D1, D0

    unlk     A6
    rts

base_case:
    move.l   1, D0
    unlk     A6
    rts
