    .data

input_addr:      .word  0x80               ; Input address where the number 'n' is stored
output_addr:     .word  0x84               ; Output address where the result should be stored
stack_top:       .word  0x200              ; Top of the stack

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
    cmp.l    1, D0
    ble      base_case

    move.l   D0, -(A7)
    sub.l    1, D0
    jsr      factorial
    move.l   (A7)+, D1
    mul.l    D1, D0
    rts

base_case:
    move.l   1, D0
    rts
