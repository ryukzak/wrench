    .data

input_addr:      .word  0x80               ; Input address where the number 'n' is stored
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text

_start:
    move.l   input_addr, (A0)
    move.l   (A0), D0

    not.l    D0

    move.l   output_addr, (A1)
    move.l   D0, (A1)

    halt
