    .data
hello:           .byte  31, 'Hello\n\0World!\0\0\0' ; The string to print with newline and null terminators
output_addr:     .word  0x84               ; Output device address

    .text
    .org     0x100
_start:
    movea.l  output_addr, A1                 ; A1 <- address of output device address
    movea.l  (A1), A1                        ; A1 <- value at output_addr (0x84)

    move.l   hello, D0
    move.l   14, D1

loop:
    movea.l  D0, A0
    move.l   (A0), D2                        ; TODO: support for .b
    and.l    0xFF, D2
    move.l   D2, (A1)

    add.l    1, D0
    add.l    -1, D1
    bne      loop

    halt
