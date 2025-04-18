    .data

input_addr:      .word  0x80               ; Input address where the character is stored
output_addr:     .word  0x84               ; Output address where the character should be stored

    .text

_start:
    move.l   input_addr, A0                  ; A0 <- input_addr
    move.l   (A0), A0                        ; A0 <- *input_addr

    move.l   output_addr, A1                 ; A1 <- output_addr
    move.l   (A1), A1                        ; A1 <- *output_addr

    move.b   (A0), D0                        ; D0 <- *input_addr (character)
    move.b   D0, (A1)                        ; *output_addr <- D0 (character)

    halt
