    .data

input_addr:      .word  0x80               ; Input address where the number 'n' is stored
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text

_start:
    movea.l  input_addr, A0                  ; A0 <- input_addr
    move.l   (A0), D0                        ; D0 <- n (input value)

    not.l    D0                              ; D0 <- ~D0 (logical NOT)

    movea.l  output_addr, A1                 ; A1 <- output_addr
    move.l   D0, (A1)                        ; *output_addr <- D0 (result)

    halt
