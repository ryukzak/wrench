    .data

input_addr:      .word  0x80               ; Input address where the number 'n' is stored
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text

_start:
    movea.l  input_addr, A0                  ; A0 <- input_addr
    movea.l  (A0), A0
    move.l   (A0), D0                        ; D0 <- n (input value)

    not.l    D0                              ; D0 <- ~D0 (logical NOT)
    move.l   0x01, D1
    and.l    D1, D0

    movea.l  output_addr, A1                 ; A1 <- output_addr
    movea.l  (A1), A1
    move.l   D0, (A1)                        ; *output_addr <- D0 (result)

    halt
