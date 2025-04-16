    .data

input_addr:      .word  0x80               ; Input address where the number 'n' is stored
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text

_start:
    move.l   input_addr, A0                ; A0 <- input_addr
    move.l   (A0), A0                      ; A0 <- *input_addr

    move.l   (A0), D0                      ; D0 <- n (input value)
    moveq    #1, D1                        ; D1 <- 1 (accumulator)

factorial_while:
    beq      factorial_end                 ; if (D0 == 0) goto factorial_end
    muls     D0, D1                        ; D1 <- D1 * D0
    subq.l   #1, D0                        ; D0 <- D0 - 1
    bra      factorial_while               ; loop back to factorial_while

factorial_end:
    move.l   output_addr, A1               ; A1 <- output_addr
    move.l   (A1), A1                      ; A1 <- *output_addr
    move.l   D1, (A1)                      ; *output_addr <- D1 (result)

    halt
