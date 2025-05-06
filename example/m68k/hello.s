    .data

buf:             .byte  'Hello\n\0World'   ; Buffer containing the string
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text

_start:
    move.l   output_addr, A0                 ; A0 <- output_addr
    move.l   (A0), A0                        ; A0 <- *output_addr

    lea      buf, A1                         ; A1 <- address of buf
    moveq    #12, D0                         ; D0 <- 12 (length of the string)

hello_while:
    beq      hello_end                       ; if (D0 == 0) goto hello_end
    move.b   (A1)+, D1                       ; D1 <- *A1; A1++
    move.b   D1, (A0)+                       ; *A0 <- D1; A0++
    subq.l   #1, D0                          ; D0 <- D0 - 1
    bra      hello_while                     ; loop back to hello_while

hello_end:
    halt
