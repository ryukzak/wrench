    .data

input_addr:      .word  0x80               ; Input address where the character is stored
output_addr:     .word  0x84               ; Output address where the character should be stored
cccccccc:        .word  0xcccccccc         ; Value to output if input is 'Y'
ffffffff:        .word  0xffffffff         ; Value to output if input is 'X'

    .text

_start:
    ; Load input value
    movea.l  input_addr, A0                  ; A0 <- address of input_addr
    movea.l  (A0), A0                        ; A0 <- value at input_addr (address of input stream)

    movea.l  output_addr, A7                 ; A7 <- address of output_addr
    movea.l  (A7), A7                        ; A7 <- value at output_addr (address of output stream)

    move.l   (A0), D0                        ; D0 <- value from input stream

    ; Check if input is 'X'
    move.l   D0, D1                          ; D1 <- D0 (input)
    sub.l    'X' , D1                        ; D1 <- D1 - 'X'
    beq      x_case                          ; If D1 == 0 (input was 'X'), goto x_case

    ; Check if input is 'Y'
    move.l   D0, D1                          ; D1 <- tmp (original input)
    sub.l    'Y' , D1                        ; D1 <- D1 - 'Y'
    beq      y_case                          ; If D1 == 0 (input was 'Y'), goto y_case

    move.l   D0, (A7)                        ; output_stream <- D0
    halt

x_case:
    move.l   0xffffffff, D0                  ; D0 <- 0xffffffff
    move.l   D0, (A7)                        ; output_stream <- D0
    halt

y_case:
    move.l   0xcccccccc, D0                  ; D0 <- 0xcccccccc
    move.l   D0, (A7)                        ; output_stream <- D0
    halt
