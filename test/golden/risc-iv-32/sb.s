    .data
buf:             .byte  '1234'
test_char:       .byte  0xFF

    .text
    .org     0x88
_start:
    ; t0 -- buf pointer
    ; t1 -- test char
    lui      t0, %hi(buf)                    ; load buffer pointer
    addi     t0, t0, %lo(buf)

    lui      t1, %hi(test_char)              ; load test char
    addi     t1, t1, %lo(test_char)
    lw       t1, 0(t1)

    sb       t1, 0(t0)                       ; write test char to buffer

    halt
