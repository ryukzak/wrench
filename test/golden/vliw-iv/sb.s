    .data
buf:             .byte  '1234'
test_char:       .byte  0xFF

    .text
    .org 0x100
_start:
    ; t0 -- buf pointer
    ; t1 -- test char
    nop          | lui t0, %hi(buf)               | nop             | nop
    nop          | addi t0, t0, %lo(buf)          | nop             | nop

    nop          | lui t1, %hi(test_char)         | nop             | nop
    nop          | addi t1, t1, %lo(test_char)    | nop             | nop
    lw t1, 0(t1) | nop                            | nop             | nop

    sb t1, 0(t0) | nop                            | nop             | nop

    nop          | nop                            | nop             | halt
