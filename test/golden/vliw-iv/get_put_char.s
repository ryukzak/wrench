    .data

input_addr:      .word  0x80
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text
    .org 0x100
_start:

    nop          | lui t0, %hi(input_addr)      | nop             | nop
    nop          | addi t0, t0, %lo(input_addr) | nop             | nop

    lw t0, 0(t0) | nop                          | nop             | nop

    nop          | lui t1, %hi(output_addr)     | nop             | nop
    nop          | addi t1, t1, %lo(output_addr) | nop            | nop

    lw t1, 0(t1) | nop                          | nop             | nop

    lw t2, 0(t0) | nop                          | nop             | nop

    nop          | addi t3, zero, 'X'           | nop             | nop

    nop          | nop                          | nop             | beq t2, t3, min_one

    nop          | addi t3, zero, 'Y'           | nop             | nop
    nop          | nop                          | nop             | beq t2, t3, cccccccc

    sb t2, 0(t1) | nop                          | nop             | nop

    nop          | nop                          | nop             | halt

min_one:
    nop          | addi t2, zero, -1            | nop             | nop
    sw t2, 0(t1) | nop                          | nop             | nop
    nop          | nop                          | nop             | halt

cccccccc:
    nop          | lui t2, %hi(0xCCCCCCCC)      | nop             | nop
    nop          | addi t2, t2, %lo(0xCCCCCCCC) | nop             | nop
    sw t2, 0(t1) | nop                          | nop             | nop
    nop          | nop                          | nop             | halt
