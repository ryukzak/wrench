    .data

input_addr:      .word  0x80               ; Input address where the number 'n' is stored
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text
    .org 0x100
_start:
    nop          | lui t0, %hi(input_addr)       | nop             | nop
    nop          | addi t0, t0, %lo(input_addr)  | nop             | nop

    lw t0, 0(t0) | nop                           | nop             | nop

    lw t1, 0(t0) | nop                           | nop             | nop

    nop          | addi t2, zero, 1              | nop             | nop

    nop          | xor t1, t1, t2                | nop             | nop

    nop          | lui t0, %hi(output_addr)      | nop             | nop
    nop          | addi t0, t0, %lo(output_addr) | nop             | nop

    lw t0, 0(t0) | nop                           | nop             | nop

    sw t1, 0(t0) | nop                           | nop             | nop

    nop          | nop                           | nop             | halt
