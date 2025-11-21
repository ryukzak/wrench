    .data
buf:             .byte  31, 'Hello\n\0World!\0\0\0'
output_addr:     .word  0x84

    .text
    .org 0x100
_start:
    nop          | lui t0, %hi(output_addr)      | nop               | nop
    nop          | addi t0, t0, %lo(output_addr) | nop               | nop
    lw t0, 0(t0) | addi t1, t1, buf              | addi t3, zero, 14 | nop

while:
    lw t2, 0(t1) | addi t4, t3, -1               | nop               | beqz t3, end
    sb t2, 0(t0) | mv t3, t4                     | addi t1, t1, 1    | j while

end:
    nop          | nop                           | nop               | halt
