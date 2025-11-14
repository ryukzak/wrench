    .data
buf:             .byte  'Hello\n\0World'
output_addr:     .word  0x84

    .text
    .org 0x100
_start:
    nop          | lui x1, %hi(output_addr)      | nop             | nop
    nop          | addi x1, x1, %lo(output_addr) | nop             | nop
    lw x1, 0(x1) | addi x2, x2, buf              | addi x3, x0, 12 | nop

while:
    lw x4, 0(x2) | addi x5, x3, -1               | nop             | beqz x3, end
    sb x4, 0(x1) | mv x3, x5                     | addi x2, x2, 1  | j while

end:
    nop          | nop                           | nop             | halt
