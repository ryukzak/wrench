    .data
buf:             .byte  'Hello\n\0World'
output_addr:     .word  0x84

    .text
    .org 0x100
_start:
    lui t0, %hi(output_addr)       /                                /              /
    addi t0, t0, %lo(output_addr)  /                                /              /
    addi t1, t1, buf               / addi t3, zero, 12              / lw t0, 0(t0) /

while:
    addi t4, t3, -1                /                                / lw t2, 0(t1) / beqz t3, end
    mv t3, t4                      / addi t1, t1, 1                 / sb t2, 0(t0) / j while

end:
                                   /                                /              / halt
