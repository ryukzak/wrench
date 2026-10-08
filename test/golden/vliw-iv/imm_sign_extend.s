    .text

_start:
    addi t0, zero, -1              /                                / nop / nop
    addi t1, zero, %lo(-1)         /                                / nop / nop

    lui t2, %hi(0x12345fff)        /                                / nop / nop
    addi t2, t2, %lo(0x12345fff)   /                                / nop / nop

    lui t3, %hi(0xffffffff)        /                                / nop / nop
    addi t3, t3, %lo(0xffffffff)   /                                / nop / nop

    nop                            / nop                            / nop / halt
