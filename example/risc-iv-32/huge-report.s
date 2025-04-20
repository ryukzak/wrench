    .text

_start:
    lui      t1, %hi(1000000)
    addi     t1, t1, %lo(1000000)

loop:
    addi     t1, t1, -1
    bne      t1, zero, loop

    halt
