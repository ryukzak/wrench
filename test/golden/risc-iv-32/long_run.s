    .data
acc:             .word  0                  ; Counter in memory, starts at 0

    .text
_start:
    lui      t0, %hi(acc)                    ; t0 = &acc
    addi     t0, t0, %lo(acc)
    addi     t1, zero, 2000                  ; 2000 iterations, 10004 instructions in all

loop:
    lw       t2, 0(t0)                       ; acc += 1, through memory
    addi     t2, t2, 1
    sw       t2, 0(t0)
    addi     t1, t1, -1
    bnez     t1, loop

    halt
