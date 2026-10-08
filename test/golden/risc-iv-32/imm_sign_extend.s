    .text

_start:
    addi     t0, zero, -1                    ; t0 = 0xffffffff
    addi     t1, zero, %lo(-1)               ; t1 = 0xffffffff, same as the literal above
    addi     t2, zero, 0x7ff                 ; t2 = 0x000007ff, largest positive field value
    addi     t3, zero, 0x800                 ; t3 = 0xfffff800, bit 11 is the sign bit

    lui      t4, %hi(0x12345fff)             ; %hi rounds up to 0x12346 ...
    addi     t4, t4, %lo(0x12345fff)         ; ... and %lo borrows it back: t4 = 0x12345fff

    lui      t5, %hi(0xffffffff)             ; %hi is 0 here ...
    addi     t5, t5, %lo(0xffffffff)         ; ... and %lo is -1: t5 = 0xffffffff

    andi     t6, t0, 0xfff                   ; t6 = 0xffffffff, andi sign-extends as well

    halt
