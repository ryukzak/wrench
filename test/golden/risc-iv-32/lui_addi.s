    .text

_start:
    lui      t1, 0x12345                     ; Load upper 20 bits with 0x12345000
    addi     t1, t1, 0x678                   ; Add lower 12 bits (0x678)

    lui      t2, %hi(0x12345678)             ; Load upper 20 bits with 0x12345000
    addi     t2, t2, %lo(0x12345678)         ; Add lower 12 bits (0x678)

    halt
