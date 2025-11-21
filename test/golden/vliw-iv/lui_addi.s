    .text

_start:
    nop          | lui t1, 0x12345                | nop             | nop
    nop          | addi t1, t1, 0x678             | nop             | nop

    nop          | lui t2, %hi(0x12345678)        | nop             | nop
    nop          | addi t2, t2, %lo(0x12345678)   | nop             | nop

    nop          | nop                            | nop             | halt
