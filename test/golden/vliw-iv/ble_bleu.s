    .text

_start:
    nop          | addi a0, zero, -1              | nop             | nop
    nop          | addi a1, zero, -1              | nop             | nop
    nop          | nop                            | nop             | j continue

make_pos_a0:
    ; Should be reached one time because of the signed comparison.
    nop          | sub a0, zero, a0               | nop             | nop
    nop          | nop                            | nop             | j continue

make_pos_a1:
    ; Never reached because of the unsigned comparison.
    nop          | sub a1, zero, a1               | nop             | nop
    nop          | nop                            | nop             | j continue

continue:
    nop          | nop                            | nop             | ble a0, zero, make_pos_a0
    nop          | nop                            | nop             | bleu a1, zero, make_pos_a1
    nop          | nop                            | nop             | halt
