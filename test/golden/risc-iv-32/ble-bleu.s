.text

_start:
    addi a0, zero, -1           ; This works because addi can encode small immediates like -1.
    addi a1, zero, -1           ; This works because addi can encode small immediates like -1.
    j continue

make_pos_a0:                    ; Should be reached one time becasu of the signed comparison.
    sub a0, zero, a0
    j continue

make_pos_a1:                    ; Never reached because of the unsigned comparison.
    sub a1, zero, a1
    j continue

continue:
    ble  a0, zero, make_pos_a0
    bleu a1, zero, make_pos_a1
    halt
