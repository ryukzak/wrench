    .text

_start:
    i32.const 6
    i32.const 3
    i32.and                                  ; 0b0110 & 0b0011 -> 2

    i32.const 6
    i32.const 3
    i32.or                                   ; 0b0110 | 0b0011 -> 7

    i32.const 6
    i32.const 3
    i32.xor                                  ; 0b0110 ^ 0b0011 -> 5

    i32.const 6
    i32.const 2
    i32.shl                                  ; 6 << 2 -> 24

    i32.const -8
    i32.const 1
    i32.shr_s                                ; -8 >>s 1 -> -4 (sign-extending)

    i32.const -8
    i32.const 1
    i32.shr_u                                ; -8 >>u 1 -> 2147483644 (zero-filling)
    halt
