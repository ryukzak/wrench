    .text

_start:
    i32.const -1
    i32.const 1
    i32.lt_s                                 ; -1 <s 1 -> 1 (true)

    i32.const -1
    i32.const 1
    i32.lt_u                                 ; -1 <u 1 -> 0 (false)
    halt
