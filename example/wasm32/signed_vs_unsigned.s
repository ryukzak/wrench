    .text

_start:
    ; Same two bit patterns, read two different ways: -1 (0xFFFFFFFF) is
    ; less than 1 as a signed comparison, but far *greater* than 1 once
    ; those same bits are read as unsigned.
    i32.const -1
    i32.const 1
    i32.lt_s                                 ; -1 <s 1 -> 1 (true)

    i32.const -1
    i32.const 1
    i32.lt_u                                 ; -1 <u 1 -> 0 (false)
    halt
