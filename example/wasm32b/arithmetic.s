    .text

_start:
    i32.const 2
    i32.const 3
    i32.add           ; -> 5
    i32.const 4
    i32.mul           ; -> 20
    i32.const 1
    i32.sub           ; -> 19
    halt
