    .text

apply_twice:
    local.get 1
    local.get 0
    call     1, 1
    local.get 0
    call     1, 1
    return

double:
    local.get 0
    i32.const 2
    i32.mul
    return

_start:
    i32.const double
    i32.const 3
    i32.const apply_twice
    call     2, 1
    halt
