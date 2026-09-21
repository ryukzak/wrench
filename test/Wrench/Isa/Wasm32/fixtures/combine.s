    .text

combine:
    locals   1
    local.get 0
    i32.const double
    call     1, 1
    local.set 1
    local.get 1
    local.get 0
    i32.add
    return

double:
    local.get 0
    i32.const 2
    i32.mul
    return

_start:
    i32.const 7
    i32.const combine
    call     1, 1
    halt
