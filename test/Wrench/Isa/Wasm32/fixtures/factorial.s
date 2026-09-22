    .text

factorial:
    local.get 0
    i32.const 1
    i32.le_s
    if
        i32.const 1
        return
    end
    local.get 0
    local.get 0
    i32.const 1
    i32.sub
    i32.const factorial
    call     1, 1
    i32.mul
    return

_start:
    i32.const 5
    i32.const factorial
    call     1, 1
    halt
