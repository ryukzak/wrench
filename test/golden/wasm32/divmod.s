    .text

divmod:
    local.get 0
    local.get 1
    i32.div_s
    local.get 0
    local.get 1
    i32.rem_s
    return

store2:
    local.get 3
    local.get 1
    i32.store
    local.get 2
    local.get 0
    i32.store
    return

_start:
    i32.const 0x80
    i32.load
    i32.const 0x84
    i32.load
    i32.const divmod
    call 2, 2
    i32.const 0x88
    i32.const 0x8c
    i32.const store2
    call 4, 0
    halt
