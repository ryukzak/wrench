    .text

sum:
    local.get 0
    i32.eqz
    if
        i32.const 0
        return
    end
    local.get 0
    local.get 0
    i32.const 1
    i32.sub
    i32.const sum
    call 1, 1
    i32.add
    return

_start:
    i32.const 0x300                          ; default root here is 0x380
    sp.init
    i32.const 20
    i32.const sum
    call 1, 1
    halt
