    .text

foo:
    locals.reserve 1
    local.get 0 local.get 1 i32.le_s
    local.tee 2
    block
        if
            local.get 2
            i32.const bar call 1, 1
        end
    end
    return

bar:
    local.get 0
    i32.const 0xD
    i32.add
    i32.const 0xE
    halt
    return

_start:
    i32.const 0x80 sp.init
    i32.const 0x0A
    i32.const 0x0B
    i32.const 0x0C
    i32.const foo call 2, 1
    halt
