    .text

foo:
    .frame ($a, $b) $le
    locals.reserve 1
    local.get $a local.get $b i32.le_s
    local.tee $le
    block
        if
            local.get $le
            i32.const bar call 1, 1
        end
    end
    return

bar:
    .frame ($x)
    local.get $x
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
