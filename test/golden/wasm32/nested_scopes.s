    .text

inner:
    i32.const 3
    block
        loop
            i32.const 3
            i32.add
        end
    end
    return

outer:
    .frame ($a, $b, $sum)
    local.get $a
    local.get $b
    i32.add
    local.set $sum
    i32.const inner
    call 0, 1
    local.get $sum
    return

_start:
    i32.const 100
    i32.const 10
    i32.const 20
    i32.const 0
    i32.const outer
    call 3, 2
    halt
