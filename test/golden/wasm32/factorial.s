    .text

factorial:
    .frame ($n)
    local.get $n i32.const 1 i32.le_s
    if
        i32.const 1 return
    end
    local.get $n
    local.get $n i32.const 1 i32.sub
    i32.const factorial call 1, 1
    i32.mul
    return

_start:
    i32.const 0x84
    i32.const 0x80 i32.load
    i32.const factorial call 1, 1
    i32.store
    halt
