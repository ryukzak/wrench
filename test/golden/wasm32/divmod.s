    .text

divmod:
    .frame ($a, $b)
    local.get $a
    local.get $b
    i32.div_s
    local.get $a
    local.get $b
    i32.rem_s
    return

store2:
    .frame ($q, $r, $q_addr, $r_addr)
    local.get $r_addr
    local.get $r
    i32.store
    local.get $q_addr
    local.get $q
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
