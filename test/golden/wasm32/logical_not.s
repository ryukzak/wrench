    .text

_start:
    .func
    i32.const 0x84
    i32.const 0x80
    i32.load
    call     logical_not
    i32.store
    halt
    .endfunc

logical_not:
    .func    params $value result i32
    local.get $value
    i32.eqz
    return
    .endfunc
