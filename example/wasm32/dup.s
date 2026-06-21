    .text

_start:
    .func
        i32.const 0x80
        i32.load
        call     write_twice
        halt
    .endfunc

write_twice:
    .func    params $value
        i32.const 0x84
        local.get $value
        i32.store

        i32.const 0x84
        local.get $value
        i32.store
    .endfunc
