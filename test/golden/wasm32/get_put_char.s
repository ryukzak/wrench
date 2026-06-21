    .text

_start:
    .func
    i32.const 0x84
    i32.const 0x80
    i32.load
    call     normalize_char
    i32.store
    halt
    .endfunc

normalize_char:
    .func    params $ch result i32

    local.get $ch
    i32.const 'X'
    i32.eq
    if       $domain_error
        i32.const -1
        return
    end

    local.get $ch
    i32.const 'Y'
    i32.eq
    if       $overflow_error
        i32.const -858993460
        return
    end

    local.get $ch
    return
    .endfunc
