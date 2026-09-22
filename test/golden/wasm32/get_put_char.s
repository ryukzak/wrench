    .text

    ; Read one character from the input port, reject two specific values as
    ; deliberate error cases (mirroring "Wrench.Isa.Wasm32"'s own example),
    ; write the (possibly rejected) result to the output port.
_start:
    i32.const 0x84
    i32.const 0x80
    i32.load
    i32.const normalize_char
    call     1, 1
    i32.store
    halt

normalize_char:
    local.get 0
    i32.const 'X'
    i32.eq
    if
        i32.const -1
        return
    end

    local.get 0
    i32.const 'Y'
    i32.eq
    if
        i32.const -858993460
        return
    end

    local.get 0
    return
