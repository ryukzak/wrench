    .text

    ; Read one value from the input port, logically negate it (i32.eqz
    ; doubles as "not" for 0/1-valued booleans -- eqz(0)=1, eqz(1)=0), write
    ; the result to the output port.
_start:
    i32.const 0x84
    i32.const 0x80
    i32.load
    i32.const logical_not
    call     1, 1
    i32.store
    halt

logical_not:
    local.get 0
    i32.eqz
    return
