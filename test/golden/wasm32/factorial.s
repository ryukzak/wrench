    .text

    ; factorial(n), recursive. No separate direct-call instruction: a
    ; statically-known call site is just `i32.const target` before `call`.
factorial:
    local.get 0 i32.const 1 i32.le_s
    if
        i32.const 1 return
    end
    ; n stays on the stack under the recursive call's own frame, untouched
    ; however deep it goes, ready for the multiply after it returns.
    local.get 0
    local.get 0 i32.const 1 i32.sub
    i32.const factorial call 1, 1
    i32.mul
    return

_start:
    i32.const 0x84
    i32.const 0x80 i32.load
    i32.const factorial call 1, 1
    i32.store
    halt
