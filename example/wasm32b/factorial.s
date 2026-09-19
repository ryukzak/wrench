    .text

; factorial(n), recursive. No separate direct-call instruction: the
; target is always popped off the stack, so a statically-known call site
; is just `i32.const target` immediately before `call` -- no callee-side
; header either, so every call site states its own paramCount/resultCount
; directly (see the Isa module haddock on `Call`).
factorial:
    local.get 0
    i32.const 1
    i32.le_s
    if
        i32.const 1
        return
    end
    ; Stack effect while computing n * factorial(n - 1): the first
    ; local.get here leaves `n` sitting below wherever the recursive
    ; call's own frame gets carved out, untouched no matter how deep the
    ; recursion goes -- the same way an open block/loop's body values
    ; survive a nested call today.
    local.get 0
    local.get 0
    i32.const 1
    i32.sub
    i32.const factorial
    call 1, 1
    i32.mul
    return

_start:
    i32.const 5
    i32.const factorial
    call 1, 1
    halt
