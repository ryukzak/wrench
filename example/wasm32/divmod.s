    .text

    ; divmod(a, b) -> (quotient, remainder): resultCount isn't limited to
    ; 1, so the callee just pushes both, in order, before `return`.
divmod:
    local.get 0
    local.get 1
    i32.div_s
    local.get 0
    local.get 1
    i32.rem_s
    return

    ; store2 takes divmod's two results as plain params instead: since
    ; i32.store needs [address, value] adjacent with value on top, and
    ; the results arrive in a fixed order, local.get lets it pair each
    ; one with its own destination in whatever order that takes.
store2:
    local.get 3
    local.get 1
    i32.store
    local.get 2
    local.get 0
    i32.store
    return

_start:
    i32.const 0x80
    i32.load
    i32.const 0x84
    i32.load
    i32.const divmod
    call     2, 2
    i32.const 0x88
    i32.const 0x8c
    i32.const store2
    call     4, 0
    halt
