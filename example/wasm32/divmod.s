    .text

    ; divmod(a, b) -> (quotient, remainder). A function's resultCount
    ; isn't limited to 1: the callee just pushes both results, in
    ; order, before `return` -- quotient first, remainder on top (see
    ; the Isa module haddock on `Call`).
divmod:
    local.get 0
    local.get 1
    i32.div_s
    local.get 0
    local.get 1
    i32.rem_s
    return

    ; `_start` is never called, so it has no parameters -- and a
    ; function's only locals are its parameters -- to hold divmod's two
    ; results in while sorting out which goes where. `i32.store` can't
    ; help either: it needs [address, value] adjacent on the stack with
    ; the value on top, and the two results already arrived on top of
    ; each other in a fixed order, with no way to slot a freshly-pushed
    ; address underneath one without disturbing the other. store2 sees
    ; them as ordinary parameters instead -- always real locals, no
    ; instruction needed to declare them -- so it can `local.get` them
    ; in whatever order actually pairs each value with its own
    ; destination.
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
