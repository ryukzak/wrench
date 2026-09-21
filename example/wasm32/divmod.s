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

_start:
    ; The two results land above the caller's own stack in push order
    ; (quotient below, remainder on top) -- stash them in locals before
    ; storing each one, same as any other value that has to survive a
    ; stack shuffle (see docs/wasm32.md, "Why a value has to live in a
    ; local").
    locals   2
    i32.const 0x80
    i32.load
    i32.const 0x84
    i32.load
    i32.const divmod
    call     2, 2
    local.set 1
    local.set 0
    i32.const 0x88
    local.get 0
    i32.store
    i32.const 0x8c
    local.get 1
    i32.store
    halt
