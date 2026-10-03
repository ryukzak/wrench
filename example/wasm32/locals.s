    .text

    ; fib(n) recursively, with local 0 the parameter and local 1 a slot
    ; reserved up front. Local 1 is the interesting part: it has to hold
    ; fib(n-1) while fib(n-2) runs, and that inner call recurses through
    ; this same function. A `.data` cell could not do it -- there is one
    ; cell, and the nested call would overwrite it before the add.
    ; `locals.reserve` gives each activation its own slot instead,
    ; zeroed, and `return` reclaims it along with the parameter.
fib:
    locals.reserve 1                         ; local 1: scratch, this frame only
    local.get 0
    i32.const 2
    i32.lt_s
    if
        local.get 0                              ; fib(0) = 0, fib(1) = 1
        return
    end
    local.get 0
    i32.const 1
    i32.sub
    i32.const fib
    call     1, 1
    local.set 1                              ; stash fib(n-1)
    local.get 0
    i32.const 2
    i32.sub
    i32.const fib
    call     1, 1                            ; fib(n-2) -- clobbers nothing of ours
    local.get 1
    i32.add
    return

_start:
    i32.const 10
    i32.const fib
    call     1, 1
    halt
