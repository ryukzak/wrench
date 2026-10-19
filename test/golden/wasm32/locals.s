    .text

fib:
    locals.reserve 1                         ; local 1: scratch, this frame only
    local.get 0
    i32.const 2
    i32.lt_s
    if
        local.get 0                          ; fib(0) = 0, fib(1) = 1
        return
    end
    local.get 0
    i32.const 1
    i32.sub
    i32.const fib
    call 1, 1
    local.set 1                              ; stash fib(n-1)
    local.get 0
    i32.const 2
    i32.sub
    i32.const fib
    call 1, 1                                ; fib(n-2) -- clobbers nothing of ours
    local.get 1
    i32.add
    return

_start:
    i32.const 10
    i32.const fib
    call 1, 1
    halt
