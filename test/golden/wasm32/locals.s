    .text

fib:
    .frame ($n) $prev                        ; locals 0, 1
    locals.reserve 1                         ; $prev: scratch, this frame only
    local.get $n
    i32.const 2
    i32.lt_s
    if
        local.get $n                         ; fib(0) = 0, fib(1) = 1
        return
    end
    local.get $n
    i32.const 1
    i32.sub
    i32.const fib
    call 1, 1
    local.set $prev                          ; stash fib(n-1)
    local.get $n
    i32.const 2
    i32.sub
    i32.const fib
    call 1, 1                                ; fib(n-2) -- clobbers nothing of ours
    local.get $prev
    i32.add
    return

_start:
    i32.const 10
    i32.const fib
    call 1, 1
    halt
