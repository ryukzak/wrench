    .text

    ; sum(n) = n + sum(n-1), recursively -- 20 deep, so 20 call records.
    ; The default root leaves room for 16 of them in this memory, so
    ; without the `sp.init` below the program stops with
    ;
    ;   control stack overflow: scope record ending at ... would reach
    ;   past the end of memory at 0x400
    ;
    ; `sp.init` moves the root both stacks grow from: lower it and the
    ; control stack gains the room the operand stack gives up. That is
    ; the whole trade -- deep recursion wants the root low, a loop that
    ; keeps a lot on the operand stack wants it high.
sum:
    local.get 0
    i32.const 0
    i32.eq                                   ; n == 0? (no i32.eqz in this ISA)
    if
        i32.const 0
        return
    end
    local.get 0
    local.get 0
    i32.const 1
    i32.sub
    i32.const sum
    call     1, 1
    i32.add
    return

_start:
    i32.const 0x300                          ; default root here is 0x380
    sp.init
    i32.const 20
    i32.const sum
    call     1, 1
    halt
