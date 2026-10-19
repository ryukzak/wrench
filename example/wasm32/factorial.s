    .text

_start:
    locals.reserve 2                         ; 0: n, 1: acc
    i32.const 0x80 i32.load
    local.set 0
    i32.const 1
    local.set 1

    ; block and loop each push a record on the control stack, and their end
    ; pops it. br n names a scope by depth, 0 being the innermost: br 0
    ; jumps back to the start of the loop, br 1 leaves the block and goes on
    ; after its end. br_if branches only if the value it pops is non-zero.
    block
        loop
            local.get 0 i32.const 1 i32.le_s
            br_if 1                          ; n <= 1 -> leave the block

            local.get 1 local.get 0 i32.mul
            local.set 1                      ; acc *= n
            local.get 0 i32.const 1 i32.sub
            local.set 0                      ; n -= 1
            br 0                             ; back to the loop head
        end
    end

    i32.const 0x84
    local.get 1
    i32.store
    halt
