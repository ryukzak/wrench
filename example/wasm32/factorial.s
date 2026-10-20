    .text

_start:
    .frame () $n, $acc
    locals.reserve 2
    i32.const 0x80 i32.load
    local.set $n
    i32.const 1
    local.set $acc

    ; block and loop each push a record on the control stack, and their end
    ; pops it. br n names a scope by depth, 0 being the innermost: br 0
    ; jumps back to the start of the loop, br 1 leaves the block and goes on
    ; after its end. br_if branches only if the value it pops is non-zero.
    block
        loop
            local.get $n i32.const 1 i32.le_s
            br_if 1                          ; n <= 1 -> leave the block

            local.get $acc local.get $n i32.mul
            local.set $acc                   ; acc *= n
            local.get $n i32.const 1 i32.sub
            local.set $n                     ; n -= 1
            br 0                             ; back to the loop head
        end
    end

    i32.const 0x84
    local.get $acc
    i32.store
    halt
