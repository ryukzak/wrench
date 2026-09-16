    .text

_start:
    ; Count down from 5, leaving a marker (the old counter value) for each
    ; value actually processed. `continue` at 3 (skip the marker, keep
    ; looping); `break` at 2 (stop for good, before 2 or 1 are processed).
    ;
    ; `block` is what makes `break` clean: it's a valid forward branch
    ; target (jump past its own `end`), unlike `loop` which only ever
    ; gives you a backward one. From inside the loop, depth 0 is the loop
    ; itself (continue), depth 1 is the enclosing block (break) -- no need
    ; to smuggle "should I stop" into the loop's own continue condition
    ; anymore.
    block
        i32.const 5
        loop
            ; --- break check: counter == 2 ---
            dup
            i32.const 2
            i32.eq
            br_if 1        ; break: branch out to the block
            ; --- continue check: counter == 3 ---
            dup
            i32.const 3
            i32.eq
            if
                ; continue: no marker, just decrement
                i32.const 1
                i32.sub
            else
                ; normal: dup leaves the old counter as a marker
                dup
                i32.const 1
                i32.sub
            end
            dup
            i32.const 0
            i32.gt_s
            br_if 0        ; keep looping while counter > 0
        end
    end
    halt
