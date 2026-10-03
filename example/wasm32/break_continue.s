    .data

counter:         .word  0

    .text

_start:
    ; Count 5 down to 0, marking each processed value; continue at 3
    ; (skip the marker), break at 2. The counter lives in `.data`, since
    ; `_start` has no params (a function's only locals). `block` gives
    ; `break` a forward target (depth 1); `loop` alone only jumps
    ; backward (depth 0, continue).
    i32.const counter
    i32.const 5
    i32.store
    block
        loop
            i32.const counter
            i32.load
            i32.const 2
            i32.eq
            br_if    1                               ; break
            i32.const counter
            i32.load
            i32.const 3
            i32.eq
            if
    ; continue: skip the marker
                i32.const counter
                i32.const counter
                i32.load
                i32.const 1
                i32.sub
                i32.store
            else
    ; mark the current counter, then decrement
                i32.const counter
                i32.load
                i32.const counter
                i32.const counter
                i32.load
                i32.const 1
                i32.sub
                i32.store
            end
            i32.const counter
            i32.load
            i32.const 0
            i32.gt_s
            br_if    0                               ; continue
        end
    end
    halt
