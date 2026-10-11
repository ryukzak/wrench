    .data

counter:         .word  0

    .text

_start:
    ; Sum 1..5 by counting down, pushing a marker (the counter's current
    ; value) each iteration before decrementing; five adds then collapse
    ; the trail (5,4,3,2,1,0) to 15. The counter lives in `.data`, same
    ; reason as loop.s.
    i32.const counter
    i32.const 5
    i32.store
    loop
        i32.const counter
        i32.load
        i32.const counter
        i32.const counter
        i32.load
        i32.const 1
        i32.sub
        i32.store
        i32.const counter
        i32.load
        i32.const 0
        i32.gt_s
        br_if    0
    end
    i32.const counter
    i32.load
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
    halt
