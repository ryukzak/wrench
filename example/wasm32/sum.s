    .data

counter:         .word  0

    .text

_start:
    ; Sum 1..5, by counting down and leaving a trail: each iteration
    ; pushes a marker (the counter's current value) before decrementing,
    ; so the marker stays on the stack forever while the counter itself
    ; keeps going. After the loop, one more marker (the final 0) is
    ; pushed, and five plain adds collapse the trail (5,4,3,2,1,0) to 15.
    ;
    ; The counter lives in `.data`, not a local, for the same reason
    ; loop.s's does: `_start` is never called, so it has no parameters --
    ; and a function's only locals are its parameters -- plus `loop`
    ; pushes its own control record between anything pushed before it
    ; and anything pushed inside its body, so an instruction inside the
    ; loop can't reach through that record to a value left there
    ; beforehand. Markers are fine on the raw stack, though -- each one
    ; is pushed *after* the record already exists, so they only ever
    ; pile up on top of each other, never needing to reach below the
    ; record.
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
