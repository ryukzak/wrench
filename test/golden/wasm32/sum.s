    .text

_start:
    ; Sum 1..5, by counting down and leaving a trail: each iteration
    ; pushes a marker (the counter's current value) before decrementing,
    ; so the marker stays on the stack forever while the counter itself
    ; keeps going. After the loop, one more marker (the final 0) is
    ; pushed, and six plain adds collapse the trail (5,4,3,2,1,0) to 15.
    ;
    ; The counter lives in a local, not the raw stack, for the same
    ; reason loop.s's does: `loop` pushes its own control record between
    ; anything pushed before it and anything pushed inside its body, so
    ; an instruction inside the loop can't reach through that record to a
    ; value left there beforehand. Markers are fine on the raw stack,
    ; though -- each one is pushed *after* the record already exists, so
    ; they only ever pile up on top of each other, never needing to reach
    ; below the record.
    locals   1
    i32.const 5
    local.set 0
    loop
        local.get 0
        local.get 0
        i32.const 1
        i32.sub
        local.set 0
        local.get 0
        i32.const 0
        i32.gt_s
        br_if    0
    end
    local.get 0
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
    halt
