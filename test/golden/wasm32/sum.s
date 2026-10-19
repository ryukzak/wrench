    .data

counter:         .word  0

    .text

_start:
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
        br_if 0
    end
    i32.const counter
    i32.load
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
    halt
