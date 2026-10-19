    .data

counter:         .word  0

    .text

_start:
    i32.const counter
    i32.const 5
    i32.store
    block
        loop
            i32.const counter
            i32.load
            i32.const 2
            i32.eq
            br_if 1                          ; break
            i32.const counter
            i32.load
            i32.const 3
            i32.eq
            if
                i32.const counter
                i32.const counter
                i32.load
                i32.const 1
                i32.sub
                i32.store
            else
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
            br_if 0                          ; continue
        end
    end
    halt
