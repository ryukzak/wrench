    .text

    ; Regression: i32.add used to reach past `block`/`loop` and corrupt
    ; the open LoopScope's own bytes instead of the "1" pushed earlier.
_start:
    i32.const 1
    block
        loop
            i32.const 2
            i32.add
        end
    end
    halt
