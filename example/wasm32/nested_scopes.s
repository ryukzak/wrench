    .text

    ; Synthetic demo: _start -> outer -> inner, with a block/loop inside
    ; inner, so one trace step shows all four control-record kinds open
    ; at once (1 LoopScope, 1 BlockScope, 2 suspended-caller CallScopes).
    ; outer also demonstrates locals: 2 params plus 1 scratch local
    ; (just a third caller-pushed slot, overwritten before ever being
    ; read as an argument), and 2 results.
inner:
    i32.const 3
    block
        loop
            i32.const 3
            i32.add
        end
    end
    return

outer:
    local.get 0
    local.get 1
    i32.add
    local.set 2
    i32.const inner
    call     0, 1
    local.get 2
    return

_start:
    i32.const 100
    i32.const 10
    i32.const 20
    i32.const 0
    i32.const outer
    call     3, 2
    halt
