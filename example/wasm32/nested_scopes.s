    .text

    ; Synthetic demo, not a real computation: two nested calls
    ; (_start -> outer -> inner) plus a block/loop pair inside inner, so a
    ; single trace step shows all four control-record kinds open at
    ; once -- 1 LoopScope, 1 BlockScope (both inner's own), and 2
    ; CallScopes (one per suspended caller: outer waiting on inner,
    ; _start waiting on outer). Operand data sits at two of the three
    ; levels (_start's own marker, and inner's 3), leaving outer's own
    ; level empty at that point -- "different, but not all" -- so the
    ; layout view's per-level (operands) spans read differently at each
    ; depth instead of looking the same everywhere.
    ;
    ; outer also demonstrates locals proper: it declares 2 params and 1
    ; local (there's no separate declaration for a "local" in this ISA --
    ; it's exactly a third caller-pushed slot that outer treats as scratch
    ; instead of as an argument, overwriting it via local.set before ever
    ; reading the caller-supplied value) and 2 results.
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
