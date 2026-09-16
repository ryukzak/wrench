    .text

_start:
    ; Sum 1..5, by counting down and leaving a trail: each iteration dups
    ; the counter *before* decrementing, so the old value stays on the
    ; stack forever (never touched again by the loop) while the fresh
    ; copy gets checked and decremented. After the loop the stack holds
    ; the whole trail (0,1,2,3,4,5); five plain adds collapse it to 15.
    ;
    ; This only works because 5 is known here, at write time -- there's
    ; no way yet to maintain two independent running values (a counter
    ; *and* a running sum, both updated every iteration) without knowing
    ; in advance how many `add`s to write afterward. That needs a way to
    ; reach the *second* stack slot (`swap`/`over`) or locals -- `dup`
    ; alone only ever reaches the top.
    i32.const 5
    loop
        dup
        i32.const 1
        i32.sub
        dup
        i32.const 0
        i32.gt_s
        br_if 0
    end
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
    halt
