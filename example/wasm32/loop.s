    .data

counter:         .word  0

    .text

_start:
    ; Double 1 until it's no longer less than 100 -- the smallest power of
    ; two >= 100.
    ;
    ; `_start` is never called, so it has no parameters -- and a
    ; function's only locals are its parameters -- to hold the counter
    ; in. It lives in `.data` instead: `loop` pushes its own control
    ; record the moment it's entered, and that record physically sits
    ; between anything pushed before `loop` and anything pushed inside
    ; its body -- an instruction inside the loop (like `i32.mul` here,
    ; needing two operands) can't reach *through* that record to a value
    ; that was on the stack before the loop started. A `.data` word's
    ; address is fixed and unaffected by any of that, so it's the right
    ; tool whenever a value needs to survive a loop/block boundary via
    ; anything other than `dup`-ing something already inside the body.
    i32.const counter
    i32.const 1
    i32.store
    loop
        i32.const counter
        i32.const counter
        i32.load
        i32.const 2
        i32.mul
        i32.store                                ; counter *= 2
        i32.const counter
        i32.load
        i32.const 100
        i32.lt_s                                 ; condition = (counter <s 100)
        br_if    0                               ; still under 100 -> double again (0 = this loop)
    end
    i32.const counter
    i32.load
    halt
