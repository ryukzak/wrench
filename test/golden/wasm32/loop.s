    .text

_start:
    ; Double 1 until it's no longer less than 100 -- the smallest power of
    ; two >= 100.
    ;
    ; The counter has to live in a local, not the raw stack: `loop`
    ; pushes its own control record the moment it's entered, and that
    ; record physically sits between anything pushed before `loop` and
    ; anything pushed inside its body -- an instruction inside the loop
    ; (like `i32.mul` here, needing two operands) can't reach *through*
    ; that record to a value that was on the stack before the loop
    ; started. A local's address is fixed and unaffected by any of that
    ; (see Wrench.Isa.Wasm32's `Locals` haddock), so it's the right tool
    ; whenever a value needs to survive a loop/block boundary via
    ; anything other than `dup`-ing something already inside the body.
    locals   1
    i32.const 1
    local.set 0
    loop
        local.get 0
        i32.const 2
        i32.mul
        local.tee 0                              ; value *= 2, keep a copy on the stack to compare
        i32.const 100
        i32.lt_s                                 ; condition = (value <s 100)
        br_if    0                               ; still under 100 -> double again (0 = this loop)
    end
    local.get 0
    halt
