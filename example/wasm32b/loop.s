    .text

_start:
    ; Double 1 until it's no longer less than 100 -- the smallest power of
    ; two >= 100. `dup` is what makes this a *real* loop: the doubled
    ; value is inspected (compared against 100) without being consumed,
    ; so it survives to feed the next iteration.
    i32.const 1
    loop
        i32.const 2
        i32.mul       ; value *= 2
        dup           ; keep a copy to compare, without losing the value
        i32.const 100
        i32.lt_s      ; condition = (value <s 100)
        br_if         ; still under 100 -> double again
    end
    halt
