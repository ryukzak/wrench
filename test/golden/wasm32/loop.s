    .text

_start:
    ; Double 1 until it's >= 100. The counter lives in a local reserved
    ; up front: `_start` has no parameters, since nothing calls it, so
    ; before `locals.reserve` the only place to keep a value across
    ; iterations was a `.data` cell -- see sum.s and hello.s for that
    ; idiom, and locals.s for a local that survives recursion.
    locals.reserve 1
    i32.const 1
    local.set 0
    loop
        local.get 0
        i32.const 2
        i32.mul
        local.set 0                              ; counter *= 2
        local.get 0
        i32.const 100
        i32.lt_s                                 ; still under 100 -> double again
        br_if    0                               ; (0 = this loop)
    end
    local.get 0
    halt
