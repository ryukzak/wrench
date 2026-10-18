    .text

_start:
    locals.reserve 1
    i32.const 1
    local.set 0
    loop
        local.get 0
        i32.const 2
        i32.mul
        local.set 0                          ; counter *= 2
        local.get 0
        i32.const 100
        i32.lt_s                             ; still under 100 -> double again
        br_if 0                              ; (0 = this loop)
    end
    local.get 0
    halt
