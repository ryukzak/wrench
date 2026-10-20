    .text

_start:
    .frame () $counter
    locals.reserve 1
    i32.const 1
    local.set $counter
    loop
        local.get $counter
        i32.const 2
        i32.mul
        local.set $counter                   ; counter *= 2
        local.get $counter
        i32.const 100
        i32.lt_s                             ; still under 100 -> double again
        br_if 0                              ; (0 = this loop)
    end
    local.get $counter
    halt
