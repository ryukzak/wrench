    .text

_start:
    i32.const 10
    i32.const 20
    i32.const 1
    i32.const 2
    i32.lt_s                                 ; condition = (1 <s 2) = 1 (true)
    if
        i32.add                              ; taken: 10 + 20 -> 30
    else
        i32.mul                              ; not taken
    end
    halt
