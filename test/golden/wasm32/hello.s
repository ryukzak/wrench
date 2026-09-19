    .data

buf:             .byte  31, 'H' , 'e' , 'l' , 'l' , 'o' , '\n' , '\0' , 'W' , 'o' , 'r' , 'l' , 'd' , '!'
buf_end:         .byte  0, 0, 0

    .text

    ; Copy buf..buf_end byte by byte to the output port. block/loop here work
    ; exactly like break_continue.s's do: depth 0 (the loop) is "keep going",
    ; depth 1 (the enclosing block) is "stop" -- reached once ptr catches up
    ; with buf_end.
_start:
    locals   2
    i32.const buf
    local.set 0
    i32.const buf_end
    local.set 1

    block
        loop
            local.get 0
            local.get 1
            i32.ge_u
            br_if    1

            i32.const 0x84
            local.get 0
            i32.load8_u
            i32.store8

            local.get 0
            i32.const 1
            i32.add
            local.set 0

            br       0
        end
    end
    halt
