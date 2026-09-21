    .data

buf:             .byte  31, 'Hello\n\0World!'
buf_end:         .byte  0, 0, 0
ptr:             .word  0

    .text

    ; Copy buf..buf_end byte by byte to the output port. block/loop here work
    ; exactly like break_continue.s's do: depth 0 (the loop) is "keep going",
    ; depth 1 (the enclosing block) is "stop" -- reached once ptr catches up
    ; with buf_end. `_start` is never called, so it has no parameters -- and
    ; a function's only locals are its parameters -- to hold the running
    ; pointer in; it lives in `.data` instead, a fixed address unaffected
    ; by the loop's own control record the same way a local would be.
_start:
    i32.const ptr
    i32.const buf
    i32.store

    block
        loop
            i32.const ptr
            i32.load
            i32.const buf_end
            i32.ge_u
            br_if    1

            i32.const 0x84
            i32.const ptr
            i32.load
            i32.load8_u
            i32.store8

            i32.const ptr
            i32.const ptr
            i32.load
            i32.const 1
            i32.add
            i32.store

            br       0
        end
    end
    halt
