    .data

buf:             .byte  31, 'Hello\n\0World!'
buf_end:         .byte  0, 0, 0
ptr:             .word  0

    .text

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
            br_if 1

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

            br 0
        end
    end
    halt
