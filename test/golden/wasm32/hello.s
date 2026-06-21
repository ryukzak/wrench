    .data

buf:             .byte  31, 'H' , 'e' , 'l' , 'l' , 'o' , '\n' , '\0' , 'W' , 'o' , 'r' , 'l' , 'd' , '!'
buf_end:         .byte  0, 0, 0

    .text

_start:
    .func    locals $ptr $end
    i32.const buf
    local.set $ptr
    i32.const buf_end
    local.set $end

    block    $done
        loop     $loop
            local.get $ptr
            local.get $end
            i32.ge_u
            br_if    $done

            i32.const 0x84
            local.get $ptr
            i32.load8_u
            i32.store8

            local.get $ptr
            i32.const 1
            i32.add
            local.set $ptr

            br       $loop
        end
    end

    halt
    .endfunc
