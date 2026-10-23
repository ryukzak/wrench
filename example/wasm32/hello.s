    .data

    ; be aware, that it is not a pstr or cstr. It is just a buffer
buf:             .byte  'Hello\n\0World'

    .text

_start:
    .frame () $ptr, $n
    locals.reserve 2
    i32.const buf
    local.set $ptr                           ; ptr = buf;
    i32.const 12
    local.set $n                             ; int n = 12;

    block
        loop
            local.get $n
            i32.eqz
            br_if 1                          ; while (n != 0) {

            i32.const 0x84
            local.get $ptr
            i32.load8_u
            i32.store8                       ;   *output_addr = *ptr;

            local.get $ptr
            i32.const 1
            i32.add
            local.set $ptr                   ;   ptr++;

            local.get $n
            i32.const 1
            i32.sub
            local.set $n                     ;   n--;

            br 0                             ; }
        end
    end

    halt
