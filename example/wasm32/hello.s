    .data

    ; be aware, that it is not a pstr or cstr. It is just a buffer
buf:             .byte  'Hello\n\0World'

    .text

_start:
    locals.reserve 2                         ; 0: ptr, 1: n
    i32.const buf
    local.set 0                              ; ptr = buf;
    i32.const 12
    local.set 1                              ; int n = 12;

    block
        loop
            local.get 1
            i32.eqz
            br_if 1                          ; while (n != 0) {

            i32.const 0x84
            local.get 0
            i32.load8_u
            i32.store8                       ;   *output_addr = *ptr;

            local.get 0
            i32.const 1
            i32.add
            local.set 0                      ;   ptr++;

            local.get 1
            i32.const 1
            i32.sub
            local.set 1                      ;   n--;

            br 0                             ; }
        end
    end

    halt
