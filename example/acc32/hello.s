    .data


buf:             .byte  'Hello\n\0World\0\0\0\0\0' ; Note: it is not a pstr or cstr.
i:               .word  0
ptr:             .word  0
output_addr:     .word  0x84
buf_size:        .word  12
const_1:         .word  1
const_FF:        .word  0xFF

    .text

_start:

    load_imm     buf
    store        ptr                         ; ptr <- buf

    load         buf_size
    store        i                           ; i <- buf_size

while:
    beqz         end                         ; while (i != 0) {

    load_ind     ptr
    and          const_FF
    store_ind    output_addr                 ;     *output_addr <- *ptr & const_FF

    load         ptr
    add          const_1
    store        ptr                         ;     ptr <- ptr + const_1

    load         i
    sub          const_1
    store        i                           ;     i <- i - const_1

    jmp          while                       ; }

end:
    halt
