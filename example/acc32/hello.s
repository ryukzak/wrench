    .data

const_1:         .word  1
const_FF:        .word  0xFF
output_addr:     .word  0x84
buf:             .byte  'Hello\n\0World\0\0\0\0\0' ; Note: it is not a pstr or cstr.
buf_size:        .word  12
i:               .word  0
ptr:             .word  0

    .text
    .org         0x90
_start:

    load_imm     buf
    store        ptr                         ; ptr <- buf

    load         buf_size
    store        i                           ; i <- buf_size

while:
    beqz         end                         ; while (i != 0) {

    load         ptr
    load_acc
    and          const_FF
    store_ind    output_addr                 ;     *output_addr <- *ptr & const_FF

    load_addr    ptr
    add          const_1
    store_addr   ptr                         ;     ptr <- ptr + const_1

    load_addr    i
    sub          const_1
    store_addr   i                           ;     i <- i - const_1

    jmp          while                       ; }

end:
    halt
