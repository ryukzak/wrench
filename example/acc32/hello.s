    .data

    ; be aware, that it is not a pstr or cstr. It is just a buffer

buf:             .byte  'Hello\n\0World\0\0\0\0\0'
i:               .word  0
ptr:             .word  0
output_addr:     .word  0x84               ; Output address where the result should be stored
buf_size:        .word  12
const_1:         .word  1
const_FF:        .word  0xFF

    .text

_start:

    load_imm     buf
    store_addr   ptr                         ; ptr <- buf

    load_addr    buf_size
    store        i                           ; buf_size <- i

while:
    beqz         end                         ; while (i != 0) {

    load_ind     ptr
    and          const_FF
    store_ind    output_addr                 ;     *output_addr = *ptr & 0xFF

    load_addr    ptr
    add          const_1
    store_addr   ptr                         ;     ptr++

    load         i
    sub          const_1
    store        i                           ;     i--

    jmp          while                       ; }

end:
    halt
