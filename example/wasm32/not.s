    .text

_start:
    i32.const 0x84                           ; output address, under the value
    i32.const 0x80
    i32.load                                 ; int n = *input_addr;
    i32.const 1
    i32.xor                                  ; n ^= 1;
    i32.store                                ; *output_addr = n;
    halt
