    .text

_start:
    i32.const 0x84                           ; output address, under the value
    i32.const 0x80
    i32.load                                 ; tmp = *input_addr;
    i32.store8                               ; *output_addr = tmp;
    halt
