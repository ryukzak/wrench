    .data

input_addr:      .word  0x80
output_addr:     .word  0x84

    .text

_start:

    load         input_addr
    load_acc
    store_ind    output_addr
    store_ind    output_addr
    halt
