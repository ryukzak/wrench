    .data

input_addr:      .word  0x80
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text

_start:

    load         input_addr
    load_acc
    store_ind    output_addr                 ; mem[mem[output_addr]] <- acc

    halt
