    .data

input_addr:      .word  0x80
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text

_start:

    load_ind     input_addr                  ; acc <- mem[mem[input_addr]]
    store_ind    output_addr                 ; mem[mem[output_addr]] <- acc

    halt
