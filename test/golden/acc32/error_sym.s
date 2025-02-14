    .data

cc:              .word  0xCCCCCCCC
ff:              .word  0xFFFFFFFF
o:               .word  'o'
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text

_start:

    load_addr    cc
    store_ind    output_addr

    load_addr    ff
    store_ind    output_addr

    load_addr    o
    store_ind    output_addr

    halt
