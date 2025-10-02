    .data

input_addr:      .word  0x80
output_addr:     .word  0x84               ; Output address where the result should be stored
tmp:             .word  0x0
x:               .word  'X'
y:               .word  'Y'
cccccccc:        .word  0xcccccccc
ffffffff:        .word  0xffffffff


    .text

_start:

    load         input_addr
    load_acc


    ; if 'x' is the input, then acc = 0xffffffff
    ; if 'y' is the input, then acc = 0xcccccccc

    store_addr   tmp

    sub          x
    beqz         x_case

    load_addr    tmp
    sub          y
    beqz         y_case

    load_addr    tmp
    store_ind    output_addr
    halt

x_case:
    load_addr    ffffffff
    store_ind    output_addr
    halt

y_case:
    load_addr    cccccccc
    store_ind    output_addr
    halt
