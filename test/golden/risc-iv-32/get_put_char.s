    .data

input_addr:        .word 0x80
output_addr:       .word 0x84    ; Output address where the result should be stored

    .text

_start:

    lui      t0, input_addr      ; int * input_addr_const = *input_addr;
    addi t0, t0, input_addr

    lw      t0, 0(t0)            ; int output_addr = *output_addr_const;

    lui      t1, output_addr     ; int * output_addr_const = *output_addr;
    addi t1, t1, output_addr

    lw      t1, 0(t1)            ; int output_addr = *output_addr_const;

    lw      t2, 0(t0)            ;   tmp = *input_addr
    sb      t2, 0(t1)            ;   *output_addr = tmp;

    halt
