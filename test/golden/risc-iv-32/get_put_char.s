    .data

input_addr:      .word  0x80
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text

_start:

    lui      t0, %hi(input_addr)             ; int * input_addr_const = *input_addr;
    addi     t0, t0, %lo(input_addr)

    lw       t0, 0(t0)                       ; int output_addr = *output_addr_const;

    lui      t1, %hi(output_addr)            ; int * output_addr_const = *output_addr;
    addi     t1, t1, %lo(output_addr)

    lw       t1, 0(t1)                       ; int output_addr = *output_addr_const;

    lw       t2, 0(t0)                       ;   tmp = *input_addr

    addi     t3, zero, 'X'

    beq      t2, t3, min_one

    addi     t3, zero, 'Y'
    beq      t2, t3, cccccccc

    sb       t2, 0(t1)                       ;   *output_addr = tmp;

    halt

min_one:
    addi     t2, zero, -1
    sw       t2, 0(t1)
    halt

cccccccc:
    lui      t2, %hi(0xCCCCCCCC)
    addi     t2, t2, %lo(0xCCCCCCCC)
    sw       t2, 0(t1)
    halt
