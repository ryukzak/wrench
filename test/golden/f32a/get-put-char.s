    .data

input_addr:        .word 0x80
output_addr:       .word 0x84

    .text

_start:

    @p input_addr  a!
    @p output_addr b!

    @ !b

    halt
