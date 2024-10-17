    .data

input_addr:        .word 0x80
output_addr:       .word 0x84

    .text

_start:
    lit 1

    @p input_addr
    dup
    if finish





finish:
    drop
    @p output_addr a! !
    halt
