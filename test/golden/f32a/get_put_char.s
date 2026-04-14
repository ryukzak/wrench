    .data

input_addr:      .word  0x80
output_addr:     .word  0x84

    .text

_start:
    @p input_addr a!
    @p output_addr b!

    @

    dup 'X' is_equal
    if minus1

    dup 'Y' is_equal
    if cccccccc

    !b halt

is_equal:
    inv 1 + + ;

minus1:
    -1 !b halt

cccccccc:
    0xCCCCCCCC !b halt
