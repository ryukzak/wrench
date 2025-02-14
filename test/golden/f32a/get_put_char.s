    .data

input_addr:      .word  0x80
output_addr:     .word  0x84

    .text

_start:
    @p input_addr a!
    @p output_addr b!

    @

    dup lit 'X' is_equal
    if minus1

    dup lit 'Y' is_equal
    if cccccccc

    !b halt

is_equal:
    inv lit 1 + + ;

minus1:
    lit -1 !b halt

cccccccc:
    lit 0xCCCCCCCC !b halt
