    .data

input_addr:      .word  0x80
output_addr:     .word  0x84
divisor:         .word  0x88

    .text

_start:
    @p input_addr a!                         \ A <- input_addr_label
    @ @                                      \ divisor:dividend:[]

    @p divisor b!                            \ B <- divisor_label
    !b                                       \ [B] <- divisor

    a!                                       \ A <- dividend
    lit 0 lit 0                              \ quotient:remainder:[]

    lit 31 r>                                \ for R = 31
multiply_begin:
    +/                                       \ mres-high:acc-old:n:[]
    \ mres-low in a
    next multiply_begin

    @p input_addr a!
    ! !

    halt
