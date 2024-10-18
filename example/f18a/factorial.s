    .data

input_addr:        .word 0x80
output_addr:       .word 0x84
alligment:         .word '................................'

    .text

_start:
    lit 1                          \ acc:[]

    @p input_addr  a! @            \ n:acc:[]

while:
    dup                            \ n:n:acc:[]
    if finish                      \ n:acc:[]

    dup a!                         \ n:acc:[]

    over                           \ acc:n:[]
    lit 0                          \ 0:acc:n:[]

    lit 31 r>                      \ for R = 31
multiply_begin:
    +*                             \ mres-high:acc-old:n:[]
                                   \ mres-low in a
    next multiply_begin

    drop drop a                    \ mres-low:n:[] => acc:n:[]

    over                           \ n:acc
    lit -1 +                       \ n-1:acc

    while ;

finish:                            \ n:acc:[]
    drop
    @p output_addr a! !
    halt
