    .data

input_addr:        .word 0x80
output_addr:       .word 0x84

    .text

_start:
    lit 1                          \ acc:[]


    @p input_addr  a! @            \ n:acc:[]



while:
    dup                            \ n:n:acc:[]
    if finish
                                   \ n:acc:[]


    dup a!
    over a \ n:acc:n:[]
    +* \ acc:n

    over \ n:acc
    lit -1 +

    while ;



finish:                           \ n:acc:[]
    drop
    @p output_addr a! !
    halt
