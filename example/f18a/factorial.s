    .data

input_addr:        .word 0x800
output_addr:       .word 0x804

    .text

_start:
    lit 1                          \ acc:[]


    @p input_addr  a! @            \ n:acc:[]



while:
    dup                            \ n:n:acc:[]
    if finish
                                   \ n:acc:[]

    dup a!

    over          \ acc:n:[]
    lit 0         \ 0:acc:n:[]

    +* +* +* +*   +* +* +* +*
    +* +* +* +*   +* +* +* +*
    +* +* +* +*   +* +* +* +*
    +* +* +* +*   +* +* +* +*          \ mres-high:acc-old:n:[]
                                       \ mres-low in a

    drop drop a                        \ mres-low:n:[] => acc:n:[]

    over \ n:acc
    lit -1 +

    while ;



finish:                           \ n:acc:[]
    drop
    @p output_addr a! !
    halt
