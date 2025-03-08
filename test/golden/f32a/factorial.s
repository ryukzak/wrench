    .data

input_addr:      .word  0x80
output_addr:     .word  0x84
tmp_divisor:     .word  0x0
tmp_prev:        .word  0x1
alligment:       .word  '................................'

    .text

    \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

_start:
    lit tmp_divisor
    b!
    @p input_addr a! @       \ n:[]

    dup
    -if continue

    handler_negative

continue:
    factorial
    @p output_addr a! !
    halt

    \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

handler_negative:
    lit -1
    @p output_addr a! !
    halt

    \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

handler_overflow:
    lit -858993460
    @p output_addr a! !
    halt

    \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

multiply:
    lit 31 >r                \ for R = 31
multiply_do:
    +*                       \ mres-high:acc-old:n:[]
    \ mres-low in a

    next multiply_do
    drop drop a              \ mres-low:n:[] => acc:n:[]
    ;

    \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

divide:
    lit 31 >r                \ for R = 31
divide_do:
    +/

    next divide_do
    ;

    \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

factorial:
    lit 1 over               \ n:acc:[]
factorial_while:
    dup                      \ n:n:acc:[]
    if factorial_finish      \ n:acc:[]

    dup a!                   \ n:acc:[]

    over                     \ acc:n:[]
    dup !p tmp_prev

continue_factorial:
    lit 0                    \ 0:acc:n:[]

    multiply                 \ acc:n:[]

    a! !b                    \ []
    a @b                     \ n:acc:[]
    lit 0 lit 0              \ 0:0:n:acc:[]
    divide                   \ quot:rem:n:acc:[]
    over                     \ rem:quot:n:acc:[]
    if continue_factorial2
    handler_overflow

continue_factorial2:
    @p tmp_prev              \ prev_acc:quot:n:acc[]

    inv lit 1 + +            \ quot-prev_acc:n:acc[]

    if continue_factorial3
    handler_overflow

continue_factorial3:
    lit -1 +                 \ n-1:acc
    factorial_while ;

factorial_finish:
    \ n:acc:[]
    drop
    ;
