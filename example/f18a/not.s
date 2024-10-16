    .data

input_addr:        .word 0x80
output_addr:       .word 0x84
const_1:           .word 0x1

    .text

_start:

    ; /------------------- T <- input_addr
    ; |           /------- T -> A
    ; |           |  /---- [A] -> T
    ; v           v  v
    @p input_addr a! @
    @p const_1 xor        ; T <- T xor S

    ;                /-- T -> [A]
    ;                v
    @p output_addr a! !

    halt
