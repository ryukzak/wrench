;; Логическое нет
    .data

input_addr:        .word 0x80    ; Input address where the number 'n' is stored
output_addr:       .word 0x84    ; Output address where the result should be stored
const_1:           .word 0x01

    .text

_start:
    load_ind   input_addr             ; acc <- m[input_addr]
    not                                ; acc <- ~acc
    and        const_1
    store_ind  output_addr             ; m[output_addr] <- acc
    halt
