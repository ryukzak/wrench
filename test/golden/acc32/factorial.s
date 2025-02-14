    .data

input_addr:      .word  0x80               ; Input address where the number 'n' is stored
output_addr:     .word  0x84               ; Output address where the result should be stored
n:               .word  0x00               ; Variable to store the number 'n'
result:          .word  0x01               ; Variable to store the result of the factorial, initialized to 1
const_1:         .word  0x01               ; Constant 1

    .text

_start:
    load_ind     input_addr                  ; acc <- mem[*input_addr]
    store_addr   n                           ; mem[n] <- acc

factorial_begin:
    load_addr    const_1                     ; acc <- m[const_1]
    store_addr   result                      ; mem[result] <- acc
    load_addr    n                           ; acc <- m[n]

factorial_while:
    beqz         factorial_end               ; while (acc != 0) {
    load_addr    result                      ;   acc <- m[result]
    mul          n                           ;   acc *= m[n]
    store_addr   result                      ;   m[result] <- acc

    load_addr    n                           ;   acc <- m[n]
    sub          const_1                     ;   acc = acc - 1
    store_addr   n                           ;   m[n] <- acc
    jmp          factorial_while             ; }

factorial_end:
    load_addr    result                      ; acc <- m[result]
    store_ind    output_addr                 ; m[output_addr] <- acc

    halt
