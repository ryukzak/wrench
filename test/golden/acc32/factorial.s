    .data

input_addr:      .word  0x80               ; Input address where the number 'n' is stored
output_addr:     .word  0x84               ; Output address where the result should be stored
n:               .word  0x00               ; Variable to store the number 'n'
result:          .word  0x01               ; Variable to store the result of the factorial, initialized to 1
const_1:         .word  0x01               ; Constant 1

    .text

_start:
    load         input_addr
    load_acc
    store        n                           ; mem[n] = acc

    ble          not_in_domain

factorial_begin:
    load         const_1                     ; acc = mem[const_1]
    store        result                      ; mem[result] = acc
    load         n                           ; acc = mem[n]

factorial_while:
    beqz         factorial_end               ; if (acc == 0) goto factorial_end
    load         result                      ; acc = mem[result]
    mul          n                           ; acc = acc * mem[n]
    bvs          factorial_overflow          ; if overflow, goto factorial_overflow
    store        result                      ; mem[result] = acc

    load         n                           ; acc = mem[n]
    sub          const_1                     ; acc = acc - mem[const_1]
    store        n                           ; mem[n] = acc
    jmp          factorial_while             ; goto factorial_while

factorial_end:
    load         result                      ; acc = mem[result]
    store_ind    output_addr                 ; mem[mem[output_addr]] = acc
    halt

not_in_domain:

    load_imm     0xFFFF_FFFF
    store_ind    output_addr                 ; mem[mem[output_addr]] = -1
    halt

factorial_overflow:
    load_imm     0xCCCC_CCCC
    store_ind    output_addr                 ; mem[mem[output_addr]] = 0xCCCC_CCCC
    halt
