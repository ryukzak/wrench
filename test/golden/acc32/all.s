    .data

input_addr:      .word  0x80               ; Input address where the number 'n' is stored
output_addr:     .word  0x84               ; Output address where the result should be stored
n:               .word  0x00               ; Variable to store the number 'n'
result:          .word  0x01               ; Variable to store the result of the factorial, initialized to 1
const_1:         .word  0x01               ; Constant 1
const_2:         .word  0x02               ; Constant 2
const_3:         .word  0x03               ; Constant 3
const_4:         .word  0x04               ; Constant 4
const_5:         .word  0x05               ; Constant 5
temp:            .word  0x00               ; Temporary storage

    .text

_start:
    ; Load immediate value
    load_imm     0x10                        ; acc <- 0x10
    store_addr   temp                        ; mem[temp] <- acc

    ; Load from address
    load_addr    const_1                     ; acc <- mem[const_1]
    store_addr   temp                        ; mem[temp] <- acc

    ; Load relative
    load         2                           ; acc <- mem[pc + 2]
    store_addr   temp                        ; mem[temp] <- acc

    ; Load indirect
    load     input_addr                  ; acc <- mem[mem[input_addr]]
    store_addr   temp                        ; mem[temp] <- acc

    ; Store to address
    load_imm     0x20                        ; acc <- 0x20
    store_addr   temp                        ; mem[temp] <- acc

    ; Store relative
    load         0x30                        ; acc <- 0x30
    store        2                           ; mem[pc + 2] <- acc

    ; Store indirect
    load_imm     0x40                        ; acc <- 0x40
    store_ind    input_addr                  ; mem[mem[input_addr]] <- acc

    ; Arithmetic operations
    load_addr    const_2                     ; acc <- mem[const_2]
    add          const_3                     ; acc += mem[const_3]
    store_addr   temp                        ; mem[temp] <- acc

    load_addr    const_5                     ; acc <- mem[const_5]
    sub          const_3                     ; acc -= mem[const_3]
    store_addr   temp                        ; mem[temp] <- acc

    load_addr    const_2                     ; acc <- mem[const_2]
    mul          const_3                     ; acc *= mem[const_3]
    store_addr   temp                        ; mem[temp] <- acc

    load_addr    const_5                     ; acc <- mem[const_5]
    div          const_2                     ; acc /= mem[const_2]
    store_addr   temp                        ; mem[temp] <- acc

    load_addr    const_5                     ; acc <- mem[const_5]
    rem          const_3                     ; acc %= mem[const_3]
    store_addr   temp                        ; mem[temp] <- acc

    ; Bitwise operations
    load_addr    const_1                     ; acc <- mem[const_1]
    shiftl       const_2                     ; acc <<= mem[const_2]
    store_addr   temp                        ; mem[temp] <- acc

    load_addr    const_4                     ; acc <- mem[const_4]
    shiftr       const_1                     ; acc >>= mem[const_1]
    store_addr   temp                        ; mem[temp] <- acc

    load_addr    const_3                     ; acc <- mem[const_3]
    and          const_1                     ; acc &= mem[const_1]
    store_addr   temp                        ; mem[temp] <- acc

    load_addr    const_2                     ; acc <- mem[const_2]
    or           const_1                     ; acc |= mem[const_1]
    store_addr   temp                        ; mem[temp] <- acc

    load_addr    const_3                     ; acc <- mem[const_3]
    xor          const_1                     ; acc ^= mem[const_1]
    store_addr   temp                        ; mem[temp] <- acc

    load_addr    const_5                     ; acc <- mem[const_5]
    not                                      ; acc = ~acc
    store_addr   temp                        ; mem[temp] <- acc

    ; Jump instructions
    load_imm     0x00                        ; acc <- 0x00
    beqz         jump_target                 ; if acc == 0, jump to jump_target

    load_imm     0x01                        ; acc <- 0x01
    bnez         jump_target                 ; if acc != 0, jump to jump_target

    load_imm     0x02                        ; acc <- 0x02
    bgt          jump_target                 ; if acc > 0, jump to jump_target

    load_imm     0xFF                        ; acc <- 0xFF
    ble          jump_target                 ; if acc < 0, jump to jump_target

jump_target:
    ; Halt the machine
    halt
