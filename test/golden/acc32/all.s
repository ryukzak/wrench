    .data

data_ptr:        .word  0x24               ; Address of `temp`, for the indirect forms
output_addr:     .word  0x84               ; Output address where the result should be stored
n:               .word  0x00               ; Variable to store the number 'n'
result:          .word  0x01               ; Variable to store the result of the factorial, initialized to 1
const_1:         .word  0x01               ; Constant 1
const_2:         .word  0x02               ; Constant 2
const_3:         .word  0x03               ; Constant 3
const_4:         .word  0x04               ; Constant 4
const_5:         .word  0x05               ; Constant 5
temp:            .word  0x00               ; Temporary storage -- must stay at 0x24, see data_ptr

    .text

_start:
    ; Load immediate value
    load_imm     0x10                        ; acc <- 0x10
    store_addr   temp                        ; mem[temp] <- acc

    ; Load from address
    load_addr    const_1                     ; acc <- mem[const_1]
    store_addr   temp                        ; mem[temp] <- acc

    ; Load relative -- the displacement is to a .data cell, never to code
    load         const_2                     ; acc <- mem[const_2]
    store_addr   temp                        ; mem[temp] <- acc

    ; Load indirect
    load         data_ptr                    ; acc <- mem[data_ptr]
    load_acc                                 ; acc <- mem[acc]
    store_addr   temp                        ; mem[temp] <- acc

    ; Store to address
    load_imm     0x20                        ; acc <- 0x20
    store_addr   temp                        ; mem[temp] <- acc

    ; Store relative -- again into .data, not into the instruction stream
    load_imm     0x30                        ; acc <- 0x30
    store        temp                        ; mem[temp] <- acc

    ; Store indirect
    load_imm     0x40                        ; acc <- 0x40
    store_ind    data_ptr                    ; mem[mem[data_ptr]] <- acc

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

    ; Flag clears
    clv                                      ; clear overflow
    clc                                      ; clear carry

    ; Branches. Each targets the instruction right after it, so every one
    ; executes and control continues in order whether it is taken or not.
    load_imm     0x00                        ; acc <- 0
    beqz         at_bnez                     ; taken
at_bnez:
    load_imm     0x01                        ; acc <- 1
    bnez         at_bgtz                     ; taken
at_bgtz:
    bgtz         at_bgez                     ; taken
at_bgez:
    bgez         at_bltz                     ; taken
at_bltz:
    load_imm     -1                          ; acc <- -1
    bltz         at_bvs                      ; taken
at_bvs:
    bvs          at_bvc                      ; not taken -- clv cleared it
at_bvc:
    bvc          at_bcs                      ; taken
at_bcs:
    bcs          at_bcc                      ; not taken -- clc cleared it
at_bcc:
    bcc          at_jmp                      ; taken
at_jmp:
    jmp          jump_target

jump_target:
    ; Halt the machine
    halt
