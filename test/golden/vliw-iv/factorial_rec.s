    .data

input_addr:      .word  0x80               ; Input address where the number 'n' is stored
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text
    .org 0x100
factorial:
    nop          | addi t0, zero, -1             | nop | nop
    nop          | nop                           | nop | ble a0, t0, factorial_return_minus_one
    nop          | addi t0, zero, 1              | nop | nop
    nop          | nop                           | nop | ble a0, t0, factorial_return_one

    nop          | addi sp, sp, -8               | nop | nop
    sw ra, 4(sp) | nop                           | nop | nop
    sw a0, 0(sp) | nop                           | nop | nop

    ; Recursive call: factorial(n - 1)
    nop          | addi a0, a0, -1               | nop | nop
    nop          | nop                           | nop | jal ra, factorial

    lw t1, 0(sp) | nop                           | nop | nop
    lw ra, 4(sp) | nop                           | nop | nop
    nop          | addi sp, sp, 8                | nop | nop

    nop          | nop                           | nop | bgt a1, zero, factorial_overflow_case

    nop          | mulh t3, a0, t1               | nop | nop
    nop          | nop                           | nop | bnez t3, factorial_overflow_case

    ; Compute n * factorial(n-1)
    nop          | mul a0, a0, t1                | nop | nop
    nop          | nop                           | nop | jr ra

factorial_return_one:
    nop          | addi a0, zero, 1              | nop | nop
    nop          | nop                           | nop | jr ra

factorial_return_minus_one:
    nop          | addi a0, zero, -1             | nop | nop
    nop          | nop                           | nop | jr ra

factorial_overflow_case:
    nop          | lui a0, 0xCCCCC               | nop | nop
    nop          | addi a0, a0, 0xCCC            | nop | nop
    nop          | addi a1, zero, 1              | nop | nop
    nop          | nop                           | nop | jr ra

_start:
    nop          | lui t0, %hi(input_addr)       | nop | nop
    nop          | addi t0, t0, %lo(input_addr)  | nop | nop
    lw t0, 0(t0) | nop                           | nop | nop

    lw a0, 0(t0) | nop                           | nop | nop
    nop          | addi a1, zero, 0              | nop | nop

    nop          | lui sp, %hi(0x512)            | nop | nop
    nop          | addi sp, sp, %lo(0x512)       | nop | nop

    nop          | nop                           | nop | jal ra, factorial

    nop          | lui t0, %hi(output_addr)      | nop | nop
    nop          | addi t0, t0, %lo(output_addr) | nop | nop
    lw t0, 0(t0) | nop                           | nop | nop
    sw a0, 0(t0) | nop                           | nop | nop

    nop          | nop                           | nop | halt
