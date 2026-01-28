    .data

input_addr:      .word  0x80               ; Input address where the number 'n' is stored
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text
    .org 0x90

_start:
    lui t0, %hi(input_addr)       / nop / nop          / nop                         ; int * input_addr_const = 0x00;
    addi t0, t0, %lo(input_addr)  / nop / nop          / nop                         ; // t0 <- 0x00;

    nop                           / nop / lw t0, 0(t0) / nop                         ; int input_addr = *input_addr_const;
    ; // t0 <- *t0;

    nop                           / nop / lw t1, 0(t0) / nop                         ; int n = *input_addr;
    ; // t1 <- *t0;

    nop                           / nop / nop          / bgt zero, t1, negative_case ; check if negative (if 0 > t1, jump)

factorial_begin:
    addi t2, zero, 1              / nop / nop          / nop                         ; int acc = 1;
    ; // t2 <- 1;

factorial_while:
    nop                           / nop / nop          / beqz t1, factorial_end      ; while (acc != 0) {
    mulh t3, t2, t1               / nop / nop          / nop                         ;   acc *= n   // t2 <- t2 * t1;
    nop                           / nop / nop          / bnez t3, check_overflow     ; Check for overflow
    ; If no overflow, continue
    mul t2, t2, t1                / nop / nop          / nop                         ; acc *= n
    addi t1, t1, -1               / nop / nop          / nop                         ;   n = n - 1  // t1 <- t1 - 1;
    nop                           / nop / nop          / j factorial_while           ; }

check_overflow:
    lui t0, %hi(output_addr)      / nop / nop          / nop                         ; Load upper immediate for output_addr
    addi t0, t0, %lo(output_addr) / nop / nop          / nop                         ; t0 = output_addr
    nop                           / nop / lw t0, 0(t0) / nop
    lui t4, 0xCCCCC               / nop / nop          / nop
    addi t4, t4, 0xCCC            / nop / nop          / nop                         ; Load overflow indicator (0xCC)
    nop                           / nop / sw t4, 0(t0) / nop
    nop                           / nop / nop          / j exit

negative_case:
    lui t0, %hi(output_addr)      / nop / nop          / nop
    addi t0, t0, %lo(output_addr) / nop / nop          / nop
    nop                           / nop / lw t0, 0(t0) / nop
    addi t4, zero, -1             / nop / nop          / nop
    nop                           / nop / sw t4, 0(t0) / nop
    nop                           / nop / nop          / j exit

factorial_end:
    lui t0, %hi(output_addr)      / nop / nop          / nop                         ; int * output_addr_const = 0x04;
    addi t0, t0, %lo(output_addr) / nop / nop          / nop                         ; // t0 <- 0x04;

    nop                           / nop / lw t0, 0(t0) / nop                         ; int output_addr = *output_addr_const;
    ; // t0 <- *t0;

    nop                           / nop / sw t2, 0(t0) / nop                         ; *output_addr_const = acc;
    ; // *t0 = t2;

exit:
    nop                           / nop / nop          / halt
