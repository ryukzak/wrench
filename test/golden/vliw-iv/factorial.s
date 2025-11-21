    .data

input_addr:      .word  0x80               ; Input address where the number 'n' is stored
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text
    .org 0x90

_start:
    nop          | lui t0, %hi(input_addr)       | nop | nop                         ; int * input_addr_const = 0x00;
    nop          | addi t0, t0, %lo(input_addr)  | nop | nop                         ; // t0 <- 0x00;

    lw t0, 0(t0) | nop                           | nop | nop                         ; int input_addr = *input_addr_const;
    ; // t0 <- *t0;

    lw t1, 0(t0) | nop                           | nop | nop                         ; int n = *input_addr;
    ; // t1 <- *t0;

    nop          | nop                           | nop | bgt zero, t1, negative_case ; check if negative (if 0 > t1, jump)

factorial_begin:
    nop          | addi t2, zero, 1              | nop | nop                         ; int acc = 1;
    ; // t2 <- 1;

factorial_while:
    nop          | nop                           | nop | beqz t1, factorial_end      ; while (acc != 0) {
    nop          | mulh t3, t2, t1               | nop | nop                         ;   acc *= n   // t2 <- t2 * t1;
    nop          | nop                           | nop | bnez t3, check_overflow     ; Check for overflow
    ; If no overflow, continue
    nop          | mul t2, t2, t1                | nop | nop                         ; acc *= n
    nop          | addi t1, t1, -1               | nop | nop                         ;   n = n - 1  // t1 <- t1 - 1;
    nop          | nop                           | nop | j factorial_while           ; }

check_overflow:
    nop          | lui t0, %hi(output_addr)      | nop | nop                         ; Load upper immediate for output_addr
    nop          | addi t0, t0, %lo(output_addr) | nop | nop                         ; t0 = output_addr
    lw t0, 0(t0) | nop                           | nop | nop
    nop          | lui t4, 0xCCCCC               | nop | nop
    nop          | addi t4, t4, 0xCCC            | nop | nop                         ; Load overflow indicator (0xCC)
    sw t4, 0(t0) | nop                           | nop | nop
    nop          | nop                           | nop | j exit

negative_case:
    nop          | lui t0, %hi(output_addr)      | nop | nop
    nop          | addi t0, t0, %lo(output_addr) | nop | nop
    lw t0, 0(t0) | nop                           | nop | nop
    nop          | addi t4, zero, -1             | nop | nop
    sw t4, 0(t0) | nop                           | nop | nop
    nop          | nop                           | nop | j exit

factorial_end:
    nop          | lui t0, %hi(output_addr)      | nop | nop                         ; int * output_addr_const = 0x04;
    nop          | addi t0, t0, %lo(output_addr) | nop | nop                         ; // t0 <- 0x04;

    lw t0, 0(t0) | nop                           | nop | nop                         ; int output_addr = *output_addr_const;
    ; // t0 <- *t0;

    sw t2, 0(t0) | nop                           | nop | nop                         ; *output_addr_const = acc;
    ; // *t0 = t2;

exit:
    nop          | nop                           | nop | halt
