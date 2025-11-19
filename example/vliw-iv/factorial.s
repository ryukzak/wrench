    .data
input_addr:      .word  0x80               ; Input address where the number 'n' is stored
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text
    .org 0x100
_start:
    ; Load input_addr constant and get the address
    nop          | lui t0, %hi(input_addr)       | nop             | nop
    nop          | addi t0, t0, %lo(input_addr)  | nop             | nop
    lw t0, 0(t0) | nop                           | nop             | nop
    ; t0 now contains the input address

    ; Load n from input address
    lw t1, 0(t0) | nop                           | nop             | nop
    ; t1 now contains n

factorial_begin:
    ; Initialize accumulator to 1
    nop          | addi t2, zero, 1              | nop             | nop
    ; t2 = acc = 1

factorial_while:
    ; Check if n == 0, if so exit loop
    nop          | nop                           | nop             | beqz t1, factorial_end
    ; acc *= n, n = n - 1 in parallel
    nop          | mul t2, t2, t1                | addi t1, t1, -1 | j factorial_while

factorial_end:
    ; Load output_addr constant
    nop          | lui t0, %hi(output_addr)      | nop             | nop
    nop          | addi t0, t0, %lo(output_addr) | nop             | nop
    lw t0, 0(t0) | nop                           | nop             | nop
    ; t0 now contains the output address

    ; Store result
    sw t2, 0(t0) | nop                           | nop             | nop
    ; *output_addr = acc

    nop          | nop                           | nop             | halt
