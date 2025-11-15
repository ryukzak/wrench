    .data
input_addr:      .word  0x80               ; Input address where the number 'n' is stored
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text
    .org 0x100
_start:
    ; Load input_addr constant and get the address
    nop          | lui x5, %hi(input_addr)       | nop             | nop
    nop          | addi x5, x5, %lo(input_addr)  | nop             | nop
    lw x5, 0(x5) | nop                           | nop             | nop
    ; x5 now contains the input address

    ; Load n from input address
    lw x6, 0(x5) | nop                           | nop             | nop
    ; x6 now contains n

factorial_begin:
    ; Initialize accumulator to 1
    nop          | addi x7, x0, 1                | nop             | nop
    ; x7 = acc = 1

factorial_while:
    ; Check if n == 0, if so exit loop
    nop          | nop                           | nop             | beqz x6, factorial_end
    ; acc *= n, n = n - 1 in parallel
    nop          | mul x7, x7, x6                | addi x6, x6, -1 | j factorial_while

factorial_end:
    ; Load output_addr constant
    nop          | lui x5, %hi(output_addr)      | nop             | nop
    nop          | addi x5, x5, %lo(output_addr) | nop             | nop
    lw x5, 0(x5) | nop                           | nop             | nop
    ; x5 now contains the output address

    ; Store result
    sw x7, 0(x5) | nop                           | nop             | nop
    ; *output_addr = acc

    nop          | nop                           | nop             | halt
