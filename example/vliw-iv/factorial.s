    .data
input_addr:      .word  0x80               ; Input address where the number 'n' is stored
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text
    .org 0x100
_start:
    ; Load input_addr constant and get the address
    lui t0, %hi(input_addr)       /                 /              /
    addi t0, t0, %lo(input_addr)  /                 /              /
                                  /                 / lw t0, 0(t0) /
    ; t0 now contains the input address

    ; Load n from input address
                                  /                 / lw t1, 0(t0) /
    ; t1 now contains n

factorial_begin:
    ; Initialize accumulator to 1
    addi t2, zero, 1              /                 /              /
    ; t2 = acc = 1

factorial_while:
    ; Check if n == 0, if so exit loop
                                  /                 /              / beqz t1, factorial_end
    ; acc *= n, n = n - 1 in parallel
    mul t2, t2, t1                / addi t1, t1, -1 /              / j factorial_while

factorial_end:
    ; Load output_addr constant
    lui t0, %hi(output_addr)      /                 /              /
    addi t0, t0, %lo(output_addr) /                 /              /
                                  /                 / lw t0, 0(t0) /
    ; t0 now contains the output address

    ; Store result
                                  /                 / sw t2, 0(t0) /
    ; *output_addr = acc

                                  /                 /              / halt
