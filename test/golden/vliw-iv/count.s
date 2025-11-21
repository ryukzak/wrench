    .data

acc:             .word  0                  ; Counter accumulator, starts at 0
step:            .word  2                  ; Step value for incrementing the counter
out_port:        .word  88                 ; Output port address, used for writing the counter value
limit:           .word  21                 ; Limit value, stop the program if counter reaches 21

    .text
    .org 0x100

_start:

    ; Load the step value into register t1
    nop          | lui t0, %hi(step)        | nop             | nop
    nop          | addi t0, t0, %lo(step)   | nop             | nop
    lw t1, 0(t0) | nop                      | nop             | nop

    ; Load the accumulator value into register t2
    nop          | lui t0, %hi(acc)         | nop             | nop
    nop          | addi t0, t0, %lo(acc)    | nop             | nop
    lw t2, 0(t0) | nop                      | nop             | nop

    ; Load the address of the output port into register t4
    nop          | lui t3, %hi(out_port)    | nop             | nop
    nop          | addi t3, t3, %lo(out_port) | nop           | nop
    lw t4, 0(t3) | nop                      | nop             | nop

    ; Load the limit value into register t5
    nop          | lui t6, %hi(limit)       | nop             | nop
    nop          | addi t6, t6, %lo(limit)  | nop             | nop
    lw t5, 0(t6) | nop                      | nop             | nop

count:
    ; Increment the counter by the step value
    sw t2, 0(t0) | add t2, t2, t1           | nop             | nop

    ; Output the updated counter value
    sw t2, 0(t4) | nop                      | nop             | bgt t2, t5, end

    ; Loop again
    nop          | nop                      | nop             | j count

end:
    nop          | nop                      | nop             | halt
