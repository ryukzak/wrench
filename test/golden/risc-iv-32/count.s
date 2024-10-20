    .data

acc:        .word 0       ; Counter accumulator, starts at 0
step:       .word 2       ; Step value for incrementing the counter
out_port:   .word 88      ; Output port address, used for writing the counter value
limit:      .word 21      ; Limit value, stop the program if counter reaches 21

    .text

_start:

    ; Load the step value into register t1
    lui     t0, step      ; Load upper immediate of 'step' into t0
    addi    t0, t0, step  ; Add lower immediate part of 'step' to t0
    lw      t1, 0(t0)     ; Load the value of 'step' into t1 (t1 = step)

    ; Load the accumulator value into register t2
    lui     t0, acc       ; Load upper immediate of 'acc' into t0
    addi    t0, t0, acc   ; Add lower immediate part of 'acc' to t0
    lw      t2, 0(t0)     ; Load the value of 'acc' into t2 (t2 = acc)

    ; Load the address of the output port into register t4
    lui     t3, out_port  ; Load upper immediate of 'out_port' into t3
    addi    t3, t3, out_port ; Add lower immediate part of 'out_port' to t3
    lw      t4, 0(t3)     ; Load the value of 'out_port' into t4 (t4 = out_port)

    ; Load the limit value into register t5
    lui     t6, limit     ; Load upper immediate of 'limit' into t6
    addi    t6, t6, limit ; Add lower immediate part of 'limit' to t6
    lw      t5, 0(t6)     ; Load the value of 'limit' into t5 (t5 = limit)

count:
    ; Increment the counter by the step value
    add     t2, t2, t1    ; Add step value (t1) to accumulator (t2)
    sw      t2, 0(t0)     ; Store the updated counter (t2) back in acc

    ; Output the updated counter value
    sw      t2, 0(t4)     ; Write the updated counter to the output port

    ; Check if the counter has reached the limit
    bgt     t2, t5, end   ; If t2 (acc) >= t5 (limit), branch to halt

    ; Loop again
    j       count         ; Jump back to the start of count loop

end:
    halt
