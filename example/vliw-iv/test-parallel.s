     ; Program to simulate different hazards that can arise
     ; Try to adjust seed in config and see that value of T4 can change due to WAW hazard
    .text
_start:
    ; Set t0 = 5, t1 = 3
    nop | addi t0, zero, 5   | addi t1, zero, 3   | nop

    ; Read-After-Read - both ALU slots read t0, this is totaly fine
    nop | add t2, t0, t0     | mul t3, t0, t1     | nop
    ; Expected: t2 = 5+5 = 10, t3 = 5*3 = 15

    ; Write-After-Write hazard - both ALU slots write to T0
    ; This is undefined behaviour, result should vary run to run
    nop | addi t4, zero, 100 | addi t4, zero, 200 | nop
    ; Expected: t4 = either 100 or 200 (last write wins)

    ; Write-After-Read hazard - one reads t2, another writes t2
    ; The read should see the old value (10), not the new value
    nop | addi t2, zero, 99  | mv t5, t2          | nop
    ; Expected: t2 = 99 (new value), t5 = 10 (old t2)

    nop | nop                | nop                | halt
