     ; Program to simulate different hazards that can arise
     ; Try to adjust seed in config and see that value of T4 can change due to WAW hazard
    .text
_start:
    ; Set t0 = 5, t1 = 3
    addi t0, zero, 5               / addi t1, zero, 3               / nop / nop

    ; Read-After-Read - both ALU slots read t0, this is totaly fine
    add t2, t0, t0                 / mul t3, t0, t1                 / nop / nop
    ; Expected: t2 = 5+5 = 10, t3 = 5*3 = 15

    ; Write-After-Write hazard - both ALU slots write to T0
    ; This is undefined behaviour, result should vary run to run
    addi t4, zero, 100             / addi t4, zero, 200             / nop / nop
    ; Expected: t4 = either 100 or 200 (last write wins)

    ; Write-After-Read hazard - one reads t2, another writes t2
    ; Slots run one by one in random order, so the read may see either value
    addi t2, zero, 99              / mv t5, t2                      / nop / nop
    ; Expected: t2 = 99, t5 = either 10 (old t2) or 99 (new t2)

    nop                            / nop                            / nop / halt
