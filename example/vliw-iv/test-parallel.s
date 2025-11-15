     ; Program to simulate different hazards that can arise
    .text
_start:
    ; Set x1 = 5, x2 = 3
    nop | addi x1, x0, 5   | addi x2, x0, 3   | nop

    ; Read-After-Read - both ALU slots read x1, this is totaly fine
    nop | add x3, x1, x1   | mul x4, x1, x2   | nop
    ; Expected: x3 = 5+5 = 10, x4 = 5*3 = 15

    ; Write-After-Write hazard - both ALU slots write to x5
    ; This is undefined behaviour, result should vary run to run
    nop | addi x5, x0, 100 | addi x5, x0, 200 | nop
    ; Expected: x5 = either 100 or 200 (last write wins)

    ; Write-After-Read hazard - one reads x3, another writes x3
    ; The read should see the old value (10), not the new value
    nop | addi x3, x0, 99  | mv x6, x3        | nop
    ; Expected: x3 = 99 (new value), x6 = 10 (old x3)

    nop | nop              | nop              | halt
