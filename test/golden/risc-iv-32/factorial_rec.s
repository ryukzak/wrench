    .text

factorial:
    addi     t0, zero, -1
    ble      a0, t0, factorial_return_minus_one
    addi     t0, zero, 1
    ble      a0, t0, factorial_return_one

    addi     sp, sp, -8                      ; Save return address and n on stack
    sw       ra, 4(sp)
    sw       a0, 0(sp)

    ; Recursive call: factorial(n - 1)
    addi     a0, a0, -1
    jal      ra, factorial                   ; Call factorial(n-1)

    lw       t1, 0(sp)                       ; Restore n and return address
    lw       ra, 4(sp)
    addi     sp, sp, 8

    bgt      a1, zero, factorial_overflow_case

    mulh     t3, a0, t1                      ; Check for overflow
    bnez     t3, factorial_overflow_case

    ; Compute n * factorial(n-1)
    mul      a0, a0, t1
    jr       ra                              ; Return

factorial_return_one:
    addi     a0, zero, 1
    jr       ra

factorial_return_minus_one:
    addi     a0, zero, -1
    jr       ra

factorial_overflow_case:
    lui      a0, 0xCCCCC
    addi     a0, a0, 0xCCC                   ; Load overflow indicator (0xCC)
    addi     a1, zero, 1
    jr       ra

    .data

input_addr:      .word  0x80               ; Input address where the number 'n' is stored
output_addr:     .word  0x84               ; Output address where the result should be stored


    .text
    .org     140
_start:
    lui      t0, %hi(input_addr)             ; int * input_addr_const = 0x00;
    addi     t0, t0, %lo(input_addr)         ; // t0 <- 0x00;
    lw       t0, 0(t0)                       ; int input_addr = *input_addr_const;

    lw       a0, 0(t0)                       ; int n = *input_addr;
    addi     a1, zero, 0

    lui      sp, %hi(0x256)                  ; Set up stack pointer
    addi     sp, sp, %lo(0x256)

    jal      ra, factorial                   ; Call factorial(n)

    lui      t0, %hi(output_addr)            ; Load output address
    addi     t0, t0, %lo(output_addr)
    lw       t0, 0(t0)
    sw       a0, 0(t0)                       ; Store result

    halt
