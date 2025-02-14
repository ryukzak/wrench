    .data

input_addr:      .word  0x80               ; Input address where the number 'n' is stored
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text

_start:
    lui      t0, %hi(input_addr)             ; int * input_addr_const = 0x00;
    addi     t0, t0, %lo(input_addr)         ; // t0 <- 0x00;

    lw       t0, 0(t0)                       ; int input_addr = *input_addr_const; // t0 <- *t0;
    lw       t1, 0(t0)                       ; int n = *input_addr; // t1 <- *t0;

factorial_begin:
    addi     t2, zero, 1                     ; int acc = 1; // t2 <- 1;

factorial_while:
    beqz     t1, factorial_end               ; while (acc != 0) {
    mul      t2, t2, t1                      ;   acc *= n  // t2 <- t2 * t1;
    addi     t1, t1, -1                      ;   n = n - 1  // t1 <- t1 - 1;
    j        factorial_while                 ; }

factorial_end:
    lui      t0, %hi(output_addr)            ; int * output_addr_const = 0x04;
    addi     t0, t0, %lo(output_addr)        ; // t0 <- 0x04;

    lw       t0, 0(t0)                       ; int output_addr = *output_addr_const; // t0 <- *t0;

    sw       t2, 0(t0)                       ; *output_addr_const = acc; // *t0 = t2;

    halt
