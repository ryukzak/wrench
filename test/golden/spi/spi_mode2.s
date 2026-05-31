    .data

spi_base:       .word  0x90

    .text

_start:
    ; Load SPI base
    lui      t0, %hi(spi_base)
    addi     t0, t0, %lo(spi_base)
    lw       t0, 0(t0)

    addi     t1, t0, 0        ; SPI_PINS_OUT
    addi     t2, t0, 4        ; SPI_PINS_IN

    ; Mode 2 has idle clock high (CPOL=1)
    ; Keep bus inactive first: CS=1, CLK=1
    addi     t6, zero, 3
    sw       t6, 0(t1)        ; cs=1, clk=1, mosi=0 (inactive)

    ; Activate slave while CLK is high
    ; In our model for mode 2, activation also primes first MISO bit
    addi     t6, zero, 2
    sw       t6, 0(t1)        ; cs=0, clk=1, mosi=0 (activate, mode2 prime)

word_loop:
    addi     t3, zero, 32
    addi     t4, zero, 0

bit_loop:
    ; Mode 2:
    ;   sample on falling edge
    ;   shift on rising edge
    ; Read visible MISO bit before sample edge
    lw       t5, 0(t2)
    andi     t5, t5, 1
    slli     t4, t4, 1
    or       t4, t4, t5
    slli     t6, t5, 2
    addi     t6, t6, 2
    sw       t6, 0(t1)

    ; Falling edge is sample edge
    addi     t6, t6, -2
    sw       t6, 0(t1)        ; falling edge (sample edge)

    ; Rising edge is shift edge
    addi     t6, t6, 2
    sw       t6, 0(t1)        ; rising edge (shift edge)

    addi     t3, t3, -1
    bnez     t3, bit_loop

    beq      t4, zero, done
    j        word_loop

    ; Stop transfer with idle high clock
done:
    addi     t6, zero, 3
    sw       t6, 0(t1)        ; cs=1, clk=1, mosi=0

    halt
