    .data

spi_base:       .word  0x90

    .text

_start:
    ; Load SPI base.
    lui      t0, %hi(spi_base)
    addi     t0, t0, %lo(spi_base)
    lw       t0, 0(t0)

    addi     t1, t0, 0        ; SPI_PINS_OUT
    addi     t2, t0, 4        ; SPI_PINS_IN

    ; Mode 3 idle clock is high (CPOL=1), so start inactive with CLK=1
    addi     t6, zero, 3
    sw       t6, 0(t1)        ; cs=1, clk=1, mosi=0 (inactive)

    ; Activate transfer (CS=0) while keeping idle clock level
    addi     t6, zero, 2
    sw       t6, 0(t1)        ; cs=0, clk=1, mosi=0 (activate)

    ; Mode 3 requires an initial falling edge before first sample
    addi     t6, zero, 0
    sw       t6, 0(t1)        ; cs=0, clk=0, mosi=0

word_loop:
    addi     t3, zero, 32
    addi     t4, zero, 0

bit_loop:
    ; Mode 3:
    ;   shift on falling edge
    ;   sample on rising edge
    ; At this point current bit is already shifted (we are at CLK=0)

    ; Read current MISO and mirror to MOSI
    lw       t5, 0(t2)
    andi     t5, t5, 1
    slli     t4, t4, 1
    or       t4, t4, t5
    slli     t6, t5, 2
    sw       t6, 0(t1)        ; cs=0, clk=0, mosi=miso

    ; Rising edge is sample edge
    addi     t6, t6, 2
    sw       t6, 0(t1)        ; rising edge: sample mode 3

    ; Falling edge is shift edge for next bit
    addi     t6, t6, -2
    sw       t6, 0(t1)

    addi     t3, t3, -1
    bnez     t3, bit_loop

    beq      t4, zero, done
    j        word_loop

    ; Stop transfer
done:
    addi     t6, zero, 1
    sw       t6, 0(t1)        ; cs=1, clk=0, mosi=0

    halt
