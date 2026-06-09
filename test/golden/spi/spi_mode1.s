    .data

spi_pins:       .word  0x90

    .text

_start:
    ; Load first SPI pin word address used by this program
    lui      t0, %hi(spi_pins)
    addi     t0, t0, %lo(spi_pins)
    lw       t0, 0(t0)

    ; Standard mapping in this test
    addi     t1, t0, 0        ; SPI_PINS_OUT
    addi     t2, t0, 4        ; SPI_PINS_IN

    ; Start transfer: CS=0, CLK=0
    sw       zero, 0(t1)      ; cs=0, clk=0, mosi=0

    ; Mode 1 requires one leading shift edge before first sample
    addi     t6, zero, 2
    sw       t6, 0(t1)        ; cs=0, clk=1, mosi=0

word_loop:
    ; Reconstruct one received word in t4
    addi     t3, zero, 32
    addi     t4, zero, 0

bit_loop:
    ; Mode 1:
    ;   shift on rising edge
    ;   sample on falling edge
    ; We are currently at CLK=1, so current bit is already shifted out
    lw       t5, 0(t2)
    andi     t5, t5, 1
    slli     t4, t4, 1
    or       t4, t4, t5

    ; Drive MOSI while CLK=1
    slli     t6, t5, 2
    addi     t6, t6, 2
    sw       t6, 0(t1)

    ; Falling edge -> sample edge
    addi     t6, t6, -2
    sw       t6, 0(t1)

    ; Rising edge -> shift edge for next bit
    addi     t6, t6, 2
    sw       t6, 0(t1)

    addi     t3, t3, -1
    bnez     t3, bit_loop

    ; Stop on null terminator
    beq      t4, zero, done
    j        word_loop

done:
    ; Stop transfer: CS=1, CLK=0
    addi     t6, zero, 1
    sw       t6, 0(t1)

    halt
