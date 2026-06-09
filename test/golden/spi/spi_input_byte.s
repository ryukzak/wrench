    .data

spi_pins:       .word  0x90

    .text

_start:
    ; This test focuses on config format `input: [{at, byte}]`
    ; Program reads words until it receives NULL
    lui      t0, %hi(spi_pins)
    addi     t0, t0, %lo(spi_pins)
    lw       t0, 0(t0)

    addi     t1, t0, 0        ; SPI_PINS_OUT
    addi     t2, t0, 4        ; SPI_PINS_IN

    ; Start transfer
    sw       zero, 0(t1)      ; cs=0, clk=0, mosi=0

word_loop:
    addi     t3, zero, 32
    addi     t4, zero, 0

bit_loop:
    ; Mode 0 transfer step (sample on rising, shift on falling)
    lw       t5, 0(t2)
    andi     t5, t5, 1
    slli     t4, t4, 1
    or       t4, t4, t5
    slli     t6, t5, 2
    sw       t6, 0(t1)        ; cs=0, clk=0, mosi=miso

    addi     t6, t6, 2
    sw       t6, 0(t1)        ; rising edge
    addi     t6, t6, -2
    sw       t6, 0(t1)        ; falling edge

    addi     t3, t3, -1
    bnez     t3, bit_loop

    beq      t4, zero, done
    j        word_loop

    ; End transfer
done:
    addi     t6, zero, 1
    sw       t6, 0(t1)        ; cs=1, clk=0, mosi=0

    halt
