    .data

spi_base:       .word  0x90

    .text

_start:
    lui      t0, %hi(spi_base)
    addi     t0, t0, %lo(spi_base)
    lw       t0, 0(t0)

    addi     t1, t0, 0        ; SPI_PINS_OUT (software mode)
    addi     t2, t0, 4        ; SPI_PINS_IN  (software mode)
    addi     t3, zero, 32     ; bit counter
    addi     t4, zero, 0      ; received word

    sw       zero, 0(t1)      ; cs=0, clk=0, mosi=0

loop:
    lw       t5, 0(t2)        ; read miso bit from SPI_PINS_IN[0]
    andi     t5, t5, 1
    slli     t4, t4, 1
    or       t4, t4, t5

    slli     t6, t5, 2        ; cs=0, clk=0, mosi=miso_bit
    sw       t6, 0(t1)

    addi     t6, t6, 2        ; cs=0, clk=1, mosi=miso_bit
    sw       t6, 0(t1)
    addi     t6, t6, -2
    sw       t6, 0(t1)        ; cs=0, clk=0, mosi=miso_bit

    addi     t3, t3, -1
    bnez     t3, loop

    addi     t6, zero, 1      ; cs=1, clk=0, mosi=0
    sw       t6, 0(t1)

    halt
