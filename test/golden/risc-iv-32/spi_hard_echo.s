    .data

spi_data:       .word  0x90

    .text

_start:
    lui      t0, %hi(spi_data)
    addi     t0, t0, %lo(spi_data)
    lw       t0, 0(t0)

loop:
    lw       t2, 4(t0)        ; read STATUS register (DATA + 4)
    andi     t2, t2, 1        ; check MISO_READY bit
    beq      t2, zero, loop   ; if not ready, wait

    lw       t2, 0(t0)        ; read DATA
    beq      t2, zero, done   ; if value == 0, exit loop

    sw       t2, 0(t0)        ; echo: write data back to SPI
    j        loop

done:
    halt
