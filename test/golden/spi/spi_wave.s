    .data

spi_out:        .word  0x90
spi_in:         .word  0x94

    .text

_start:
    ; Wave report test with non-standard pin mapping:
    ;   CS   -> addr 0x90 bit 5
    ;   CLK  -> addr 0x90 bit 6
    ;   MOSI -> addr 0x90 bit 7
    ;   MISO -> addr 0x94 bit 3
    lui      t0, %hi(spi_out)
    addi     t0, t0, %lo(spi_out)
    lw       t1, 0(t0)        ; output register address

    lui      t0, %hi(spi_in)
    addi     t0, t0, %lo(spi_in)
    lw       t2, 0(t0)        ; input register address

    ; CS=0, CLK=0, MOSI=0 with custom bit layout
    sw       zero, 0(t1)      ; cs=0, clk=0, mosi=0 (bits 5/6/7)

word_loop:
    addi     t3, zero, 32
    addi     t6, zero, 0

bit_loop:
    ; Read MISO from bit 3
    lw       t4, 0(t2)
    andi     t4, t4, 8
    srli     t4, t4, 3
    slli     t6, t6, 1
    or       t6, t6, t4

    ; Put read bit on MOSI bit 7
    slli     t5, t4, 7        ; mosi bit in bit 7, cs=0 clk=0
    sw       t5, 0(t1)

    ; Generate clock pulse on bit 6 (mode 0 cycle)
    addi     t5, t5, 64       ; clk=1 (bit 6)
    sw       t5, 0(t1)
    addi     t5, t5, -64
    sw       t5, 0(t1)        ; clk=0

    addi     t3, t3, -1
    bnez     t3, bit_loop

    beq      t6, zero, done
    j        word_loop

    ; End transfer: CS=1 on bit 5
done:
    addi     t5, zero, 32     ; cs=1 (bit 5), clk=0, mosi=0
    sw       t5, 0(t1)

    halt
