    .data

spi_pins:       .word  0x90

    .text

_start:
    ; Load first SPI pin word address used by this program
    lui      t0, %hi(spi_pins)
    addi     t0, t0, %lo(spi_pins)
    lw       t0, 0(t0)

    ; Standard pin mapping for this test:
    ;   PINS_OUT = base + 0
    ;   PINS_IN  = base + 4
    addi     t1, t0, 0        ; SPI_PINS_OUT
    addi     t2, t0, 4        ; SPI_PINS_IN

    ; Start transfer: CS=0, CLK=0, MOSI=0
    sw       zero, 0(t1)      ; cs=0, clk=0, mosi=0

word_loop:
    ; Transfer one word and reconstruct it in t4
    addi     t3, zero, 32     ; bit counter
    addi     t4, zero, 0      ; reconstructed word

bit_loop:
    ; In mode 0:
    ;   sample on rising edge
    ;   shift on falling edge
    ; Read current MISO bit (bit 0 of PINS_IN) before generating the sample edge.
    lw       t5, 0(t2)
    andi     t5, t5, 1
    slli     t4, t4, 1
    or       t4, t4, t5

    ; Drive MOSI to mirror the bit we just read
    slli     t6, t5, 2        ; cs=0, clk=0, mosi=miso_bit
    sw       t6, 0(t1)

    ; Rising edge -> sample edge in mode 0
    addi     t6, t6, 2        ; cs=0, clk=1, mosi=miso_bit
    sw       t6, 0(t1)

    ; Falling edge -> shift edge in mode 0
    addi     t6, t6, -2
    sw       t6, 0(t1)        ; cs=0, clk=0, mosi=miso_bit

    addi     t3, t3, -1
    bnez     t3, bit_loop

    ; Stop when received word is null-terminator
    beq      t4, zero, done
    j        word_loop

    ; Stop transfer: CS=1
done:
    addi     t6, zero, 1      ; cs=1, clk=0, mosi=0
    sw       t6, 0(t1)

    halt
