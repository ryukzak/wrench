; Exercises the {memory:table} report — two text and two data sections,
; partial coverage of the second data section (b_str declared but never read),
; a stack region, and gaps between sections.

    .text
_start:
    ; Set up the stack at 0x200.
    lui      sp, %hi(0x200)
    addi     sp, sp, %lo(0x200)

    ; Read input from IO.
    lui      t0, %hi(input_ptr)
    addi     t0, t0, %lo(input_ptr)
    lw       t0, 0(t0)
    lw       a0, 0(t0)

    addi     sp, sp, -4
    sw       a0, 0(sp)

    jal      ra, work
    halt

    .data
input_ptr:   .word  0x80

    ; Skip past the memory-mapped IO range (0x80..0x87).
    .text
    .org     144
work:
    ; Read a_str byte-by-byte (7 bytes).
    lui      t1, %hi(a_str)
    addi     t1, t1, %lo(a_str)
    addi     t2, zero, 7
read_a:
    lb       t3, 0(t1)
    addi     t1, t1, 1
    addi     t2, t2, -1
    bnez     t2, read_a

    ; Read c_str byte-by-byte (5 bytes). b_str is declared but never touched.
    lui      t1, %hi(c_str)
    addi     t1, t1, %lo(c_str)
    addi     t2, zero, 5
read_c:
    lb       t3, 0(t1)
    addi     t1, t1, 1
    addi     t2, t2, -1
    bnez     t2, read_c

    ; Pop the saved input and write it to the output IO.
    lw       a0, 0(sp)
    addi     sp, sp, 4
    lui      t1, %hi(output_ptr)
    addi     t1, t1, %lo(output_ptr)
    lw       t1, 0(t1)
    sw       a0, 0(t1)
    jr       ra

    .data
output_ptr:  .word  0x84
a_str:       .byte  'aaaaaaa'
b_str:       .byte  'bbbbbb'
c_str:       .byte  'ccccc'
