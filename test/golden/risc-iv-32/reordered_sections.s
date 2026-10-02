     ; Exercises .org placing sections out of their textual order: the file
     ; declares .text/.data/.text/.data, but the .org values put the *second*
     ; .data section (output_ptr) at a lower address than the *first* one
     ; (input_ptr) -- the two data sections end up swapped relative to how
     ; they appear in the file. {memory:table} (address order) should read
     ; text, data, text, data with output_ptr's cluster before input_ptr's.

    .text
_start:
    lui      t0, %hi(input_ptr)
    addi     t0, t0, %lo(input_ptr)
    lw       t0, 0(t0)
    lw       a0, 0(t0)
    jal      ra, work
    halt

    .data
.org             0x60
input_ptr:       .word  0x80

    .text
    .org     0x40
work:
    lui      t1, %hi(output_ptr)
    addi     t1, t1, %lo(output_ptr)
    lw       t1, 0(t1)
    sw       a0, 0(t1)
    jr       ra

    .data
.org             0x20
output_ptr:      .word  0x84
