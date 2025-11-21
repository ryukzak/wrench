    .data

input_addr:      .word  0x80               ; Input address where the number 'n' is stored
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text
    .org 0x100

_start:
    nop          | addi t0, t0, %hi(input_addr)  | nop             | nop
    nop          | add t2, t1, t0                | nop             | nop
    nop          | sub t3, t1, t0                | nop             | nop
    nop          | mul t4, t1, t0                | nop             | nop
    nop          | mulh t5, t1, t0               | nop             | nop
    nop          | div t6, t1, t0                | nop             | nop
    nop          | rem t2, t1, t0                | nop             | nop

    nop          | sll t2, t1, t0                | nop             | nop
    nop          | srl t2, t1, t0                | nop             | nop
    nop          | sra t2, t1, t0                | nop             | nop

    nop          | and t2, t1, t0                | nop             | nop
    nop          | or t2, t1, t0                 | nop             | nop
    nop          | xor t2, t1, t0                | nop             | nop

    nop          | mv t2, t1                     | nop             | nop

    sw t2, 0(t0) | nop                           | nop             | nop
    sb t2, 0(t0) | nop                           | nop             | nop

    nop          | lui t2, 0x1234                | nop             | nop

    lw t2, 0(t0) | nop                           | nop             | nop

    nop          | nop                           | nop             | j jump_label

    nop          | nop                           | nop             | beqz t1, beqz_label
    nop          | nop                           | nop             | bnez t1, bnez_label
    nop          | nop                           | nop             | bgt t1, t0, bgt_label
    nop          | nop                           | nop             | ble t1, t0, ble_label
    nop          | nop                           | nop             | bgtu t1, t0, bgtu_label
    nop          | nop                           | nop             | bleu t1, t0, bleu_label
    nop          | nop                           | nop             | beq t1, t0, beq_label
    nop          | nop                           | nop             | bne t1, t0, bne_label

jump_label:
    nop          | addi t0, t0, 4                | nop             | nop

beqz_label:
    nop          | addi t0, t0, 4                | nop             | nop

bnez_label:
    nop          | addi t0, t0, 4                | nop             | nop

bgt_label:
    nop          | addi t0, t0, 4                | nop             | nop

ble_label:
    nop          | addi t0, t0, 4                | nop             | nop

bgtu_label:
    nop          | addi t0, t0, 4                | nop             | nop

bleu_label:
    nop          | addi t0, t0, 4                | nop             | nop

beq_label:
    nop          | addi t0, t0, 4                | nop             | nop

bne_label:
    nop          | addi t0, t0, 4                | nop             | nop

    nop          | nop                           | nop             | halt
