    .data

input_addr:      .word  0x80               ; Input address where the number 'n' is stored
output_addr:     .word  0x84               ; Output address where the result should be stored

    .text

_start:
    addi     t0, t0, %hi(input_addr)         ; Add immediate
    add      t2, t1, t0                      ; Add
    sub      t3, t1, t0                      ; Subtract
    mul      t4, t1, t0                      ; Multiply
    mulh     t5, t1, t0                      ; Multiply high
    div      t6, t1, t0                      ; Divide
    rem      t2, t1, t0                      ; Remainder

    sll      t2, t1, t0                      ; Logical shift left
    srl      t2, t1, t0                      ; Logical shift right
    sra      t2, t1, t0                      ; Arithmetic shift right

    and      t2, t1, t0                      ; Bitwise AND
    or       t2, t1, t0                      ; Bitwise OR
    xor      t2, t1, t0                      ; Bitwise XOR

    mv       t2, t1                          ; Move

    sw       t2, 0(t0)                       ; Store word
    sb       t2, 0(t0)                       ; Store byte

    lui      t2, 0x1234                      ; Load upper immediate

    lw       t2, 0(t0)                       ; Load word

    j        jump_label                      ; Jump

    beqz     t1, beqz_label                  ; Branch if equal to zero
    bnez     t1, bnez_label                  ; Branch if not equal to zero
    bgt      t1, t0, bgt_label               ; Branch if greater than
    ble      t1, t0, ble_label               ; Branch if less than or equal
    bgtu     t1, t0, bgtu_label              ; Branch if greater than (unsigned)
    bleu     t1, t0, bleu_label              ; Branch if less than or equal (unsigned)
    beq      t1, t0, beq_label               ; Branch if equal
    bne      t1, t0, bne_label               ; Branch if not equal

jump_label:
    addi     t0, t0, 4                       ; Add immediate to simulate jump

beqz_label:
    addi     t0, t0, 4                       ; Add immediate to simulate branch

bnez_label:
    addi     t0, t0, 4                       ; Add immediate to simulate branch

bgt_label:
    addi     t0, t0, 4                       ; Add immediate to simulate branch

ble_label:
    addi     t0, t0, 4                       ; Add immediate to simulate branch

bgtu_label:
    addi     t0, t0, 4                       ; Add immediate to simulate branch

bleu_label:
    addi     t0, t0, 4                       ; Add immediate to simulate branch

beq_label:
    addi     t0, t0, 4                       ; Add immediate to simulate branch

bne_label:
    addi     t0, t0, 4                       ; Add immediate to simulate branch

    halt                                     ; Halt the machine
