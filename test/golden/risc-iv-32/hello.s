    .data

buf:               .byte 'Hello\n\0World!\0\0\0\0'
output_addr:       .word 0x84    ; Output address where the result should be stored

    .text

_start:
    lui      t0, %hi(output_addr)     ; int * output_addr_const = *output_addr;
    addi t0, t0, %lo(output_addr)

    lw      t0, 0(t0)            ; int output_addr = *output_addr_const;

; FIXME:    lui      t1, buf             ; chat * ptr = buf;
    addi t1, t1, buf

    addi      t2, zero, 13               ; int n = 13;

    ; t0 -- output_addr
    ; t1 -- ptr
    ; t2 -- n
    ; t3 -- tmp

while:
    beqz    t2, end              ; while (acc != 0) {
    addi    t2, t2, -1           ;   n--

    lw      t3, 0(t1)            ;   tmp = *buf
    sb      t3, 0(t0)            ;   *output_addr = tmp;

    addi    t1, t1, 1            ;   ptr++

    j while                      ; }

end:
    halt
