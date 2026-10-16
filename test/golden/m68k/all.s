    ; Every m68k instruction, and every addressing mode in every position
    ; it is legal in, in a program that runs to halt.
    ;
    ; The space is 32 mnemonics x 5 source modes x 4 destination modes x 2
    ; sizes, which is over 900 lines if enumerated. It does not need to be:
    ; an instruction is "fetch the source through its mode, apply the
    ; operation, store through the destination's mode", and the mode half is
    ; shared machinery rather than something each mnemonic reimplements. So
    ; each dimension is covered once -- every mode in both positions, every
    ; mnemonic, both sizes -- rather than every combination of them. The mode
    ; sweep comes first and doubles as the cost table: what an instruction
    ; costs in memory accesses follows from its modes, not its mnemonic.

    .data

buf:             .word  0, 0, 0, 0         ; scratch the indirect modes walk over
val:             .word  0x7
stack_top:       .word  0x200             ; jsr/rts need A7 pointing somewhere

    .text

_start:
    ; --- the stack pointer, before anything calls -------------------
    movea.l  stack_top, A7                  ; imm   -> An
    movea.l  (A7), A7                       ; d(An) -> An

    ; --- An, legal as an operand only for movea ---------------------
    movea.l  buf, A0                        ; imm  -> An
    movea.l  A0, A1                         ; An   -> An

    ; --- mode sweep: each source mode once, each destination mode once
    move.l   0x7, D0                       ; imm    -> Dn
    move.l   D0, 0(A0)                      ; Dn     -> d(An)
    move.l   0(A0), (A1)+                   ; d(An)  -> (An)+
    move.l   (A0)+, -(A1)                   ; (An)+  -> -(An)
    move.l   -(A0), D1                      ; -(An)  -> Dn
    move.b   0x1, 0(A0)                    ; byte size, into memory

    ; --- where source and destination genuinely interact -----------
    move.l   (A0)+, (A0)+                   ; same register, both sides
    move.l   -(A0), -(A0)                   ; and the other direction

    ; --- one line per remaining mnemonic, register-direct ----------
    move.l   0x5, D0
    move.l   0x3, D1
    add.l    D0, D1                         ; D1 += D0
    sub.l    D0, D1                         ; D1 -= D0
    and.l    D0, D1
    or.l     D0, D1
    xor.l    D0, D1
    cmp.l    D0, D1                         ; flags only
    mul.l    D0, D1
    move.l   0x4, D2
    div.l    D2, D1                         ; divisor is non-zero
    not.l    D1
    neg.l    D1
    clr.l    D1
    asl.l    0x1, D1                       ; shift count as a literal
    asr.l    D0, D1                         ; shift count in a register
    lsl.l    0x1, D1
    lsr.l    0x1, D1
    not.b    D1                             ; byte size on a register

    ; --- branches, each targeting the line after it ----------------
    clr.l    D1                             ; sets Z, clears N/V/C
    beq      at_bne
at_bne:
    bne      at_bgt
at_bgt:
    bgt      at_bge
at_bge:
    bge      at_blt
at_blt:
    blt      at_ble
at_ble:
    ble      at_bmi
at_bmi:
    bmi      at_bpl
at_bpl:
    bpl      at_bcc
at_bcc:
    bcc      at_bcs
at_bcs:
    bcs      at_bvc
at_bvc:
    bvc      at_bvs
at_bvs:
    bvs      at_call

    ; --- call, return, jump ----------------------------------------
at_call:
    jsr      subr
    jmp      done

subr:
    rts

done:
    halt
