    .text

_start:
    lit 25
    lit 25
    next_call halt_jmp ;

next_call:
    + ;

halt_jmp:
    halt
