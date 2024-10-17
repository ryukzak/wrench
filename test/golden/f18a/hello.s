    .data

buf:               .byte 'Hello\n\0World'
output_addr:       .word 0x84    \ Output address where the result should be stored

    .text

_start:
    @p buf a!         \ a for buf address
    @p output_addr b! \ b for output

    @p 10             \ hardcoded counter on T

while:
    if end

    @+
    @p 255 \ FIXME: 0xFF
    and

    !b
    while ;

end:
    halt
