    .data

    \ be aware, that it is not a pstr or cstr. It is just a buffer
output_addr:     .word  0x84               \ Output address where the result should be stored
buf:             .byte  'Hello\n\0World'

    .text

_start:
    @p output_addr b!                        \ b for output

    lit buf a!                               \ a for buf address

    lit 12                                   \ hardcoded counter on T

while:
    dup
    if end

    @+ lit 0xFF and

    !b

    lit -1 +
    while ;

end:
    halt
