    .data

buf:             .byte  31, 'Hello\n\0World!\0\0\0'
output_addr:     .word  0x84               \ Output address where the result should be stored

    .text

_start:
    @p output_addr b!        \ b for output

    lit buf a!               \ a for buf address

    14                   \ hardcoded counter on T

while:
    dup
    if end

    @+ 255 and

    !b

    -1 +
    while ;

end:
    halt
