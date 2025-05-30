    .data

input_addr:      .word  0x80
output_addr:     .word  0x84

    .text

_start:
    movea.l  input_addr, A6
    movea.l  (A6), A6

    movea.l  output_addr, A7
    movea.l  (A7), A7

    move.l   (A6), D5
    lsl.l    1, D5
    move.l   D5, (A7)

    move.l   (A6), D5
    lsr.l    1, D5
    move.l   D5, (A7)

    move.l   (A6), D5
    asl.l    1, D5
    move.l   D5, (A7)

    move.l   (A6), D5
    asr.l    1, D5
    move.l   D5, (A7)

end:
    halt
