  .data
aaa:    .word  0x80
bbb:   .word  0x84
d:     .word  0x2

  .text
  .org 0x100

_start:
  @p aaa a! @  ccc  @p bbb a! !  ooo

ccc:
  dup -if ddd  drop -1  ;

ddd:
  dup if mmm

  dup -1 +
  if hhh

eee:
  @p d !b  dup a! 0 0  fff  inv 1 + @b + -if lll  if hhh  @p d 1 + !p d  eee ;

fff:
  31 >r
ggg:
  +/  next ggg  ;

hhh:
  drop 0  ;
kkk:
  drop 1  ;

lll:
  if hhh  kkk  ;

mmm:
  drop -1  ;
ooo:
  halt
