  .data
aaa:    .word  0x80
bbb:   .word  0x84
d:     .word  0x2

  .text
  .org 0x100

_start:
  @p aaa a! @  ccc  @p bbb a! !  ooo

ccc:
  dup -if ddd  drop lit -1  ;

ddd:
  dup if mmm

  dup lit -1 +
  if hhh

eee:
  @p d !b  dup a! lit 0 lit 0  fff  inv lit 1 + @b + -if lll  if hhh  @p d lit 1 + !p d  eee ;

fff:
  lit 31 >r
ggg:
  +/  next ggg  ;

hhh:
  drop lit 0  ;
kkk:
  drop lit 1  ;

lll:
  if hhh  kkk  ;

mmm:
  drop lit -1  ;
ooo:
  halt
