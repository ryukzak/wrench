    .text

_start:
    lit 2
    lit 0xFFFF_FFFE lit 1 +
    lit 1 + +

    drop

    lit 1 eam

    lit 2
    lit 0xFFFF_FFFE lit 1 +
    lit 1 + +

    drop

    halt
