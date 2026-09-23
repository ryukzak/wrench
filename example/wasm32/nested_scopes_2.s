    .text

_start:
    i32.const 1
    block
        loop
            i32.const 2
            i32.add
        end
    end
    halt
