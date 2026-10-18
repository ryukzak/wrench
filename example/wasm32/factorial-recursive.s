    .text

_start:
    i32.const 0x84                           ; output address, under the result
    i32.const 0x80
    i32.load                                 ; int n = *input_addr;
    ; call 1, 1 pops the target, and the 1 value under it (n) becomes local 0
    ; of a new frame; a call record on the control stack keeps the return
    ; address. return leaves 1 value, the result, where n was, and execution
    ; continues after the call.
    i32.const factorial
    call 1, 1
    i32.store                                ; *output_addr = factorial(n);
    halt

    ; int factorial(int n)
factorial:
    local.get 0
    i32.const 1
    i32.le_s
    ; if pops the flag: non-zero runs the code up to end, zero skips past end.
    if                                       ;   if (n <= 1)
        i32.const 1
        return                               ;     return 1;
    end

    local.get 0
    local.get 0
    i32.const 1
    i32.sub
    ; The inner call gets a frame of its own, so this frame's n stays put.
    i32.const factorial
    call 1, 1
    i32.mul
    return                                   ;   return n * factorial(n - 1);
