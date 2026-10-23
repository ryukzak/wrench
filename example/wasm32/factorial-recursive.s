    .text

    ; factorial calls itself, so several calls are active at once. Each one
    ; has a frame of its own on the operand stack: its locals first, then its
    ; operands. A call record on the control stack remembers the way back to
    ; the caller's frame.

_start:
    i32.const 0x84                           ; output address, under the result
    i32.const 0x80
    i32.load                                 ; int n = *input_addr;
    i32.const factorial
    ; call 1, 1 pops the target, then allocates the callee's frame without
    ; copying anything:
    ;   - the 1 value on top of the stack (n) becomes the callee's local 0,
    ;     and frameBase points at it; _start's frame now ends under it;
    ;   - an 8-byte call record goes on the control stack, keeping _start's
    ;     frameBase and local count, the return address, and the result
    ;     count 1.
    call 1, 1
    i32.store                                ; *output_addr = factorial(n);
    halt

    ; int factorial(int n)
    ; .frame names the function's locals, here only the parameter n, local 0.
    ; A function that needs more locals names them after the parentheses and
    ; allocates them with locals.reserve as its first instruction: they are
    ; the next words of the frame, after the parameters and before any
    ; operand.
factorial:
    .frame ($n)
    local.get $n
    i32.const 1
    i32.le_s
    ; if pops the flag: non-zero runs the code up to end, zero skips past end.
    if                                       ;   if (n <= 1)
        i32.const 1
        return                               ;     return 1;
    end

    local.get $n                             ; n, kept for the multiply
    local.get $n
    i32.const 1
    i32.sub
    ; The inner call gets a frame of its own, so this frame's n stays put.
    ; With n = 5, inside factorial(4) the operand stack is, top first:
    ;
    ;   4:5:5:132
    ;   4    local 0 of factorial(4), where frameBase points
    ;   5    factorial(5)'s operand: n, waiting for the multiply
    ;   5    local 0 of factorial(5)
    ;   132  _start's operand: the output address 0x84
    i32.const factorial
    call 1, 1
    i32.mul
    ; return rolls the frame back:
    ;   - it pops the 1 result;
    ;   - it moves sp back to just above local 0, which drops all of the
    ;     frame's locals and any operands left over;
    ;   - it pops the control stack down to and including the call record,
    ;     and restores the caller's frameBase and local count from it;
    ;   - it pushes the result where the parameter was and jumps to the
    ;     return address.
    ; factorial(4) returning 24 turns 24:4:5:5:132 into 24:5:5:132.
    return                                   ;   return n * factorial(n - 1);
