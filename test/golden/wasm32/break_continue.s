    .data

counter:         .word  0

    .text

_start:
    ; Count down from 5, leaving a marker (the old counter value) for each
    ; value actually processed. `continue` at 3 (skip the marker, keep
    ; looping); `break` at 2 (stop for good, before 2 or 1 are processed).
    ;
    ; The counter lives in `.data` rather than a local -- `_start` is
    ; never called, so it has no parameters, and a function's only
    ; locals are its parameters -- see loop.s's comment for why an
    ; instruction inside the loop body can't reach a value that was
    ; pushed before `loop`/`block` either way. Markers are still pushed
    ; straight onto the operand stack, since (like sum.s) they're only
    ; ever pushed *after* the surrounding records already exist, so they
    ; just pile up above them, never needing to reach through.
    ;
    ; `block` is what makes `break` clean: it's a valid forward branch
    ; target (jump past its own `end`), unlike `loop` which only ever
    ; gives you a backward one. From inside the loop, depth 0 is the loop
    ; itself (continue), depth 1 is the enclosing block (break) -- no need
    ; to smuggle "should I stop" into the loop's own continue condition
    ; anymore.
    i32.const counter
    i32.const 5
    i32.store
    block
        loop
            ; --- break check: counter == 2 ---
            i32.const counter
            i32.load
            i32.const 2
            i32.eq
            br_if    1                               ; break: branch out to the block
            ; --- continue check: counter == 3 ---
            i32.const counter
            i32.load
            i32.const 3
            i32.eq
            if
                ; continue: no marker, just decrement
                i32.const counter
                i32.const counter
                i32.load
                i32.const 1
                i32.sub
                i32.store
            else
                ; normal: push a marker (current counter), then decrement
                i32.const counter
                i32.load
                i32.const counter
                i32.const counter
                i32.load
                i32.const 1
                i32.sub
                i32.store
            end
            i32.const counter
            i32.load
            i32.const 0
            i32.gt_s
            br_if    0                               ; keep looping while counter > 0
        end
    end
    halt
