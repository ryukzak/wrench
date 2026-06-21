    .text

_start:
    .func    locals $result
        i32.const 0x84
        i32.const 0x80
        i32.load
        call     factorial
        i32.store
        halt
    .endfunc

factorial:
    .func    params $n result i32 locals $acc

        local.get $n
        i32.const 0
        i32.lt_s
        if       negative
            i32.const -1
            return
        end

        local.get $n
        i32.const 12
        i32.gt_s
        if       overflow
            i32.const -858993460
            return
        end

        i32.const 1
        local.set $acc

        block    done
            loop     again
                local.get $n
                i32.const 1
                i32.le_s
                br_if    done

                local.get $acc
                local.get $n
                i32.mul
                local.set $acc

                local.get $n
                i32.const 1
                i32.sub
                local.set $n

                br       again
            end
        end

        local.get $acc
    .endfunc
