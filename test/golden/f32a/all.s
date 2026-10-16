    \ Every f32a instruction, once, in a program that runs to halt.
    \ Stack contents are noted after each line as  bottom ... top.

    .data

ptr_a:           .word  0x10               \ address of `scratch`,  for a!
ptr_b:           .word  0x14               \ address of `scratch2`, for b!
one:             .word  0x1
two:             .word  0x2
scratch:         .word  0x0                \ must stay at 0x10, see ptr_a
scratch2:        .word  0x0                \ must stay at 0x14, see ptr_b

    .text

_start:
    \ --- literals, bare and explicit ------------------------------
    1                        \ 1
    lit 2                    \ 1 2

    \ --- arithmetic and bitwise -----------------------------------
    +                        \ 3
    2*                       \ 6
    2/                       \ 3
    inv                      \ -4
    inv                      \ 3
    lit 1                    \ 3 1
    xor                      \ 2
    lit 3                    \ 2 3
    and                      \ 2

    \ --- stack manipulation ---------------------------------------
    dup                      \ 2 2
    over                     \ 2 2 2
    drop                     \ 2 2
    drop                     \ 2

    \ --- extended arithmetic mode ---------------------------------
    lit 1                    \ 2 1
    eam                      \ 2          eam <- 1
    lit 0                    \ 2 0
    eam                      \ 2          eam <- 0

    \ --- A and B, and every memory form ---------------------------
    @p ptr_a                 \ 2 0x10     mem[ptr_a]
    a!                       \ 2          A <- scratch
    @p ptr_b                 \ 2 0x14     mem[ptr_b]
    b!                       \ 2          B <- scratch2
    a                        \ 2 0x10     push A
    drop                     \ 2
    dup                      \ 2 2
    !                        \ 2          mem[A] <- 2
    @                        \ 2 2        mem[A]
    !b                       \ 2          mem[B] <- 2
    @b                       \ 2 2        mem[B]
    !+                       \ 2          mem[A] <- 2, A <- A + 1
    @+                       \ 2 ?        mem[A], A <- A + 1
    drop                     \ 2
    !p scratch2              \ ()         mem[scratch2] <- 2

    \ --- multiply and divide steps --------------------------------
    @p one                   \ 1
    a!                       \ ()         A <- 1, the multiplier
    lit 2                    \ 2
    lit 3                    \ 2 3
    +*                       \ ...        one multiply step
    drop                     \ ...
    drop                     \ ()
    @p ptr_b                 \ 0x14
    b!                       \ ()         B <- scratch2, the divisor
    @p two                   \ 2
    a!                       \ ()         A <- 2, the dividend
    lit 0                    \ 0
    lit 0                    \ 0 0
    +/                       \ ...        one divide step
    drop                     \ ...
    drop                     \ ()

    \ --- return stack ---------------------------------------------
    lit 5                    \ 5
    >r                       \ ()         R <- 5
    r>                       \ 5          R -> T
    drop                     \ ()

    \ --- next: two iterations, then it pops R and falls through ---
    lit 2                    \ 2
    >r                       \ ()         R <- 2
loop:
    next loop

    \ --- branches, each targeting the line after it ---------------
    lit 0                    \ 0
    if at_minus_if           \ taken, T == 0
at_minus_if:
    lit 1                    \ 1
    -if at_call              \ taken, T >= 0

    \ --- call, return, jump ---------------------------------------
at_call:
    subr                     \ call
    done ;                   \ jump

subr:
    ;                        \ return

done:
    halt
