;        .section .text
;        .globl  fib

.section .data           ; 1
        myVar1 db 0x55            ; Define a byte with initial value 0x55
        myVar2 dw 1000            ; Define a word (2 bytes) with initial value 1000
        ;; myArray db 10, 20, 30, 40 ; Define an array of bytes

.section .data
        myVar1 db 0x55            ; Define a byte with initial value 0x55


.section .text

; Fibonacci function: calculates the nth Fibonacci number.
; Input: a1 (n)
; Output: a0 (nth Fibonacci number)
fib:
        ; Initialize registers:
        ; t0 = current Fibonacci number (initially F1)
        ; t1 = previous Fibonacci number (initially F0)
        ; t2 = counter
        li      t0, 1                ; t0 is initially set to Fibonacci(1)
        li      t1, 0                ; t1 is initially set to Fibonacci(0)
        li      t2, 1                ; t2 is our counter, starts at 1

        ; Check if the input number is less than or equal to 1
        ble     a1, zero, finish     ; if n <= 0, return Fibonacci(0)
        beq     a1, t2, finish      ; if n == 1, return Fibonacci(1)

loop:
        ; Calculate the next Fibonacci number
        add     t3, t0, t1          ; t3 = t0 + t1 (current + previous)
        mv      t1, t0              ; Move current Fibonacci to previous
        mv      t0, t3              ; New current Fibonacci

        ; Increment counter
        addi    t2, t2, 1           ; t2 = t2 + 1

        ; Loop until counter matches input number
        bne     t2, a1, loop        ; if t2 != n, repeat the loop

finish:
        ; Store result in a0 and return
        mv      a0, t0              ; Move result to a0 (return register)
        ret                         ; Return from function
