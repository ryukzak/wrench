#!/usr/bin/env python3
n = 31


def divide_unsigned(dividend, divisor):
    """
    quotient, remainder = dividend/divisor
    """
    if divisor == 0:
        raise ValueError("Division by zero error")

    quotient, remainder = 0, 0

    def div_step():
        nonlocal dividend, quotient, remainder

        remainder = remainder << 1
        dividen_upper_bit = 1 if dividend & (1 << n) else 0
        dividend = dividend << 1
        remainder = remainder | dividen_upper_bit

        quotient = quotient << 1

        if remainder >= divisor:
            remainder = remainder - divisor
            quotient = quotient | 1

        return quotient, remainder

    for _ in range(n + 1):
        quotient, remainder = div_step()

    return quotient, remainder


def divide_signed(dividend, divisor):
    if divisor == 0:
        raise ValueError("Division by zero error")

    # Determine the sign of the result
    sign = -1 if (dividend < 0) ^ (divisor < 0) else 1

    # Work with absolute values for the division process
    abs_dividend = abs(dividend)
    abs_divisor = abs(divisor)

    # Perform unsigned division on absolute values
    quotient, remainder = divide_unsigned(abs_dividend, abs_divisor)

    # Apply the sign to the quotient
    quotient *= sign

    # The remainder takes the sign of the original dividend
    remainder = remainder if dividend >= 0 else -remainder

    return quotient, remainder


dividend = 27
divisor = 4

quotient, remainder = divide_unsigned(dividend, divisor)
print(
    f"Unsigned Division: {dividend} / {divisor} = {quotient}, remainder = {remainder}"
)

assert divide_unsigned(27, 4) == (6, 3)
assert divide_unsigned(3, 2) == (1, 1)
assert divide_unsigned(9, 3) == (3, 0)
assert divide_unsigned(2, 3) == (0, 2)

dividend = -27
divisor = 4

quotient, remainder = divide_signed(dividend, divisor)
print(f"Signed division: {dividend} / {divisor} = {quotient}, remainder = {remainder}")
