from testcases.core import (
    TEST_CASES,
    TestCase,
    Word2Word,
    Words2Words,
    limit_to_int32,
    max_int32,
    min_int32,
    overflow_error_value,
)


def fibonacci(n):
    """Calculate the n-th Fibonacci number (positive only)"""
    if n == 0:
        return 0
    elif n == 1:
        return 1
    elif n < 0:
        return -1
    a, b = 0, 1
    for _ in range(2, n + 1):
        a, b = b, a + b
    return b


fibonacci_ref = limit_to_int32(fibonacci)

TEST_CASES["fibonacci"] = TestCase(
    simple=fibonacci,
    cases=[
        Word2Word(0, 0),
        Word2Word(1, 1),
        Word2Word(2, 1),
        Word2Word(3, 2),
        Word2Word(4, 3),
        Word2Word(5, 5),
        Word2Word(25, 75025),
    ],
    reference=fibonacci_ref,
    reference_cases=[
        Word2Word(-1, -1),
        Word2Word(-2, -1),
        Word2Word(47, overflow_error_value),
    ],
    is_variant=True,
    category="Mathematics",
)

###########################################################


def sum_n(n):
    """Calculate the sum of numbers from 1 to n"""
    if n <= 0:
        return -1
    total = 0
    for i in range(1, n + 1):
        total += i
    return total


sum_n_ref = limit_to_int32(sum_n)

TEST_CASES["sum_n"] = TestCase(
    simple=sum_n,
    cases=[
        Word2Word(5, 15),
        Word2Word(10, 55),
    ],
    reference=sum_n_ref,
    reference_cases=[
        Word2Word(0, -1),
        Word2Word(-1, -1),
        Word2Word(-2, -1),
        Word2Word(4170, 8696535),
        Word2Word(68000, overflow_error_value),
    ],
    is_variant=True,
    category="Mathematics",
)

###########################################################


def sum_even_n(n):
    """Calculate the sum of even numbers from 1 to n"""
    if n <= 0:
        return -1
    total = 0
    for i in range(1, n + 1):
        if i % 2 == 0:
            total += i
    return total


sum_even_n_ref = limit_to_int32(sum_even_n)

TEST_CASES["sum_even_n"] = TestCase(
    simple=sum_even_n,
    cases=[
        Word2Word(5, 6),
        Word2Word(10, 30),
        Word2Word(90000, 2025045000),
    ],
    reference=sum_even_n_ref,
    reference_cases=[
        Word2Word(0, -1),
        Word2Word(-1, -1),
        Word2Word(-2, -1),
        Word2Word(100000, overflow_error_value),
    ],
    is_variant=True,
    category="Mathematics",
)
###########################################################


def sum_odd_n(n):
    """Calculate the sum of odd numbers from 1 to n"""
    if n <= 0:
        return -1
    total = 0
    for i in range(1, n + 1):
        if i % 2 != 0:
            total += i
    return total


sum_odd_n_ref = limit_to_int32(sum_odd_n)

TEST_CASES["sum_odd_n"] = TestCase(
    simple=sum_odd_n,
    cases=[
        Word2Word(5, 9),
        Word2Word(10, 25),
        Word2Word(90000, 2025000000),
    ],
    reference=sum_odd_n_ref,
    reference_cases=[
        Word2Word(0, -1),
        Word2Word(-1, -1),
        Word2Word(-2, -1),
        Word2Word(100000, overflow_error_value),
    ],
    is_variant=True,
    category="Mathematics",
)

###########################################################


def sum_of_digits(n):
    """Calculate the sum of the digits of a number"""
    total = 0
    n = abs(n)
    while n > 0:
        total += n % 10
        n //= 10
    return total


sum_of_digits_ref = sum_of_digits

TEST_CASES["sum_of_digits"] = TestCase(
    simple=sum_of_digits,
    cases=[
        Word2Word(123, 6),
        Word2Word(-456, 15),
    ],
    reference=sum_of_digits_ref,
    reference_cases=[
        Word2Word(0, 0),
        Word2Word(1, 1),
        Word2Word(-23, 5),
        Word2Word(1_999_999_999, 82),
    ],
    is_variant=True,
    category="Mathematics",
)

###########################################################


def is_prime(n):
    """Check if a natural number is prime"""
    if n < 1:
        return -1
    if n == 1:
        return 0
    for i in range(2, int(n**0.5) + 1):
        if n % i == 0:
            return 0
    return 1


is_prime_ref = is_prime

TEST_CASES["is_prime"] = TestCase(
    simple=is_prime,
    cases=[
        Word2Word(2, 1),
        Word2Word(5, 1),
        Word2Word(4, 0),
        Word2Word(7, 1),
        Word2Word(8, 0),
        Word2Word(283, 1),
        Word2Word(284, 0),
        Word2Word(293, 1),
    ],
    reference=is_prime_ref,
    reference_cases=[
        Word2Word(0, -1),
        Word2Word(1, 0),
        Word2Word(-12, -1),
        Word2Word(12343, 1, limit=18_000),
        Word2Word(123423, 0),
        Word2Word(2_147_483_647, 1, limit=7_600_000),
    ],
    is_variant=True,
    category="Mathematics",
)


###########################################################
def count_divisors(n):
    """Count the number of divisors of a natural number"""
    if n < 1:
        return -1
    count = 0
    for i in range(1, n + 1):
        if n % i == 0:
            count += 1
    return count


count_divisors_ref = count_divisors

TEST_CASES["count_divisors"] = TestCase(
    simple=count_divisors,
    cases=[
        Word2Word(2, 2),
        Word2Word(4, 3),
        Word2Word(6, 4),
        Word2Word(10, 4),
    ],
    reference=count_divisors_ref,
    reference_cases=[
        Word2Word(-12, -1),
        Word2Word(0, -1),
        Word2Word(1, 1),
    ],
    is_variant=True,
    category="Mathematics",
)

###########################################################


def gcd(a, b):
    """Find the greatest common divisor (GCD)"""
    while b != 0:
        a, b = b, a % b
    return [abs(a)]


gcd_ref = gcd

TEST_CASES["gcd"] = TestCase(
    simple=gcd,
    cases=[
        Words2Words([48, 18], [6]),
        Words2Words([56, 98], [14]),
    ],
    reference=gcd_ref,
    reference_cases=[
        # What about negative value?
        #        Words2Words([-1, 18], [-1]),
        #        Words2Words([48, -1], [-1]),
        #        Words2Words([48, 0], [-1]),
        #        Words2Words([0, 18], [-1]),
    ],
    is_variant=True,
    category="Mathematics",
)

###########################################################


def sum_word_cstream(*xs):
    """Input: stream of word (32 bit) in c string style (end with 0).

    Need to sum all numbers and send result in two words (64 bits).
    """
    tmp = 0
    x = 0
    for x in xs:
        if x == 0:
            break
        tmp += x
    assert x == 0
    hw, lw = ((tmp & 0xFFFF_FFFF_0000_0000) >> 32), tmp & 0x0000_0000_FFFF_FFFF
    return [hw, lw]


TEST_CASES["sum_word_cstream"] = TestCase(
    simple=sum_word_cstream,
    cases=[
        Words2Words([48, 18, 0], [0, 66]),
        Words2Words([1, 0], [0, 1]),
        Words2Words([48, 18, 0, 12, 0], [0, 66], rest=[12, 0]),
        Words2Words([1, 0], [0, 1]),
        Words2Words([0x7FFF_FFFF, 1, 0], [0, 0x8000_0000]),
        Words2Words([0x7FFF_FFFF, 1, 0x7FFF_FFFF, 0], [0, 0xFFFF_FFFF]),
        Words2Words([0x7FFF_FFFF, 1, 0x7FFF_FFFF, 1, 0], [1, 0]),
        Words2Words([0x7FFF_FFFF, 1, 0x7FFF_FFFF, 2, 0], [1, 1]),
    ],
    reference=sum_word_cstream,
    reference_cases=[
        Words2Words([1, -1, 0], [0, 0]),
    ],
    is_variant=True,
    category="Mathematics",
)

###########################################################


def sum_word_pstream(n, *xs):
    """Input: stream of word (32 bit) in pascal string style (how many words,
    after that the words itself).

    Need to sum all numbers and send result in two words (64 bits).
    """
    tmp = 0
    for i in range(n):
        tmp += xs[i]
    hw, lw = ((tmp & 0xFFFF_FFFF_0000_0000) >> 32), tmp & 0x0000_0000_FFFF_FFFF
    return [hw, lw]


TEST_CASES["sum_word_pstream"] = TestCase(
    simple=sum_word_pstream,
    cases=[
        Words2Words([2, 48, 18], [0, 66]),
        Words2Words([1, 1], [0, 1]),
        Words2Words([2, 48, 18, 0, 12], [0, 66], rest=[0, 12]),
        Words2Words([2, 48, 18, 12], [0, 66], rest=[12]),
        Words2Words([2, 0x7FFF_FFFF, 1, 0], [0, 0x8000_0000], rest=[0]),
        Words2Words([3, 0x7FFF_FFFF, 1, 0x7FFF_FFFF, 0], [0, 0xFFFF_FFFF], rest=[0]),
        Words2Words([4, 0x7FFF_FFFF, 1, 0x7FFF_FFFF, 1, 0], [1, 0], rest=[0]),
        Words2Words([4, 0x7FFF_FFFF, 1, 0x7FFF_FFFF, 2, 0], [1, 1], rest=[0]),
        Words2Words([2, 1, -1], [0, 0]),
    ],
    reference=sum_word_pstream,
    reference_cases=[],
    is_variant=True,
    category="Mathematics",
)


###########################################################


def power(base, exp):
    """Compute base raised to the power of a non-negative exponent.

    - exp < 0: return -1
    - Overflow (result outside int32 range): return 0xCCCCCCCC

    Args:
        base (int): The base value.
        exp (int): The non-negative exponent.

    Returns:
        list: A one-element list with the result.
    """
    if exp < 0:
        return [-1]
    result = 1
    for _ in range(exp):
        result *= base
        if result > max_int32 or result < min_int32:
            return [overflow_error_value]
    return [result]


power_ref = power

TEST_CASES["power"] = TestCase(
    simple=power,
    cases=[
        Words2Words([2, 10], [1024]),
        Words2Words([3, 5], [243]),
        Words2Words([5, 0], [1]),
        Words2Words([0, 5], [0]),
    ],
    reference=power_ref,
    reference_cases=[
        Words2Words([2, -1], [-1]),
        Words2Words([1, 100], [1]),
        Words2Words([10, 9], [1000000000]),
        Words2Words([10, 10], [overflow_error_value]),
        Words2Words([2, 30], [1073741824]),
        Words2Words([2, 31], [overflow_error_value]),
        Words2Words([-2, 31], [-2147483648]),
    ],
    is_variant=True,
    category="Mathematics",
)


###########################################################


def collatz_length(n):
    """Count the number of steps to reach 1 in the Collatz sequence.

    Starting from n, apply:
    - n even: n = n // 2
    - n odd: n = 3 * n + 1
    Repeat until n == 1; return the number of steps.

    Note: intermediate values may temporarily exceed 32 bits for some inputs.

    - n <= 0: return -1
    - n == 1: return 0

    Args:
        n (int): The starting value.

    Returns:
        int: The number of steps to reach 1, or -1 for invalid input.
    """
    if n <= 0:
        return -1
    steps = 0
    while n != 1:
        if n % 2 == 0:
            n //= 2
        else:
            n = 3 * n + 1
        steps += 1
    return steps


collatz_length_ref = collatz_length

TEST_CASES["collatz_length"] = TestCase(
    simple=collatz_length,
    cases=[
        Word2Word(1, 0),
        Word2Word(2, 1),
        Word2Word(6, 8),
        Word2Word(10, 6),
    ],
    reference=collatz_length_ref,
    reference_cases=[
        Word2Word(-1, -1),
        Word2Word(0, -1),
        Word2Word(3, 7),
        Word2Word(4, 2),
        Word2Word(5, 5),
        Word2Word(27, 111, limit=3000),
    ],
    is_variant=True,
    category="Mathematics",
)


###########################################################


def _gcd_helper(a, b):
    while b:
        a, b = b, a % b
    return a


def lcm(a, b):
    """Compute the least common multiple (LCM) of two positive integers.

    - a <= 0 or b <= 0: return -1
    - Overflow: return 0xCCCCCCCC

    Args:
        a (int): First positive integer.
        b (int): Second positive integer.

    Returns:
        list: A one-element list with the LCM.
    """
    if a <= 0 or b <= 0:
        return [-1]
    g = _gcd_helper(a, b)
    result = (a // g) * b
    if result > max_int32:
        return [overflow_error_value]
    return [result]


lcm_ref = lcm

TEST_CASES["lcm"] = TestCase(
    simple=lcm,
    cases=[
        Words2Words([4, 6], [12]),
        Words2Words([12, 18], [36]),
        Words2Words([7, 5], [35]),
        Words2Words([1, 100], [100]),
    ],
    reference=lcm_ref,
    reference_cases=[
        Words2Words([0, 5], [-1]),
        Words2Words([-1, 5], [-1]),
        Words2Words([48, 0], [-1]),
        Words2Words([6, 4], [12]),
        Words2Words([2147483647, 1], [2147483647]),
        Words2Words([100000, 100001], [overflow_error_value]),
    ],
    is_variant=True,
    category="Mathematics",
)


###########################################################


def integer_sqrt(n):
    """Compute the integer square root (floor of sqrt(n)).

    - n < 0: return -1
    - n == 0: return 0

    Args:
        n (int): The non-negative integer.

    Returns:
        int: floor(sqrt(n)), or -1 for negative input.
    """
    if n < 0:
        return -1
    x = int(n**0.5)
    while x * x > n:
        x -= 1
    while (x + 1) * (x + 1) <= n:
        x += 1
    return x


integer_sqrt_ref = integer_sqrt

TEST_CASES["integer_sqrt"] = TestCase(
    simple=integer_sqrt,
    cases=[
        Word2Word(0, 0),
        Word2Word(1, 1),
        Word2Word(4, 2),
        Word2Word(9, 3),
        Word2Word(16, 4),
        Word2Word(25, 5),
    ],
    reference=integer_sqrt_ref,
    reference_cases=[
        Word2Word(-1, -1),
        Word2Word(2, 1),
        Word2Word(15, 3),
        Word2Word(17, 4),
        Word2Word(2147483647, 46340),
    ],
    is_variant=True,
    category="Mathematics",
)
