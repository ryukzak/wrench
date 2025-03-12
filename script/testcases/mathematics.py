from testcases.core import (
    TestCase,
    Word2Word,
    Words2Words,
    limit_to_int32,
    overflow_error_value,
    TEST_CASES,
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
        Word2Word(12343, 1),
        Word2Word(123423, 0),
        Word2Word(2_147_483_647, 1, limit=510000),
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
        Words2Words([2, 48, 18, 12], [0, 66]),
        Words2Words([2, 0x7FFF_FFFF, 1, 0], [0, 0x8000_0000]),
        Words2Words([3, 0x7FFF_FFFF, 1, 0x7FFF_FFFF, 0], [0, 0xFFFF_FFFF]),
        Words2Words([4, 0x7FFF_FFFF, 1, 0x7FFF_FFFF, 1, 0], [1, 0]),
        Words2Words([4, 0x7FFF_FFFF, 1, 0x7FFF_FFFF, 2, 0], [1, 1]),
        Words2Words([2, 1, -1], [0, 0]),
    ],
    reference=sum_word_pstream,
    reference_cases=[],
    is_variant=True,
    category="Mathematics",
)
