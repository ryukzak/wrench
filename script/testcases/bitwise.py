from testcases.core import (
    TestCase,
    Word2Word,
    max_int32,
    TEST_CASES,
)


def count_ones(n):
    """Count the number of ones in the binary representation of a number"""
    count = 0
    while n > 0:
        count += n & 1
        n >>= 1
    return count


def count_ones_ref(n):
    return {-1: 32, -2: 31}.get(n, count_ones(n))


TEST_CASES["count_ones"] = TestCase(
    simple=count_ones,
    cases=[
        Word2Word(5, 2),
        Word2Word(7, 3),
        Word2Word(247923789, 13),
        Word2Word(max_int32, 31),
    ],
    reference=count_ones_ref,
    reference_cases=[
        Word2Word(0, 0),
        Word2Word(-1, 32),
        Word2Word(-2, 31),
    ],
    is_variant=True,
    category="Bitwise Operations",
)


###########################################################


def count_zero(n):
    """Count the number of zeros in the binary representation of a number"""
    count = 0
    for _ in range(32):
        count += 0 if n & 1 else 1
        n >>= 1
    return count


count_zero_ref = count_zero

TEST_CASES["count_zero"] = TestCase(
    simple=count_zero,
    cases=[
        Word2Word(5, 30),
        Word2Word(7, 29),
        Word2Word(247923789, 19),
    ],
    reference=count_zero_ref,
    reference_cases=[
        Word2Word(0, 32),
        Word2Word(-1, 0),
        Word2Word(-2, 1),
        Word2Word(-2342, 5),
    ],
    is_variant=True,
    category="Bitwise Operations",
)


###########################################################


def reverse_bits(n):
    """Reverse the bits of a number"""
    result = 0
    inv = n & 0x01
    for _ in range(32):
        result <<= 1
        result |= n & 1
        n >>= 1
    if inv == 1:
        result = -result
    return result


def reverse_bits_ref(n):
    if n == -1:
        return -1
    return reverse_bits(n)


TEST_CASES["reverse_bits"] = TestCase(
    simple=reverse_bits,
    cases=[
        Word2Word(1, -2147483648),
        Word2Word(2, 1073741824),
    ],
    reference=reverse_bits_ref,
    reference_cases=[
        Word2Word(-1, -1),
        Word2Word(0, 0),
        Word2Word(0x0000CC00, 0x00330000),
    ],
    is_variant=True,
    category="Bitwise Operations",
)

###########################################################


def little_to_big_endian(n):
    """Convert a 32-bit integer from little-endian to big-endian format"""
    return int.from_bytes(n.to_bytes(4, byteorder="little"), byteorder="big")


TEST_CASES["little_to_big_endian"] = TestCase(
    simple=little_to_big_endian,
    cases=[
        Word2Word(0x12345678, 0x78563412),
        Word2Word(0xAABBCCDD, 0xDDCCBBAA),
    ],
    reference=little_to_big_endian,
    reference_cases=[],
    is_variant=True,
    category="Bitwise Operations",
)

###########################################################


def big_to_little_endian(n):
    """Convert a 32-bit integer from big-endian to little-endian format"""
    return int.from_bytes(n.to_bytes(4, byteorder="big"), byteorder="little")


big_to_little_endian_ref = big_to_little_endian

TEST_CASES["big_to_little_endian"] = TestCase(
    simple=big_to_little_endian,
    cases=[
        Word2Word(0x78563412, 0x12345678),
        Word2Word(0xDDCCBBAA, 0xAABBCCDD),
    ],
    reference=big_to_little_endian,
    reference_cases=[],
    is_variant=True,
    category="Bitwise Operations",
)


###########################################################
def count_leading_zeros(n):
    """Count the number of leading zeros in the binary representation of an integer.

    Args:
        n (int): The integer to count leading zeros for.

    Returns:
        int: The number of leading zeros.
    """
    if n == 0:
        return 32
    count = 0
    for i in range(31, -1, -1):
        if (n >> i) & 1 == 0:
            count += 1
        else:
            break
    return count


count_leading_zeros_ref = count_leading_zeros

TEST_CASES["count_leading_zeros"] = TestCase(
    simple=count_leading_zeros,
    cases=[
        Word2Word(1, 31),
        Word2Word(2, 30),
        Word2Word(16, 27),
    ],
    reference=count_leading_zeros_ref,
    reference_cases=[
        Word2Word(0, 32),
        Word2Word(0x00F12345, 8),
        Word2Word(-1, 0),
        Word2Word(-4345241, 0),
    ],
    is_variant=True,
    category="Bitwise Operations",
)

###########################################################


def count_trailing_zeros(n):
    """Count the number of trailing zeros in the binary representation of an integer.

    Args:
        n (int): The integer to count trailing zeros for.

    Returns:
        int: The number of trailing zeros.
    """
    if n == 0:
        return 32
    count = 0
    while (n & 1) == 0:
        count += 1
        n >>= 1
    return count


count_trailing_zeros_ref = count_trailing_zeros

TEST_CASES["count_trailing_zeros"] = TestCase(
    simple=count_trailing_zeros,
    cases=[
        Word2Word(1, 0),
        Word2Word(2, 1),
        Word2Word(16, 4),
    ],
    reference=count_trailing_zeros_ref,
    reference_cases=[
        Word2Word(-1, 0),
        Word2Word(-2, 1),
        Word2Word(0x01010000, 16),
    ],
    is_variant=True,
    category="Bitwise Operations",
)

###########################################################


def is_binary_palindrome(n):
    """Check if the 32-bit binary representation of a number is a palindrome.

    Args:
        n (int): The integer to check.

    Returns:
        int: 1 if the binary representation is a palindrome, otherwise 0.
    """
    binary_str = f"{n:032b}"  # Convert to 32-bit binary string
    res = binary_str == binary_str[::-1]
    return 1 if res else 0


def is_binary_palindrome_ref(n):
    if n == -1:
        return 1
    return is_binary_palindrome(n)


TEST_CASES["is_binary_palindrome"] = TestCase(
    simple=is_binary_palindrome,
    cases=[
        Word2Word(0x00000005, 0),
        Word2Word(0x0000000F, 0),
        Word2Word(0xF000000F, 1),
        Word2Word(0xC0000002, 0),
    ],
    reference=is_binary_palindrome_ref,
    reference_cases=[
        Word2Word(0x0F0F0F0F, 0),
        Word2Word(-1, 1),
        Word2Word(-2, 0),
    ],
    is_variant=True,
    category="Bitwise Operations",
)
