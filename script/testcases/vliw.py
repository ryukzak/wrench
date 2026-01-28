from testcases.core import (
    CharSequence2Word,
    TestCase,
    Words2Words,
    TEST_CASES,
)


def fnv32_1_hash(xs):
    """Input: stream of chars forming c string style (end with 0)

    Need to calculate FNV-1 32 bit hash of input string
    More info: https://ru.wikipedia.org/wiki/FNV
    """
    it = 0
    fnv32_prime = 0x01000193
    hash_value = 0x811C9DC5
    while ord(xs[it]) > 0:
        hash_value = (hash_value * fnv32_prime) & 0xFFFFFFFF
        hash_value ^= ord(xs[it])
        it += 1

    return hash_value


TEST_CASES["fnv32_1_hash"] = TestCase(
    simple=fnv32_1_hash,
    cases=[
        CharSequence2Word("a\0", 0x050C5D7E),
        CharSequence2Word("abc\0", 0x439C2F4B),
        CharSequence2Word("Computers are awesome!\0", 0xE97BD97F),
    ],
    reference=fnv32_1_hash,
    reference_cases=[],
    is_variant=True,
    category="VLIW",
)


###########################################################


def fnv32_1a_hash(xs):
    """Input: stream of chars forming c string style (end with 0)

    Need to calculate FNV-1A 32 bit hash of input string
    More info: https://ru.wikipedia.org/wiki/FNV
    """
    it = 0
    fnv32_prime = 0x01000193
    hash_value = 0x811C9DC5
    while ord(xs[it]) > 0:
        hash_value ^= ord(xs[it])
        hash_value = (hash_value * fnv32_prime) & 0xFFFFFFFF
        it += 1

    return hash_value


TEST_CASES["fnv32_1a_hash"] = TestCase(
    simple=fnv32_1a_hash,
    cases=[
        CharSequence2Word("a\0", 0xE40C292C),
        CharSequence2Word("abc\0", 0x1A47E90B),
        CharSequence2Word("Computers are awesome!\0", 0xFCEFE74B),
    ],
    reference=fnv32_1a_hash,
    reference_cases=[],
    is_variant=True,
    category="VLIW",
)


###########################################################


def djb2_hash(xs):
    """Input: stream of chars forming c string style (end with 0)

    Need to calculate DJB2 32 bit hash of input string
    More info: https://theartincode.stanis.me/008-djb2/
    """
    it = 0
    hash_value = 5381
    while ord(xs[it]) > 0:
        hash_value = (hash_value * 33 + ord(xs[it])) & 0xFFFFFFFF
        it += 1

    return hash_value


TEST_CASES["djb2_hash"] = TestCase(
    simple=djb2_hash,
    cases=[
        CharSequence2Word("\0", 0x00001505),
        CharSequence2Word("a\0", 0x0002B606),
        CharSequence2Word("abc\0", 0x0B885C8B),
        CharSequence2Word("Computers are awesome!\0", 0x86D49D71),
    ],
    reference=djb2_hash,
    reference_cases=[],
    is_variant=True,
    category="VLIW",
)


###########################################################


def determinant_3x3(*xs):
    """Input: 3x3 matrix in format a_10, a_20, a_30, a_11, ...

    Need to calculate determinant of this matrix
    """
    result = (
        xs[0] * xs[4] * xs[8]
        + xs[1] * xs[5] * xs[6]
        + xs[2] * xs[3] * xs[7]
        - xs[0] * xs[5] * xs[7]
        - xs[1] * xs[3] * xs[8]
        - xs[2] * xs[4] * xs[6]
    )

    if result > 0xFFFFFFFF:
        return [0xCCCCCCCC]

    return [result]


TEST_CASES["determinant_3x3"] = TestCase(
    simple=determinant_3x3,
    cases=[
        Words2Words([0, 0, 0, 0, 0, 0, 0, 0, 0], [0]),
        Words2Words([1, 2, 3, 4, 5, 6, 7, 8, 9], [0]),
        Words2Words([0, 0, 1, 0, 1, 0, 1, 0, 0], [-1]),
        Words2Words([7, -5, 4, 32, 8, 3, 5, 2, 8], [1707]),
    ],
    reference=determinant_3x3,
    reference_cases=[],
    is_variant=True,
    category="VLIW",
)
