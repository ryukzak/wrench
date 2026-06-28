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


###########################################################


def linear_filter(*xs):
    """
    Input: first word N (length of array), then N values of X.
    Output: N values of Y where Y[i] = 3*X[i] + 2*X[i-1] + X[i-2]
    with X[-1] = X[-2] = 0
    (so Y[0] = 3*X[0], Y[1] = 3*X[1] + 2*X[0]).
    """
    n = xs[0]
    x = list(xs[1 : n + 1])

    result = []
    for i in range(n):
        x_i = x[i]
        x_i1 = x[i - 1] if i >= 1 else 0
        x_i2 = x[i - 2] if i >= 2 else 0
        y_i = 3 * x_i + 2 * x_i1 + x_i2
        result.append(y_i)

    return result


TEST_CASES["linear_filter"] = TestCase(
    simple=linear_filter,
    cases=[
        Words2Words([0], []),
        Words2Words([1, 5], [15]),
        Words2Words([2, 5, 10], [15, 40]),
        Words2Words([3, 1, 2, 3], [3, 8, 14]),
        Words2Words([5, 1, 2, 3, 4, 5], [3, 8, 14, 20, 26]),
    ],
    reference=linear_filter,
    reference_cases=[
        Words2Words([3, 10, 20, 30], [30, 80, 140]),
        Words2Words([4, 100, 0, 100, 0], [300, 200, 400, 200]),
        Words2Words([6, 1, 1, 1, 1, 1, 1], [3, 5, 6, 6, 6, 6]),
        Words2Words([2, -5, 10], [-15, 20]),
    ],
    is_variant=True,
    category="VLIW",
)


###########################################################


def sdbm_hash(xs):
    """Input: stream of chars forming c string style (end with 0)

    Need to calculate SDBM 32 bit hash of input string.
    """
    it = 0
    hash_value = 0
    while ord(xs[it]) > 0:
        c = ord(xs[it])
        hash_value = (
            c + (hash_value << 6) + (hash_value << 16) - hash_value
        ) & 0xFFFFFFFF
        it += 1

    return hash_value


TEST_CASES["sdbm_hash"] = TestCase(
    simple=sdbm_hash,
    cases=[
        CharSequence2Word("\0", 0x00000000),
        CharSequence2Word("a\0", 0x00000061),
        CharSequence2Word("abc\0", 0x3025F862),
        CharSequence2Word("Computers are awesome!\0", 0x04B79E52),
    ],
    reference=sdbm_hash,
    reference_cases=[],
    is_variant=True,
    category="VLIW",
)


###########################################################


def affine2d_transform(*xs):
    """Input: first word N, then N pairs: x, y.

    Output for every pair: u = 3*x + 2*y + 5, v = -x + 4*y - 7.
    """
    n = xs[0]
    if n < 0:
        return [-1]

    result = []
    for i in range(n):
        x = xs[1 + 2 * i]
        y = xs[2 + 2 * i]
        u = 3 * x + 2 * y + 5
        v = -x + 4 * y - 7
        if (
            u < -0x80000000
            or u > 0x7FFFFFFF
            or v < -0x80000000
            or v > 0x7FFFFFFF
        ):
            return [0xCCCCCCCC]
        result.extend([u, v])

    return result


TEST_CASES["affine2d_transform"] = TestCase(
    simple=affine2d_transform,
    cases=[
        Words2Words([0], []),
        Words2Words([1, 1, 2], [12, 0]),
        Words2Words([2, 0, 0, 3, -1], [5, -7, 12, -14]),
        Words2Words([3, -2, 5, 10, 0, -1, -1], [9, 15, 35, -17, 0, -10]),
    ],
    reference=affine2d_transform,
    reference_cases=[
        Words2Words([-1], [-1]),
        Words2Words([1, 1000000000, 1000000000], [0xCCCCCCCC]),
    ],
    is_variant=True,
    category="VLIW",
)


###########################################################


def sum_and_sum_squares(*xs):
    """Input: first word N, then N values.

    Output: two words: sum(X) and sum(x*x for x in X).
    """
    n = xs[0]
    if n < 0:
        return [-1]

    total = 0
    square_total = 0
    for i in range(n):
        x = xs[1 + i]
        total += x
        square_total += x * x

    if (
        total < -0x80000000
        or total > 0x7FFFFFFF
        or square_total < -0x80000000
        or square_total > 0x7FFFFFFF
    ):
        return [0xCCCCCCCC]

    return [total, square_total]


TEST_CASES["sum_and_sum_squares"] = TestCase(
    simple=sum_and_sum_squares,
    cases=[
        Words2Words([0], [0, 0]),
        Words2Words([3, 1, 2, 3], [6, 14]),
        Words2Words([4, -2, 5, 0, -3], [0, 38]),
        Words2Words([5, 10, 20, 30, 40, 50], [150, 5500]),
    ],
    reference=sum_and_sum_squares,
    reference_cases=[
        Words2Words([-1], [-1]),
        Words2Words([2, 50000, 50000], [0xCCCCCCCC]),
    ],
    is_variant=True,
    category="VLIW",
)


###########################################################


def determinant_2x2_stream(*xs):
    """Input: first word N, then N matrices: a, b, c, d.

    Output: N values of determinant where det = a*d - b*c.
    """
    n = xs[0]
    if n < 0:
        return [-1]

    result = []
    for i in range(n):
        base = 1 + 4 * i
        a, b, c, d = xs[base : base + 4]
        det = a * d - b * c
        if det < -0x80000000 or det > 0x7FFFFFFF:
            return [0xCCCCCCCC]
        result.append(det)

    return result


TEST_CASES["determinant_2x2_stream"] = TestCase(
    simple=determinant_2x2_stream,
    cases=[
        Words2Words([0], []),
        Words2Words([1, 1, 2, 3, 4], [-2]),
        Words2Words([2, 1, 0, 0, 1, 2, 3, 5, 7], [1, -1]),
        Words2Words([3, 0, 0, 0, 0, -1, 2, 3, -4, 7, -5, 4, 8], [0, -2, 76]),
    ],
    reference=determinant_2x2_stream,
    reference_cases=[
        Words2Words([-1], [-1]),
        Words2Words([1, 50000, 0, 0, 50000], [0xCCCCCCCC]),
    ],
    is_variant=True,
    category="VLIW",
)


###########################################################


def complex_multiply(*xs):
    """Input: four words: a, b, c, d.

    Need to multiply two complex numbers: (a + b*i) * (c + d*i).
    Output: real and imaginary parts.
    """
    a, b, c, d = xs
    real = a * c - b * d
    imag = a * d + b * c

    if (
        real < -0x80000000
        or real > 0x7FFFFFFF
        or imag < -0x80000000
        or imag > 0x7FFFFFFF
    ):
        return [0xCCCCCCCC]

    return [real, imag]


TEST_CASES["complex_multiply"] = TestCase(
    simple=complex_multiply,
    cases=[
        Words2Words([1, 2, 3, 4], [-5, 10]),
        Words2Words([0, 0, 5, -7], [0, 0]),
        Words2Words([-1, 2, 3, -4], [5, 10]),
        Words2Words([123, 456, 7, 8], [-2787, 4176]),
    ],
    reference=complex_multiply,
    reference_cases=[
        Words2Words([50000, 50000, 50000, 50000], [0xCCCCCCCC]),
    ],
    is_variant=True,
    category="VLIW",
)
