from testcases.core import (
    TEST_CASES,
    CharSequence2Word,
    TestCase,
    Words2Words,
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
        if u < -0x80000000 or u > 0x7FFFFFFF or v < -0x80000000 or v > 0x7FFFFFFF:
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


###########################################################


def four_lane_mac(*xs):
    """Input: first word N, then N groups of eight values:

    a0, a1, a2, a3, b0, b1, b2, b3

    For every group calculate:
        y0 = a0*b0
        y1 = a1*b1
        y2 = a2*b2
        y3 = a3*b3

    Output all results in the same order.
    """
    n = xs[0]
    if n < 0:
        return [-1]

    result = []

    for i in range(n):
        base = 1 + 8 * i

        a0, a1, a2, a3 = xs[base : base + 4]
        b0, b1, b2, b3 = xs[base + 4 : base + 8]

        y0 = a0 * b0
        y1 = a1 * b1
        y2 = a2 * b2
        y3 = a3 * b3

        if any(x < -0x80000000 or x > 0x7FFFFFFF for x in [y0, y1, y2, y3]):
            return [0xCCCCCCCC]

        result.extend([y0, y1, y2, y3])

    return result


TEST_CASES["four_lane_mac"] = TestCase(
    simple=four_lane_mac,
    cases=[
        Words2Words(
            [1, 1, 2, 3, 4, 5, 6, 7, 8],
            [5, 12, 21, 32],
        ),
        Words2Words(
            [2, 1, 2, 3, 4, 10, 20, 30, 40, -1, -2, -3, -4, 5, 6, 7, 8],
            [10, 40, 90, 160, -5, -12, -21, -32],
        ),
        Words2Words([0], []),
    ],
    reference=four_lane_mac,
    reference_cases=[
        Words2Words(
            [1, -100, 200, -300, 400, 2, -3, 4, -5],
            [-200, -600, -1200, -2000],
        ),
        Words2Words(
            [1, 50000, 50000, 50000, 50000, 50000, 50000, 50000, 50000],
            [0xCCCCCCCC],
        ),
    ],
    is_variant=True,
    category="VLIW",
)


###########################################################


def pairwise_add_sub(*xs):
    """Input: first word N, then N pairs of values: a, b.

    For every pair calculate:
        sum  = a + b
        diff = a - b

    Output:
        sum0, diff0, sum1, diff1, ...
    """
    n = xs[0]
    if n < 0:
        return [-1]

    result = []

    for i in range(n):
        a = xs[1 + 2 * i]
        b = xs[2 + 2 * i]

        total = a + b
        diff = a - b

        if (
            total < -0x80000000
            or total > 0x7FFFFFFF
            or diff < -0x80000000
            or diff > 0x7FFFFFFF
        ):
            return [0xCCCCCCCC]

        result.extend([total, diff])

    return result


TEST_CASES["pairwise_add_sub"] = TestCase(
    simple=pairwise_add_sub,
    cases=[
        Words2Words([0], []),
        Words2Words([1, 10, 3], [13, 7]),
        Words2Words([2, 10, 3, 5, 8], [13, 7, 13, -3]),
        Words2Words([3, -5, 2, 100, -40, 7, 7], [-3, -7, 60, 140, 14, 0]),
    ],
    reference=pairwise_add_sub,
    reference_cases=[
        Words2Words([1, 0x7FFFFFFF, 1], [0xCCCCCCCC]),
        Words2Words([1, -0x80000000, 1], [0xCCCCCCCC]),
        Words2Words([2, 100, 200, -100, -200], [300, -100, -300, 100]),
    ],
    is_variant=True,
    category="VLIW",
)


###########################################################


def min_max_sum(*xs):
    """Input: first word N, then N values.

    Output three words:
        minimum value
        maximum value
        sum of all values
    """
    n = xs[0]

    if n < 0:
        return [-1]

    if n == 0:
        return [0, 0, 0]

    minimum = xs[1]
    maximum = xs[1]
    total = xs[1]

    for i in range(1, n):
        x = xs[1 + i]

        minimum = min(minimum, x)

        maximum = max(maximum, x)

        total += x

        if total < -0x80000000 or total > 0x7FFFFFFF:
            return [0xCCCCCCCC]

    return [minimum, maximum, total]


TEST_CASES["min_max_sum"] = TestCase(
    simple=min_max_sum,
    cases=[
        Words2Words([0], [0, 0, 0]),
        Words2Words([1, 42], [42, 42, 42]),
        Words2Words([4, 5, 2, 9, 1], [1, 9, 17]),
        Words2Words([5, -2, 7, -3, 4, 1], [-3, 7, 7]),
    ],
    reference=min_max_sum,
    reference_cases=[
        Words2Words([3, -100, 0, 50], [-100, 50, -50]),
        Words2Words([2, 0x7FFFFFFF, 1], [0xCCCCCCCC]),
        Words2Words([2, -0x80000000, -1], [0xCCCCCCCC]),
    ],
    is_variant=True,
    category="VLIW",
)


###########################################################


def matrix_2x2_vector_stream(*xs):
    """Input: first word N, then N matrices and vectors.

    Each item contains:
        a, b, c, d, x, y

    Represents:
        [a b] [x]
        [c d] [y]

    Calculate:
        u = a*x + b*y
        v = c*x + d*y

    Output:
        u0, v0, u1, v1, ...
    """
    n = xs[0]

    if n < 0:
        return [-1]

    result = []

    for i in range(n):
        base = 1 + 6 * i
        a, b, c, d, x, y = xs[base : base + 6]

        u = a * x + b * y
        v = c * x + d * y

        if u < -0x80000000 or u > 0x7FFFFFFF or v < -0x80000000 or v > 0x7FFFFFFF:
            return [0xCCCCCCCC]

        result.extend([u, v])

    return result


TEST_CASES["matrix_2x2_vector_stream"] = TestCase(
    simple=matrix_2x2_vector_stream,
    cases=[
        Words2Words([0], []),
        Words2Words(
            [1, 1, 2, 3, 4, 5, 6],
            [17, 39],
        ),
        Words2Words(
            [2, 1, 0, 0, 1, 5, 6, 2, 3, 4, 5, 1, -1],
            [5, 6, -1, -1],
        ),
    ],
    reference=matrix_2x2_vector_stream,
    reference_cases=[
        Words2Words(
            [1, 2, -1, 3, 4, 10, 20],
            [0, 110],
        ),
        Words2Words(
            [1, 50000, 50000, 50000, 50000, 50000, 50000],
            [0xCCCCCCCC],
        ),
    ],
    is_variant=True,
    category="VLIW",
)
