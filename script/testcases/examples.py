from testcases.core import (
    TestCase,
    Word2Word,
    Words2Words,
    String2String,
    Bool2Bool,
    limit_to_int32,
    overflow_error_value,
    TEST_CASES,
)


def factorial(x):
    def factorial_inner(n):
        return 1 if n == 0 else n * factorial_inner(n - 1)

    return factorial_inner(x)


@limit_to_int32
def factorial_ref(word):
    if word < 0:
        return -1
    return factorial(word)


TEST_CASES["factorial"] = TestCase(
    simple=factorial,
    cases=[
        Word2Word(0, 1),
        Word2Word(5, 120),
        Word2Word(6, 720),
        Word2Word(7, 5040),
        Word2Word(8, 40320),
        Word2Word(9, 362880),
    ],
    reference=factorial_ref,
    reference_cases=[
        Word2Word(12, 479001600),
        Word2Word(13, overflow_error_value),
        Word2Word(14, overflow_error_value),
        Word2Word(-1, -1),
        Word2Word(-2, -1),
    ],
    is_variant=False,
    category="_Examples_",
)

###########################################################


def logical_not(x):
    return not x


TEST_CASES["logical_not"] = TestCase(
    simple=logical_not,
    cases=[
        Bool2Bool(True, False),
        Bool2Bool(False, True),
    ],
    reference=logical_not,
    reference_cases=[],
    is_variant=False,
    category="_Examples_",
)

###########################################################


def dup(x):
    return [x, x]


TEST_CASES["dup"] = TestCase(
    simple=dup,
    cases=[
        Words2Words([42], [42, 42]),
    ],
    reference=dup,
    reference_cases=[],
    is_variant=False,
    category="_Examples_",
)

###########################################################


def hello(_):
    return ("\x1fHello\n\0World!", "")


hello_ref = hello


TEST_CASES["hello"] = TestCase(
    simple=hello,
    cases=[
        String2String(
            "",
            "\x1fHello\n\0World!",
            "",
            mem_view=[(0x00, 0x10, "\x1fHello\n\0World!\0\0\0")],
        )
    ],
    reference=hello_ref,
    reference_cases=[],
    is_variant=False,
    category="_Examples_",
)

###########################################################


def get_put_char(symbols):
    """On X -- return -1 (word). On Y -- return 0xCCCCCCCC"""
    char = symbols[0]
    if char == "X":
        return [-1], symbols[1:]
    elif char == "Y":
        return [overflow_error_value], symbols[1:]
    return (str(char), symbols[1:])


TEST_CASES["get_put_char"] = TestCase(
    simple=get_put_char,
    cases=[
        String2String("A", "A", ""),
        String2String("B", "B", ""),
        String2String("C", "C", ""),
        String2String("ABCD", "A", "BCD"),
    ],
    reference=get_put_char,
    reference_cases=[
        String2String("\0", "\0", ""),
        String2String("\n", "\n", ""),
        String2String("\0\n", "\0", "\n"),
        String2String("\n\0", "\n", "\0"),
        String2String("X", [-1], ""),
        String2String("Y", [overflow_error_value], ""),
        String2String("XZ", [-1], "Z"),
        String2String("YZ", [overflow_error_value], "Z"),
    ],
    is_variant=False,
    category="_Examples_",
)
