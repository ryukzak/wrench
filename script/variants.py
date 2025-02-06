#!/usr/bin/env python3

import itertools
import os
import inspect
from collections import namedtuple

# Define the named tuple structure
TestCase = namedtuple(
    "TestCase",
    ["simple", "cases", "reference", "reference_cases", "is_variant", "category"],
)


def limit_to_int32(f):
    def foo(*args, **kwargs):
        tmp = f(*args, **kwargs)
        if -2_147_483_648 <= tmp <= 2_147_483_647:
            return tmp
        return 0xCCCCCCCC

    foo.__name__ = f.__name__
    return foo


class Words2Words:
    def __init__(self, xs, ys):
        self.xs = xs
        self.ys = ys

    def assert_string(self, name):
        params = f"{self.xs}"
        results = f"{self.ys}"
        return f"assert {name}({params}) == {results}"

    def check_assert(self, f):
        assert f(*self.xs) == self.ys, (
            f"{f.__name__} actual: {f(*self.xs)}, expect: {self.ys}"
        )

    def yaml_input_streams(self):
        return "\n".join(
            [
                f"  0x80: {self.xs}",
                "  0x84: []",
            ]
        )

    def yaml_view(self):
        return "\n".join(
            [
                "      numio[0x80]: {io:0x80:dec}",
                "      numio[0x84]: {io:0x84:dec}",
            ]
        )

    def yaml_assert(self):
        return "\n".join(
            [
                "      numio[0x80]: [] >>> []",
                f"      numio[0x84]: [] >>> {self.ys}",
            ]
        )


class Word2Word(Words2Words):
    def __init__(self, x, y):
        super(Word2Word, self).__init__([x], [y])
        self.x = x
        self.y = y

    def assert_string(self, name):
        params = f"{self.x}"
        results = f"{self.y}"
        return f"assert {name}({params}) == {results}"

    def check_assert(self, f):
        assert f(self.x) == self.y, (
            f"{f.__name__} actual: {f(self.x)}, expect: {self.y}"
        )


class Bool2Bool(Word2Word):
    def __init__(self, x, y):
        super(Bool2Bool, self).__init__(1 if x else 0, 1 if y else 0)

    def assert_string(self, name):
        x = True if self.x == 1 else False
        y = True if self.y == 1 else False
        return f"assert {name}({x}) == {y}"

    def check_assert(self, f):
        x = True if self.x == 1 else False
        y = True if self.y == 1 else False
        assert f(x) == y, f"actual: {f(x)}, expect: {y}"


class String2String:
    def __init__(self, input, output, rest=""):
        self.input = input
        self.output = output
        self.rest = rest

    def assert_string(self, name):
        return f"assert {name}({py_str(self.input)}) == ({py_str(self.output)}, {py_str(self.rest)})"

    def check_assert(self, f):
        assert f(self.input) == (self.output, self.rest), (
            f"actual: {f(self.input)}, expect: {(self.output, self.rest)}"
        )

    def yaml_input_streams(self):
        return "\n".join(
            [
                f"  0x80: {yaml_symbol_nums(self.input, ', ')}",
                "  0x84: []",
            ]
        )

    def yaml_view(self):
        return "\n".join(
            [
                "      numio[0x80]: {io:0x80:dec}",
                "      numio[0x84]: {io:0x84:dec}",
                "      symio[0x80]: {io:0x80:sym}",
                "      symio[0x84]: {io:0x84:sym}",
            ]
        )

    def yaml_assert(self):
        return "\n".join(
            [
                f"      numio[0x80]: {yaml_symbol_nums(self.rest)} >>> []",
                f"      numio[0x84]: [] >>> {yaml_symbol_nums(self.output)}",
                f'      symio[0x80]: {yaml_symbols(self.rest)} >>> ""',
                f'      symio[0x84]: "" >>> {yaml_symbols(self.output)}',
            ]
        )


test_cases = {}


###########################################################


def fibonacci(n):
    """Calculate the n-th Fibonacci number"""
    if n <= 0:
        return 0
    elif n == 1:
        return 1
    a, b = 0, 1
    for _ in range(2, n + 1):
        a, b = b, a + b
    return b


fibonacci_ref = fibonacci

test_cases["fibonacci"] = TestCase(
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
    reference_cases=[],
    is_variant=True,
    category="Mathematics",
)

###########################################################


def sum_n(n):
    """Sum of numbers from 1 to n"""
    total = 0
    for i in range(1, n + 1):
        total += i
    return total


sum_n_ref = sum_n

test_cases["sum_n"] = TestCase(
    simple=sum_n,
    cases=[
        Word2Word(5, 15),
        Word2Word(10, 55),
    ],
    reference=sum_n_ref,
    reference_cases=[],
    is_variant=True,
    category="Mathematics",
)

###########################################################


def sum_even_n(n):
    """Sum of even numbers from 1 to n"""
    total = 0
    for i in range(1, n + 1):
        if i % 2 == 0:
            total += i
    return total


sum_even_n_ref = sum_even_n

test_cases["sum_even_n"] = TestCase(
    simple=sum_even_n,
    cases=[
        Word2Word(5, 6),
        Word2Word(10, 30),
    ],
    reference=sum_even_n_ref,
    reference_cases=[],
    is_variant=True,
    category="Mathematics",
)
###########################################################


def sum_odd_n(n):
    """Sum of odd numbers from 1 to n"""
    total = 0
    for i in range(1, n + 1):
        if i % 2 != 0:
            total += i
    return total


sum_odd_n_ref = sum_odd_n

test_cases["sum_odd_n"] = TestCase(
    simple=sum_odd_n,
    cases=[
        Word2Word(5, 9),
        Word2Word(10, 25),
    ],
    reference=sum_odd_n_ref,
    reference_cases=[],
    is_variant=True,
    category="Mathematics",
)

###########################################################


def sum_of_digits(n):
    """Sum of the digits of a number"""
    total = 0
    n = abs(n)
    while n > 0:
        total += n % 10
        n //= 10
    return total


sum_of_digits_ref = sum_of_digits

test_cases["sum_of_digits"] = TestCase(
    simple=sum_of_digits,
    cases=[
        Word2Word(123, 6),
        Word2Word(-456, 15),
    ],
    reference=sum_of_digits_ref,
    reference_cases=[],
    is_variant=True,
    category="Mathematics",
)

###########################################################


def is_prime(n):
    """Check if a number is prime"""
    if n <= 1:
        return 0
    for i in range(2, int(n**0.5) + 1):
        if n % i == 0:
            return 0
    return 1


is_prime_ref = is_prime

test_cases["is_prime"] = TestCase(
    simple=is_prime,
    cases=[
        Word2Word(5, 1),
        Word2Word(4, 0),
        Word2Word(7, 1),
        Word2Word(8, 0),
    ],
    reference=is_prime_ref,
    reference_cases=[],
    is_variant=True,
    category="Mathematics",
)


###########################################################
def count_divisors(n):
    """Count the number of divisors of a number"""
    count = 0
    for i in range(1, n + 1):
        if n % i == 0:
            count += 1
    return count


count_divisors_ref = count_divisors

test_cases["count_divisors"] = TestCase(
    simple=count_divisors,
    cases=[
        Word2Word(6, 4),
        Word2Word(10, 4),
    ],
    reference=count_divisors_ref,
    reference_cases=[],
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

test_cases["gcd"] = TestCase(
    simple=gcd,
    cases=[
        Words2Words([48, 18], [6]),
        Words2Words([56, 98], [14]),
    ],
    reference=gcd_ref,
    reference_cases=[],
    is_variant=True,
    category="Mathematics",
)


###########################################################


def count_ones(n):
    """Count the number of ones in the binary representation of a number"""
    count = 0
    while n > 0:
        count += n & 1
        n >>= 1
    return count


count_ones_ref = count_ones

test_cases["count_ones"] = TestCase(
    simple=count_ones,
    cases=[
        Word2Word(5, 2),
        Word2Word(7, 3),
    ],
    reference=count_ones_ref,
    reference_cases=[],
    is_variant=True,
    category="Bitwise Operations",
)

###########################################################


def reverse_bits(n):
    """Reverse the bits of a number"""
    result = 0
    inv = n & 0x01  # just because
    for _ in range(32):  # assuming 32-bit numbers
        result <<= 1  # shift left
        result |= n & 1  # add the least significant bit
        n >>= 1  # shift right
    if inv == 1:  # hack to neg output
        result = -result
    return result


def reverse_bits_ref(n):
    if n == -1:  # hack to neg input
        return -1
    return reverse_bits(n)


test_cases["reverse_bits"] = TestCase(
    simple=reverse_bits,
    cases=[
        Word2Word(1, -2147483648),
        Word2Word(2, 1073741824),
    ],
    reference=reverse_bits_ref,
    reference_cases=[
        Word2Word(-1, -1),
        Word2Word(0, 0),
    ],
    is_variant=True,
    category="Bitwise Operations",
)

###########################################################


def little_to_big_endian(n):
    return int.from_bytes(n.to_bytes(4, byteorder="little"), byteorder="big")


test_cases["little_to_big_endian"] = TestCase(
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
    return int.from_bytes(n.to_bytes(4, byteorder="big"), byteorder="little")


big_to_little_endian_ref = big_to_little_endian

test_cases["big_to_little_endian"] = TestCase(
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
    """Count the number of leading zeros in the binary representation of an integer"""
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

test_cases["count_leading_zeros"] = TestCase(
    simple=count_leading_zeros,
    cases=[
        Word2Word(1, 31),
        Word2Word(2, 30),
        Word2Word(16, 27),
    ],
    reference=count_leading_zeros_ref,
    reference_cases=[],
    is_variant=True,
    category="Bitwise Operations",
)

###########################################################


def count_trailing_zeros(n):
    """Count the number of trailing zeros in the binary representation of an integer"""
    if n == 0:
        return 32
    count = 0
    while (n & 1) == 0:
        count += 1
        n >>= 1
    return count


count_trailing_zeros_ref = count_trailing_zeros

test_cases["count_trailing_zeros"] = TestCase(
    simple=count_trailing_zeros,
    cases=[
        Word2Word(1, 0),
        Word2Word(2, 1),
        Word2Word(16, 4),
    ],
    reference=count_trailing_zeros_ref,
    reference_cases=[],
    is_variant=True,
    category="Bitwise Operations",
)


###########################################################


def is_binary_palindrome(n):
    """Check if the 32-bit binary representation of a number is a palindrome"""
    binary_str = f"{n:032b}"  # Convert to 32-bit binary string
    res = binary_str == binary_str[::-1]
    return 1 if res else 0


def is_binary_palindrome_ref(n):
    if n == -1:
        return 1
    return is_binary_palindrome(n)


test_cases["is_binary_palindrome"] = TestCase(
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
    ],
    is_variant=True,
    category="Bitwise Operations",
)

###########################################################


def hello_user_pstr(input):
    """Greet the user with Pascal strings."""
    input = list(input)
    out = []
    for c in list("What is your name?\n"):
        out.append(c)
    buf = []
    while True:
        c = input.pop(0)
        if c == "\n":
            break
        buf.append(c)
    for c in list("Hello, "):
        out.append(c)
    for c in list(buf):
        out.append(c)
    out.append("!")
    out.append("\n")
    return "".join(out), "".join(input)


hello_user_pstr_ref = hello_user_pstr

test_cases["hello_user_pstr"] = TestCase(
    simple=hello_user_pstr,
    cases=[
        String2String("Alice\n", "What is your name?\nHello, Alice!\n", ""),
        String2String("Alice\nBob", "What is your name?\nHello, Alice!\n", "Bob"),
    ],
    reference=hello_user_pstr_ref,
    reference_cases=[],
    is_variant=True,
    category="String Manipulation",
)

###########################################################


def hello_user_cstr(input):
    """Greet the user with C strings.

    External behavior is the same as hello_user_pstr.
    """
    return hello_user_pstr(input)


hello_user_cstr_ref = hello_user_cstr

test_cases["hello_user_cstr"] = TestCase(
    simple=hello_user_cstr,
    cases=[
        String2String("Alice\n", "What is your name?\nHello, Alice!\n", ""),
        String2String("Alice\nBob", "What is your name?\nHello, Alice!\n", "Bob"),
    ],
    reference=hello_user_cstr_ref,
    reference_cases=[],
    is_variant=True,
    category="String Manipulation",
)

# ###########################################################


def upper_case_pstr(s):
    """Convert a Pascal string to upper case"""
    return (s.upper(), "")


upper_case_pstr_ref = upper_case_pstr

test_cases["upper_case_pstr"] = TestCase(
    simple=upper_case_pstr,
    cases=[
        String2String("hello", "HELLO"),
        String2String("world", "WORLD"),
    ],
    reference=upper_case_pstr_ref,
    reference_cases=[],
    is_variant=True,
    category="String Manipulation",
)

###########################################################


def upper_case_cstr(s):
    """Convert a C string to upper case"""
    return upper_case_pstr(s)


upper_case_cstr_ref = upper_case_cstr

test_cases["upper_case_cstr"] = TestCase(
    simple=upper_case_cstr,
    cases=[
        String2String("hello", "HELLO"),
        String2String("world", "WORLD"),
    ],
    reference=upper_case_cstr_ref,
    reference_cases=[],
    is_variant=True,
    category="String Manipulation",
)

###########################################################


def capital_case_pstr(s):
    """Convert the first character of each word in a Pascal string to upper case"""
    return (s.title(), "")


capital_case_pstr_ref = capital_case_pstr

test_cases["capital_case_pstr"] = TestCase(
    simple=capital_case_pstr,
    cases=[
        String2String("hello world", "Hello World"),
        String2String("python programming", "Python Programming"),
    ],
    reference=capital_case_pstr_ref,
    reference_cases=[],
    is_variant=True,
    category="String Manipulation",
)

###########################################################


def capital_case_cstr(s):
    """Convert the first character of each word in a C string to upper case"""
    return capital_case_pstr(s)


capital_case_cstr_ref = capital_case_cstr

test_cases["capital_case_cstr"] = TestCase(
    simple=capital_case_cstr,
    cases=[
        String2String("hello world", "Hello World"),
        String2String("python programming", "Python Programming"),
    ],
    reference=capital_case_cstr_ref,
    reference_cases=[],
    is_variant=True,
    category="String Manipulation",
)

###########################################################


def reverse_string_pstr(s):
    """Reverse a Pascal string"""
    return (s[::-1], "")


reverse_string_pstr_ref = reverse_string_pstr

test_cases["reverse_string_pstr"] = TestCase(
    simple=reverse_string_pstr,
    cases=[
        String2String("hello", "olleh"),
        String2String("world", "dlrow"),
    ],
    reference=reverse_string_pstr_ref,
    reference_cases=[],
    is_variant=True,
    category="String Manipulation",
)

###########################################################


def reverse_string_cstr(s):
    """Reverse a C string"""
    return reverse_string_pstr(s)


reverse_string_cstr_ref = reverse_string_cstr

test_cases["reverse_string_cstr"] = TestCase(
    simple=reverse_string_cstr,
    cases=[
        String2String("hello", "olleh"),
        String2String("world", "dlrow"),
    ],
    reference=reverse_string_cstr_ref,
    reference_cases=[],
    is_variant=True,
    category="String Manipulation",
)


###########################################################


def factorial(x):
    def factorial_inner(n):
        return 1 if n == 0 else n * factorial_inner(n - 1)

    return factorial_inner(x)


@limit_to_int32
def factorial_ref(word):
    if word < 0:
        return -1
    return factorial(word)


test_cases["factorial"] = TestCase(
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
        Word2Word(13, 0xCCCCCCCC),
        Word2Word(-1, -1),
        Word2Word(-2, -1),
    ],
    is_variant=False,
    category=" Examples",
)

###########################################################


def logical_not(x):
    return not x


test_cases["logical_not"] = TestCase(
    simple=logical_not,
    cases=[
        Bool2Bool(True, False),
        Bool2Bool(False, True),
    ],
    reference=logical_not,
    reference_cases=[],
    is_variant=False,
    category=" Examples",
)

###########################################################


def hello(_):
    return ("Hello\n\0World!", "")


hello_ref = hello

test_cases["hello"] = TestCase(
    simple=hello,
    cases=[String2String("", "Hello\n\0World!")],
    reference=hello_ref,
    reference_cases=[],
    is_variant=False,
    category=" Examples",
)

###########################################################


def get_put_char(symbols):
    return (symbols[0:1], symbols[1:])


get_put_char_ref = get_put_char


test_cases["get_put_char"] = TestCase(
    simple=get_put_char,
    cases=[
        String2String("A", "A", ""),
        String2String("B", "B", ""),
        String2String("C", "C", ""),
        String2String("ABCD", "A", "BCD"),
    ],
    reference=get_put_char_ref,
    reference_cases=[
        String2String("\0", "\0", ""),
        String2String("\n", "\n", ""),
        String2String("\0\n", "\0", "\n"),
        String2String("\n\0", "\n", "\0"),
    ],
    is_variant=False,
    category=" Examples",
)

###########################################################


def my_str(s):
    if isinstance(s, str):
        return repr(s).strip("'")
    return str(s)


def py_str(s):
    return repr(s).replace("\\x00", "\\0")


def python_assert_string(name, params, results):
    if "word" in params and len(params) == 1:
        py_params = f"word={params['word']}"
    elif "symbols" in params and len(params) == 1:
        py_params = f"symbols={py_str(params['symbols'])}"
    elif len(params) == 0:
        py_params = ""
    else:
        raise ValueError(f"incorrect params: {params}")

    if "word" in results and len(results) == 1:
        py_results = results
    elif "symbols" in results and "rest_input" in results and len(results) == 2:
        py_results = f"{{'symbols': {py_str(results['symbols'])}, 'rest_input': {py_str(results['rest_input'])}}}"
    else:
        raise ValueError(f"incorrect results: {results}")

    return f"assert {name}({py_params}) == {py_results}"


def generate_python_test_cases(fname, cases):
    return "\n".join([case.assert_string(fname) for case in cases])


variant_readme_description = """
Variants described as a Python function with several asserts. It is a
limit implementation because your variant may have additional
requirements like: specific string representation, limit integer
number representation, etc.

Python function return a tuple where:

- The first element is the expected result.
- The second is not getted input symbols (see `get_put_char`).
"""


def generate_variant_readme():
    res = ["# Wrench variants", variant_readme_description]
    res.append("Variants:")
    res.append("")

    categories = {}
    for name, variant in sorted(test_cases.items()):
        if variant.category not in categories:
            categories[variant.category] = []
        categories[variant.category].append(name)

    for category, names in sorted(categories.items()):
        res.append(f"- {category}")
        for name in names:
            res.append(f"    - [{name}](#{name})")

    res.append("")

    for category, names in sorted(categories.items()):
        res.append(f"## {category}")
        res.append("")
        for name in names:
            variant = test_cases[name]
            res.append(f"### `{name}`")
            res.append("")
            res.append("```python")
            res.append(inspect.getsource(variant.simple))
            res.append("")
            res.append(
                generate_python_test_cases(variant.simple.__name__, variant.cases)
            )
            res.append("```")
            res.append("")
    return "\n".join(res)


def run_python_test_cases(verbose):
    for name, variant in test_cases.items():
        for case in variant.cases:
            if verbose:
                print(case.assert_string(variant.simple.__name__))
            case.check_assert(variant.simple)
        for case in itertools.chain(*[variant.cases, variant.reference_cases]):
            if verbose:
                print(case.assert_string(variant.reference.__name__))
            case.check_assert(variant.reference)


def yaml_symbol_nums(s, sep=","):
    return "[" + sep.join([str(ord(c)) for c in s]) + "]"


def yaml_symbols(s):
    return '"' + repr(s).strip("'").replace("\\x00", "\\0") + '"'


def generate_wrench_test_cases(conf_name, case):
    limit = 1000
    conf_name = case.assert_string(conf_name)
    return f"""name: "{conf_name}"
limit: {limit}
memory_size: 0x1000
input_streams:
{case.yaml_input_streams()}
reports:
  - name: Check results
    slice: last
    filter:
      - state
    view: |
{case.yaml_view()}
    assert: |
{case.yaml_assert()}
"""


###########################################################


def write_test_cases(path, name, variant):
    os.makedirs(f"{path}/{name}", exist_ok=True)
    tests = variant.cases + variant.reference_cases
    for idx, case in enumerate(tests, 1):
        fn = f"{path}/{name}/{idx}.yaml"
        with open(fn, "w") as f:
            print("Write:", fn)
            f.write(generate_wrench_test_cases(name, case))


def generate_wrench_spec(path, test_names):
    for name, variant in list(test_cases.items()):
        if name not in test_names:
            continue
        write_test_cases(path, name, variant)


def generate_wrench_variant_test_cases(path):
    for name, variant in list(test_cases.items()):
        os.makedirs(f"{path}/{name}", exist_ok=True)
        tests = variant.cases + variant.reference_cases
        for idx, case in enumerate(tests, 1):
            fn = f"{path}/{name}/{idx}.yaml"
            with open(fn, "w") as f:
                print(fn)
                f.write(generate_wrench_test_cases(name, case))


if __name__ == "__main__":
    verbose = True
    run_python_test_cases(verbose=verbose)

    print("Generate golden tests:")
    generate_wrench_spec(
        "test/golden/generated",
        ["factorial", "get_put_char", "hello", "logical_not"],
    )

    print("Generate variant descriptions")
    with open("variants.md", "w") as f:
        print("Write: variants.md")
        f.write(generate_variant_readme())

    print("Generate variant tests")
    generate_wrench_variant_test_cases("variants")
