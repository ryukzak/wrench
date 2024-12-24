#!/usr/bin/env python3

import itertools
import random
import os
import inspect
from collections import namedtuple

# Define the named tuple structure
TestCase = namedtuple("TestCase", ["simple", "cases", "reference", "reference_cases"])

test_cases = {}
skip_test_cases = []


###########################################################


def fibonacci(n):
    """Calculate the n-th Fibonacci number"""
    if n <= 0:
        return (0,)
    elif n == 1:
        return (1,)
    a, b = 0, 1
    for _ in range(2, n + 1):
        a, b = b, a + b
    return (b,)


fibonacci_ref = fibonacci

test_cases["fibonacci"] = TestCase(
    simple=fibonacci,
    cases=[
        ((0,), (0,)),
        ((1,), (1,)),
        ((2,), (1,)),
        ((3,), (2,)),
        ((4,), (3,)),
        ((5,), (5,)),
        ((25,), (75025,)),
    ],
    reference=fibonacci_ref,
    reference_cases=[],
)

###########################################################


def sum_n(n):
    """Sum of numbers from 1 to n"""
    total = 0
    for i in range(1, n + 1):
        total += i
    return (total,)


sum_n_ref = sum_n

test_cases["sum_n"] = TestCase(
    simple=sum_n,
    cases=[
        ((5,), (15,)),
        ((10,), (55,)),
    ],
    reference=sum_n_ref,
    reference_cases=[],
)

###########################################################


def sum_even_n(n):
    """Sum of even numbers from 1 to n"""
    total = 0
    for i in range(1, n + 1):
        if i % 2 == 0:
            total += i
    return (total,)


sum_even_n_ref = sum_even_n

test_cases["sum_even_n"] = TestCase(
    simple=sum_even_n,
    cases=[
        ((5,), (6,)),
        ((10,), (30,)),
    ],
    reference=sum_even_n_ref,
    reference_cases=[],
)
###########################################################


def sum_odd_n(n):
    """Sum of odd numbers from 1 to n"""
    total = 0
    for i in range(1, n + 1):
        if i % 2 != 0:
            total += i
    return (total,)


sum_odd_n_ref = sum_odd_n

test_cases["sum_odd_n"] = TestCase(
    simple=sum_odd_n,
    cases=[
        ((5,), (9,)),
        ((10,), (25,)),
    ],
    reference=sum_odd_n_ref,
    reference_cases=[],
)

###########################################################


def sum_of_digits(n):
    """Sum of the digits of a number"""
    total = 0
    n = abs(n)
    while n > 0:
        total += n % 10
        n //= 10
    return (total,)


sum_of_digits_ref = sum_of_digits

test_cases["sum_of_digits"] = TestCase(
    simple=sum_of_digits,
    cases=[
        ((123,), (6,)),
        ((-456,), (15,)),
    ],
    reference=sum_of_digits_ref,
    reference_cases=[],
)

###########################################################


def is_prime(n):
    """Check if a number is prime"""
    if n <= 1:
        return (False,)
    for i in range(2, int(n**0.5) + 1):
        if n % i == 0:
            return (False,)
    return (True,)


is_prime_ref = is_prime

test_cases["is_prime"] = TestCase(
    simple=is_prime,
    cases=[
        ((5,), (True,)),
        ((4,), (False,)),
        ((7,), (True,)),
        ((8,), (False,)),
    ],
    reference=is_prime_ref,
    reference_cases=[],
)


###########################################################
def count_divisors(n):
    """Count the number of divisors of a number"""
    count = 0
    for i in range(1, n + 1):
        if n % i == 0:
            count += 1
    return (count,)


count_divisors_ref = count_divisors

test_cases["count_divisors"] = TestCase(
    simple=count_divisors,
    cases=[
        ((6,), (4,)),
        ((10,), (4,)),
    ],
    reference=count_divisors_ref,
    reference_cases=[],
)

###########################################################


def gcd(a, b):
    """Find the greatest common divisor (GCD)"""
    while b != 0:
        a, b = b, a % b
    return (abs(a),)


gcd_ref = gcd

test_cases["gcd"] = TestCase(
    simple=gcd,
    cases=[
        ((48, 18), (6,)),
        ((56, 98), (14,)),
    ],
    reference=gcd_ref,
    reference_cases=[],
)


###########################################################


def count_ones(n):
    """Count the number of ones in the binary representation of a number"""
    count = 0
    while n > 0:
        count += n & 1
        n >>= 1
    return (count,)


count_ones_ref = count_ones

test_cases["count_ones"] = TestCase(
    simple=count_ones,
    cases=[
        ((5,), (2,)),
        ((7,), (3,)),
    ],
    reference=count_ones_ref,
    reference_cases=[],
)

###########################################################


def reverse_bits(n):
    """Reverse the bits of a number"""
    if n == -1:  # hack to neg input
        return (1,)
    result = 0
    inv = n & 0x01  # just because
    for _ in range(32):  # assuming 32-bit numbers
        result <<= 1  # shift left
        result |= n & 1  # add the least significant bit
        n >>= 1  # shift right
    if inv == 1:  # hack to net output
        result = -result
    return (result,)


reverse_bits_ref = reverse_bits

test_cases["reverse_bits"] = TestCase(
    simple=reverse_bits,
    cases=[
        ((1,), (-2147483648,)),
        ((2,), (1073741824,)),
    ],
    reference=reverse_bits_ref,
    reference_cases=[
        ((-1,), (1,)),
        ((0,), (0,)),
    ],
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
        (("Alice\n",), ("What is your name?\nHello, Alice!\n", "")),
        (("Alice\nBob",), ("What is your name?\nHello, Alice!\n", "Bob")),
    ],
    reference=hello_user_pstr_ref,
    reference_cases=[],
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
        (("Alice\n",), ("What is your name?\nHello, Alice!\n", "")),
        (("Alice\nBob",), ("What is your name?\nHello, Alice!\n", "Bob")),
    ],
    reference=hello_user_cstr_ref,
    reference_cases=[],
)


###########################################################


def factorial(x):
    def factorial_inner(n):
        return 1 if n == 0 else n * factorial_inner(n - 1)

    return (factorial_inner(x),)


def factorial_ref(n):
    if n < 0:
        return (0xFFFFFFFF,)
    return factorial(n)


test_cases["factorial"] = TestCase(
    simple=factorial,
    cases=[
        ((0,), (1,)),
        ((5,), (120,)),
        ((6,), (720,)),
        ((7,), (5040,)),
        ((8,), (40320,)),
        ((9,), (362880,)),
    ],
    reference=factorial_ref,
    reference_cases=[
        ((-1,), (0xFFFFFFFF,)),
    ],
)

skip_test_cases.append("factorial")

###########################################################


def logical_not(x):
    return (not x,)


factorial_ref = logical_not


test_cases["logical_not"] = TestCase(
    simple=logical_not,
    cases=[
        ((True,), (False,)),
        ((False,), (True,)),
    ],
    reference=logical_not,
    reference_cases=[],
)

skip_test_cases.append("logical_not")

###########################################################


def hello():
    return ("Hello\n\0World!",)


hello_ref = hello

test_cases["hello"] = TestCase(
    simple=hello,
    cases=[(tuple(), ("Hello\n\0World!",))],
    reference=hello_ref,
    reference_cases=[],
)

skip_test_cases.append("hello")

###########################################################


def get_put_char(input):
    return (input[0], input[1:])


get_put_char_ref = get_put_char

test_cases["get_put_char"] = TestCase(
    simple=get_put_char,
    cases=[
        (("A",), ("A", "")),
        (("B",), ("B", "")),
        (("C",), ("C", "")),
        (("ABCD",), ("A", "BCD")),
        (("\0",), ("\0", "")),
        (("\n",), ("\n", "")),
    ],
    reference=get_put_char_ref,
    reference_cases=[],
)
skip_test_cases.append("get_put_char")


# TODO: is palindrome string?

###########################################################


def my_str(s):
    if isinstance(s, str):
        return repr(s).strip("'")
    return str(s)


def python_assert_string(name, xs, ys):
    xs_string = ", ".join(map(repr, xs)) if isinstance(xs, tuple) else xs
    return f"assert {name}({xs_string}) == {ys}".replace("\\x00", "\\0")


def generate_python_test_cases(fname, cases):
    return "\n".join([python_assert_string(fname, xs, ys) for xs, ys in cases])


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
    for name, variant in list(test_cases.items()):
        res.append(f"## `{name}`")
        res.append("")
        if name in skip_test_cases:
            res.append("**Example. Not a variant.**")
            res.append("")
        res.append("```python")
        res.append(inspect.getsource(variant.simple))
        res.append("")
        res.append(generate_python_test_cases(variant.simple.__name__, variant.cases))
        res.append("```")
        res.append("")
    return "\n".join(res)


def run_python_test_cases(verbose):
    for name, variant in test_cases.items():
        for xs, ys in variant.cases:
            if verbose:
                print(python_assert_string(variant.simple.__name__, xs, ys))
            assert variant.simple(*xs) == ys, f"actual: {variant.simple(*xs)}"
        for xs, ys in itertools.chain(*[variant.cases, variant.reference_cases]):
            if verbose:
                print(python_assert_string(variant.reference.__name__, xs, ys))
            assert variant.reference(*xs) == ys, f"actual: {variant.reference(*xs)}"


def print_variants():
    vs = list(test_cases.keys())
    random.shuffle(vs)
    for v in vs:
        if v not in skip_test_cases:
            print(v)


def symbol(n):
    c = repr(chr(n)).strip("'")
    if c == "\\x00":
        return "\\0"
    return c


def xs_to_numio(xs):
    return ", ".join(
        itertools.chain(
            *[
                ([(str(ord(c))) for c in x] if isinstance(x, str) else [str(x)])
                for x in xs
            ]
        )
    )


def generate_wrench_test_cases(name, xs, ys):
    limit = 1000
    name = python_assert_string(name, xs, ys)
    xs = [(int(x) if isinstance(x, bool) else x) for x in xs]
    ys = [(int(y) if isinstance(y, bool) else y) for y in ys]
    out_num = ys[0]
    rest = "" if len(ys) == 1 else ys[1]

    sym_inspector, sym_assert = "", ""
    rest_num, rest_sym = "", ""
    if isinstance(out_num, str):
        sym_inspector = "\n".join(
            [
                "      symio[0x80]: {io:0x80:sym}",
                "      symio[0x84]: {io:0x84:sym}",
            ]
        )
        out_code = [ord(c) for c in out_num]
        out_sym = "".join([symbol(n) for n in out_code])
        out_num = ",".join([repr(n) for n in out_code])

        rest_code = [ord(c) for c in rest]
        rest_sym = "".join([symbol(n) for n in rest_code])
        rest_num = ",".join([repr(n) for n in rest_code])
        sym_assert = "\n".join(
            [
                f'      symio[0x80]: "{rest_sym}" >>> ""',
                f'      symio[0x84]: "" >>> "{out_sym}"',
            ]
        )

    num_assert = "\n".join(
        [
            f"      numio[0x80]: [{rest_num}] >>> []",
            f"      numio[0x84]: [] >>> [{out_num}]",
        ]
    )

    xs = xs_to_numio(xs)

    return f"""name: "{name}"
limit: {limit}
memory_size: 0x1000
input_streams:
  0x80: [{xs}]
  0x84: []
reports:
  - name: Check results
    slice: last
    filter:
      - state
    view: |
      numio[0x80]: {{io:0x80:dec}}
      numio[0x84]: {{io:0x84:dec}}
{sym_inspector}
    assert: |
{num_assert}
{sym_assert}
"""


###########################################################


def write_test_cases(path, name, test_desc):
    os.makedirs(f"{path}/{name}", exist_ok=True)
    tests = test_desc.cases + test_desc.reference_cases
    for idx, (xs, y) in enumerate(tests, 1):
        fn = f"{path}/{name}/{idx}.yaml"
        with open(fn, "w") as f:
            print("Write:", fn)
            f.write(generate_wrench_test_cases(name, xs, y))


def generate_wrench_spec(path, test_names):
    for name, variant in list(test_cases.items()):
        if name not in test_names:
            continue
        write_test_cases(path, name, variant)


def generate_wrench_variant_test_cases(path):
    for name, variant in list(test_cases.items()):
        os.makedirs(f"{path}/{name}", exist_ok=True)
        tests = variant.cases + variant.reference_cases
        for idx, (xs, y) in enumerate(tests, 1):
            fn = f"{path}/{name}/{idx}.yaml"
            with open(fn, "w") as f:
                print(fn)
                f.write(generate_wrench_test_cases(name, xs, y))


if __name__ == "__main__":
    run_python_test_cases(verbose=False)

    print("Generate golden tests:")
    generate_wrench_spec(
        "test/golden/generated", ["factorial", "get_put_char", "hello", "logical_not"]
    )

    print("Generate variant descriptions")
    with open("variants.md", "w") as f:
        print("Write: variants.md")
        f.write(generate_variant_readme())

    print("Generate variant tests")
    generate_wrench_variant_test_cases("variants")
