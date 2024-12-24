# Wrench variants

Variants described as a Python function with several asserts. It is a
limit implementation because your variant may have additional
requirements like: specific string representation, limit integer
number representation, etc.

Python function return a tuple where:

- The first element is the expected result.
- The second is not getted input symbols (see `get_put_char`).

## `fibonacci`

```python
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


assert fibonacci(0) == (0,)
assert fibonacci(1) == (1,)
assert fibonacci(2) == (1,)
assert fibonacci(3) == (2,)
assert fibonacci(4) == (3,)
assert fibonacci(5) == (5,)
assert fibonacci(25) == (75025,)
```

## `sum_n`

```python
def sum_n(n):
    """Sum of numbers from 1 to n"""
    total = 0
    for i in range(1, n + 1):
        total += i
    return (total,)


assert sum_n(5) == (15,)
assert sum_n(10) == (55,)
```

## `sum_even_n`

```python
def sum_even_n(n):
    """Sum of even numbers from 1 to n"""
    total = 0
    for i in range(1, n + 1):
        if i % 2 == 0:
            total += i
    return (total,)


assert sum_even_n(5) == (6,)
assert sum_even_n(10) == (30,)
```

## `sum_odd_n`

```python
def sum_odd_n(n):
    """Sum of odd numbers from 1 to n"""
    total = 0
    for i in range(1, n + 1):
        if i % 2 != 0:
            total += i
    return (total,)


assert sum_odd_n(5) == (9,)
assert sum_odd_n(10) == (25,)
```

## `sum_of_digits`

```python
def sum_of_digits(n):
    """Sum of the digits of a number"""
    total = 0
    n = abs(n)
    while n > 0:
        total += n % 10
        n //= 10
    return (total,)


assert sum_of_digits(123) == (6,)
assert sum_of_digits(-456) == (15,)
```

## `is_prime`

```python
def is_prime(n):
    """Check if a number is prime"""
    if n <= 1:
        return (False,)
    for i in range(2, int(n**0.5) + 1):
        if n % i == 0:
            return (False,)
    return (True,)


assert is_prime(5) == (True,)
assert is_prime(4) == (False,)
assert is_prime(7) == (True,)
assert is_prime(8) == (False,)
```

## `count_divisors`

```python
def count_divisors(n):
    """Count the number of divisors of a number"""
    count = 0
    for i in range(1, n + 1):
        if n % i == 0:
            count += 1
    return (count,)


assert count_divisors(6) == (4,)
assert count_divisors(10) == (4,)
```

## `gcd`

```python
def gcd(a, b):
    """Find the greatest common divisor (GCD)"""
    while b != 0:
        a, b = b, a % b
    return (abs(a),)


assert gcd(48, 18) == (6,)
assert gcd(56, 98) == (14,)
```

## `count_ones`

```python
def count_ones(n):
    """Count the number of ones in the binary representation of a number"""
    count = 0
    while n > 0:
        count += n & 1
        n >>= 1
    return (count,)


assert count_ones(5) == (2,)
assert count_ones(7) == (3,)
```

## `reverse_bits`

```python
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


assert reverse_bits(1) == (-2147483648,)
assert reverse_bits(2) == (1073741824,)
```

## `hello_user_pstr`

```python
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


assert hello_user_pstr('Alice\n') == ('What is your name?\nHello, Alice!\n', '')
assert hello_user_pstr('Alice\nBob') == ('What is your name?\nHello, Alice!\n', 'Bob')
```

## `hello_user_cstr`

```python
def hello_user_cstr(input):
    """Greet the user with C strings.

    External behavior is the same as hello_user_pstr.
    """
    return hello_user_pstr(input)


assert hello_user_cstr('Alice\n') == ('What is your name?\nHello, Alice!\n', '')
assert hello_user_cstr('Alice\nBob') == ('What is your name?\nHello, Alice!\n', 'Bob')
```

## `factorial`

**Example. Not a variant.**

```python
def factorial(x):
    def factorial_inner(n):
        return 1 if n == 0 else n * factorial_inner(n - 1)

    return (factorial_inner(x),)


assert factorial(0) == (1,)
assert factorial(5) == (120,)
assert factorial(6) == (720,)
assert factorial(7) == (5040,)
assert factorial(8) == (40320,)
assert factorial(9) == (362880,)
```

## `logical_not`

**Example. Not a variant.**

```python
def logical_not(x):
    return (not x,)


assert logical_not(True) == (False,)
assert logical_not(False) == (True,)
```

## `hello`

**Example. Not a variant.**

```python
def hello():
    return ("Hello\n\0World!",)


assert hello() == ('Hello\n\0World!',)
```

## `get_put_char`

**Example. Not a variant.**

```python
def get_put_char(input):
    return (input[0], input[1:])


assert get_put_char('A') == ('A', '')
assert get_put_char('B') == ('B', '')
assert get_put_char('C') == ('C', '')
assert get_put_char('ABCD') == ('A', 'BCD')
assert get_put_char('\0') == ('\0', '')
assert get_put_char('\n') == ('\n', '')
```
