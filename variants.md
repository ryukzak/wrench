# Wrench variants

Variants described as a Python function with several asserts. It is a
limited implementation because your variant may have additional
requirements like: specific string representation, limited integer
number representation, etc.

Additional requirements for all variants:

1. If the input does not match the domain -- return `-1`.
1. If the result cannot be correctly calculated (the result cannot be
   represented within the machine word) -- return the result filled with
   bytes with the value `0xCC`.
1. The input should be passed through memory cell `0x80`.
1. The output should be passed to memory cell `0x84`.
1. The input value and the result by default -- a 32-bit machine word
    unless otherwise specified.
1. ISA-specific requirements:
    - `F32a`: use procedures.

Also we have the following helper functions not from builtins:

```python
def read_line(s, buf_size):
    """Read line from input with buffer size limits."""
    assert "\n" in s, "input should have a newline character"
    line = "".join(itertools.takewhile(lambda x: x != "\n", s))

    if len(line) > buf_size - 1:
        return None, s[buf_size:]

    return line, s[len(line) + 1 :]


def cstr(s, buf_size):
    """Make content for buffer with pascal string (default value for cell: `_`)."""
    assert len(s) + 1 <= buf_size
    buf = s + "\0" + ("_" * (buf_size - len(s) - 1))
    return "".join(itertools.takewhile(lambda c: c != "\0", s)), buf


def pstr(s, buf_size):
    """Make content for buffer with pascal string (default value for cell: `_`)."""
    assert len(s) + 1 <= buf_size
    buf = chr(len(s)) + s + ("_" * (buf_size - len(s) - 1))
    return s, buf


def cbuf(s, buf_size):
    return cstr(s, buf_size)[1]


def pbuf(s, buf_size):
    return pstr(s, buf_size)[1]
```

Variants:

- Bitwise Operations
    - [big_to_little_endian](#big_to_little_endian)
    - [count_leading_zeros](#count_leading_zeros)
    - [count_ones](#count_ones)
    - [count_trailing_zeros](#count_trailing_zeros)
    - [count_zero](#count_zero)
    - [is_binary_palindrome](#is_binary_palindrome)
    - [little_to_big_endian](#little_to_big_endian)
    - [reverse_bits](#reverse_bits)
- Mathematics
    - [count_divisors](#count_divisors)
    - [fibonacci](#fibonacci)
    - [gcd](#gcd)
    - [is_prime](#is_prime)
    - [sum_even_n](#sum_even_n)
    - [sum_n](#sum_n)
    - [sum_odd_n](#sum_odd_n)
    - [sum_of_digits](#sum_of_digits)
    - [sum_word_cstream](#sum_word_cstream)
    - [sum_word_pstream](#sum_word_pstream)
- String Manipulation
    - [capital_case_cstr](#capital_case_cstr)
    - [capital_case_pstr](#capital_case_pstr)
    - [hello_user_cstr](#hello_user_cstr)
    - [hello_user_pstr](#hello_user_pstr)
    - [reverse_string_cstr](#reverse_string_cstr)
    - [reverse_string_pstr](#reverse_string_pstr)
    - [upper_case_cstr](#upper_case_cstr)
    - [upper_case_pstr](#upper_case_pstr)
- _Examples_
    - [dup](#dup)
    - [factorial](#factorial)
    - [get_put_char](#get_put_char)
    - [hello](#hello)
    - [logical_not](#logical_not)

## Bitwise Operations

### `big_to_little_endian`

```python
def big_to_little_endian(n):
    """Convert a 32-bit integer from big-endian to little-endian format"""
    return int.from_bytes(n.to_bytes(4, byteorder="big"), byteorder="little")


assert big_to_little_endian(2018915346) == 305419896
assert big_to_little_endian(3721182122) == 2864434397
```

### `count_leading_zeros`

```python
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


assert count_leading_zeros(1) == 31
assert count_leading_zeros(2) == 30
assert count_leading_zeros(16) == 27
```

### `count_ones`

```python
def count_ones(n):
    """Count the number of ones in the binary representation of a number"""
    count = 0
    while n > 0:
        count += n & 1
        n >>= 1
    return count


assert count_ones(5) == 2
assert count_ones(7) == 3
assert count_ones(247923789) == 13
assert count_ones(2147483647) == 31
```

### `count_trailing_zeros`

```python
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


assert count_trailing_zeros(1) == 0
assert count_trailing_zeros(2) == 1
assert count_trailing_zeros(16) == 4
```

### `count_zero`

```python
def count_zero(n):
    """Count the number of zeros in the binary representation of a number"""
    count = 0
    for _ in range(32):
        count += 0 if n & 1 else 1
        n >>= 1
    return count


assert count_zero(5) == 30
assert count_zero(7) == 29
assert count_zero(247923789) == 19
```

### `is_binary_palindrome`

```python
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


assert is_binary_palindrome(5) == 0
assert is_binary_palindrome(15) == 0
assert is_binary_palindrome(4026531855) == 1
assert is_binary_palindrome(3221225474) == 0
```

### `little_to_big_endian`

```python
def little_to_big_endian(n):
    """Convert a 32-bit integer from little-endian to big-endian format"""
    return int.from_bytes(n.to_bytes(4, byteorder="little"), byteorder="big")


assert little_to_big_endian(305419896) == 2018915346
assert little_to_big_endian(2864434397) == 3721182122
```

### `reverse_bits`

```python
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


assert reverse_bits(1) == -2147483648
assert reverse_bits(2) == 1073741824
```

## Mathematics

### `count_divisors`

```python
def count_divisors(n):
    """Count the number of divisors of a natural number"""
    if n < 1:
        return -1
    count = 0
    for i in range(1, n + 1):
        if n % i == 0:
            count += 1
    return count


assert count_divisors(2) == 2
assert count_divisors(4) == 3
assert count_divisors(6) == 4
assert count_divisors(10) == 4
```

### `fibonacci`

```python
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


assert fibonacci(0) == 0
assert fibonacci(1) == 1
assert fibonacci(2) == 1
assert fibonacci(3) == 2
assert fibonacci(4) == 3
assert fibonacci(5) == 5
assert fibonacci(25) == 75025
```

### `gcd`

```python
def gcd(a, b):
    """Find the greatest common divisor (GCD)"""
    while b != 0:
        a, b = b, a % b
    return [abs(a)]


assert gcd([48, 18]) == [6]
assert gcd([56, 98]) == [14]
```

### `is_prime`

```python
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


assert is_prime(5) == 1
assert is_prime(4) == 0
assert is_prime(7) == 1
assert is_prime(8) == 0
assert is_prime(283) == 1
assert is_prime(284) == 0
assert is_prime(293) == 1
```

### `sum_even_n`

```python
def sum_even_n(n):
    """Calculate the sum of even numbers from 1 to n"""
    if n <= 0:
        return -1
    total = 0
    for i in range(1, n + 1):
        if i % 2 == 0:
            total += i
    return total


assert sum_even_n(5) == 6
assert sum_even_n(10) == 30
assert sum_even_n(90000) == 2025045000
```

### `sum_n`

```python
def sum_n(n):
    """Calculate the sum of numbers from 1 to n"""
    if n <= 0:
        return -1
    total = 0
    for i in range(1, n + 1):
        total += i
    return total


assert sum_n(5) == 15
assert sum_n(10) == 55
```

### `sum_odd_n`

```python
def sum_odd_n(n):
    """Calculate the sum of odd numbers from 1 to n"""
    if n <= 0:
        return -1
    total = 0
    for i in range(1, n + 1):
        if i % 2 != 0:
            total += i
    return total


assert sum_odd_n(5) == 9
assert sum_odd_n(10) == 25
assert sum_odd_n(90000) == 2025000000
```

### `sum_of_digits`

```python
def sum_of_digits(n):
    """Calculate the sum of the digits of a number"""
    total = 0
    n = abs(n)
    while n > 0:
        total += n % 10
        n //= 10
    return total


assert sum_of_digits(123) == 6
assert sum_of_digits(-456) == 15
```

### `sum_word_cstream`

```python
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


assert sum_word_cstream([48, 18, 0]) == [0, 66]
assert sum_word_cstream([1, 0]) == [0, 1]
assert sum_word_cstream([48, 18, 0, 12, 0]) == [0, 66]
assert sum_word_cstream([1, 0]) == [0, 1]
assert sum_word_cstream([2147483647, 1, 0]) == [0, 2147483648]
assert sum_word_cstream([2147483647, 1, 2147483647, 0]) == [0, 4294967295]
assert sum_word_cstream([2147483647, 1, 2147483647, 1, 0]) == [1, 0]
assert sum_word_cstream([2147483647, 1, 2147483647, 2, 0]) == [1, 1]
```

### `sum_word_pstream`

```python
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


assert sum_word_pstream([2, 48, 18]) == [0, 66]
assert sum_word_pstream([1, 1]) == [0, 1]
assert sum_word_pstream([2, 48, 18, 0, 12]) == [0, 66]
assert sum_word_pstream([2, 48, 18, 12]) == [0, 66]
assert sum_word_pstream([2, 2147483647, 1, 0]) == [0, 2147483648]
assert sum_word_pstream([3, 2147483647, 1, 2147483647, 0]) == [0, 4294967295]
assert sum_word_pstream([4, 2147483647, 1, 2147483647, 1, 0]) == [1, 0]
assert sum_word_pstream([4, 2147483647, 1, 2147483647, 2, 0]) == [1, 1]
assert sum_word_pstream([2, 1, -1]) == [0, 0]
```

## String Manipulation

### `capital_case_cstr`

```python
def capital_case_cstr(s):
    """Convert the first character of each word in a C string to capital case.

    Capital Case Is Something Like This.

    - Result string should be represented as a correct C string.
    - Buffer size for the message -- `0x20`, starts from `0x00`.
    - End of input -- new line.
    - Initial buffer values -- `_`.

    Python example args:
        s (str): The input string till new line.

    Returns:
        tuple: A tuple containing the capitalized output string and input rest.
    """
    line, rest = read_line(s, 0x20)
    if line is None:
        return [overflow_error_value], rest
    return (cstr(line.title(), 0x20)[0]), rest


assert capital_case_cstr('hello world\n') == ('Hello World', '')
# and mem[0..31]: 48 65 6c 6c 6f 20 57 6f 72 6c 64 00 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f
assert capital_case_cstr('python programming\n') == ('Python Programming', '')
# and mem[0..31]: 50 79 74 68 6f 6e 20 50 72 6f 67 72 61 6d 6d 69 6e 67 00 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f
```

### `capital_case_pstr`

```python
def capital_case_pstr(s):
    """Convert the first character of each word in a Pascal string to capital case.

    Capital Case Is Something Like This.

    - Result string should be represented as a correct Pascal string.
    - Buffer size for the message -- `0x20`, starts from `0x00`.
    - End of input -- new line.
    - Initial buffer values -- `_`.

    Python example args:
        s (str): The input string till new line.

    Returns:
        tuple: A tuple containing the capitalized output string and input rest.
    """
    line, rest = read_line(s, 0x20)
    if line is None:
        return [overflow_error_value], rest
    return line.title(), rest


assert capital_case_pstr('hello world\n') == ('Hello World', '')
# and mem[0..31]: 0b 48 65 6c 6c 6f 20 57 6f 72 6c 64 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f
assert capital_case_pstr('python programming\n') == ('Python Programming', '')
# and mem[0..31]: 12 50 79 74 68 6f 6e 20 50 72 6f 67 72 61 6d 6d 69 6e 67 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f
```

### `hello_user_cstr`

```python
def hello_user_cstr(input):
    """Greet the user with C string: ask the name and greet by `Hello, <name>!` message.

    - Result string with greet message should be represented as a correct C string.
    - Buffer size for the message -- `0x20`, starts from `0x00`.
    - End of input -- new line.
    - Initial buffer values -- `_`.

    Python example args:
        input (str): The input string containing the user's name.

    Returns:
        tuple: A tuple containing the greeting message and the remaining input.
    """
    line, rest = read_line(input, 0x20 - len("Hello, " + "!") - 1)

    q = "What is your name?\n"
    if not line:
        return [q, overflow_error_value], rest

    greet = "Hello, " + "".join(itertools.takewhile(lambda c: c != "\0", line)) + "!"
    return q + cstr(greet, 0x20)[0], rest


assert hello_user_cstr('Alice\n') == ('What is your name?\nHello, Alice!', '')
# and mem[0..31]: 48 65 6c 6c 6f 2c 20 41 6c 69 63 65 21 00 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f
assert hello_user_cstr('Bob\n') == ('What is your name?\nHello, Bob!', '')
# and mem[0..31]: 48 65 6c 6c 6f 2c 20 42 6f 62 21 00 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f
```

### `hello_user_pstr`

```python
def hello_user_pstr(input):
    """Greet the user with Pascal string: ask the name and greet by `Hello, <name>!` message.

    - Result string with greet message should be represented as a correct Pascal string.
    - Buffer size for the message -- `0x20`, starts from `0x00`.
    - End of input -- new line.
    - Initial buffer values -- `_`.

    Python example args:
        input (str): The input string containing the user's name.

    Returns:
        tuple: A tuple containing the greeting message and the remaining input.
    """
    line, rest = read_line(input, 0x20 - len("Hello, " + "!") - 1)

    q = "What is your name?\n"
    if not line:
        return [q, overflow_error_value], rest

    greet = "Hello, " + line + "!"
    return q + greet, rest


assert hello_user_pstr('Alice\n') == ('What is your name?\nHello, Alice!', '')
# and mem[0..31]: 0d 48 65 6c 6c 6f 2c 20 41 6c 69 63 65 21 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f
assert hello_user_pstr('Bob\n') == ('What is your name?\nHello, Bob!', '')
# and mem[0..31]: 0b 48 65 6c 6c 6f 2c 20 42 6f 62 21 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f
```

### `reverse_string_cstr`

```python
def reverse_string_cstr(s):
    """Reverse a C string.

    - Result string should be represented as a correct C string.
    - Buffer size for the message -- `0x20`, starts from `0x00`.
    - End of input -- new line.
    - Initial buffer values -- `_`.

    Python example args:
        s (str): The input C string.

    Returns:
        tuple: A tuple containing the reversed string and an empty string.
    """
    line, rest = read_line(s, 0x20)
    if line is None:
        return [overflow_error_value], rest
    return cstr(line[::-1], 0x20)[0], rest


assert reverse_string_cstr('hello\n') == ('olleh', '')
# and mem[0..31]: 6f 6c 6c 65 68 00 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f
assert reverse_string_cstr('world!\n') == ('!dlrow', '')
# and mem[0..31]: 21 64 6c 72 6f 77 00 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f
```

### `reverse_string_pstr`

```python
def reverse_string_pstr(s):
    """Reverse a Pascal string.

    - Result string should be represented as a correct Pascal string.
    - Buffer size for the message -- `0x20`, starts from `0x00`.
    - End of input -- new line.
    - Initial buffer values -- `_`.

    Python example args:
        s (str): The input Pascal string.

    Returns:
        tuple: A tuple containing the reversed string and an empty string.
    """
    line, rest = read_line(s, 0x20)
    if line is None:
        return [overflow_error_value], rest
    return line[::-1], rest


assert reverse_string_pstr('hello\n') == ('olleh', '')
# and mem[0..31]: 05 6f 6c 6c 65 68 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f
assert reverse_string_pstr('world!\n') == ('!dlrow', '')
# and mem[0..31]: 06 21 64 6c 72 6f 77 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f
```

### `upper_case_cstr`

```python
def upper_case_cstr(s):
    """Convert a C string to upper case.

    - Result string should be represented as a correct C string.
    - Buffer size for the message -- `0x20`, starts from `0x00`.
    - End of input -- new line.
    - Initial buffer values -- `_`.

    Python example args:
        s (str): The input C string.

    Returns:
        tuple: A tuple containing the upper case string and an empty string.
    """
    line, rest = read_line(s, 0x20)
    if line is None:
        return [overflow_error_value], rest
    return cstr(line.upper(), 0x20)[0], rest


assert upper_case_cstr('Hello\n') == ('HELLO', '')
# and mem[0..31]: 48 45 4c 4c 4f 00 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f
assert upper_case_cstr('world\n') == ('WORLD', '')
# and mem[0..31]: 57 4f 52 4c 44 00 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f
```

### `upper_case_pstr`

```python
def upper_case_pstr(s):
    """Convert a Pascal string to upper case.

    - Result string should be represented as a correct Pascal string.
    - Buffer size for the message -- `0x20`, starts from `0x00`.
    - End of input -- new line.
    - Initial buffer values -- `_`.

    Python example args:
        s (str): The input string.

    Returns:
        tuple: A tuple containing the upper case string and an empty string.
    """
    line, rest = read_line(s, 0x20)
    if line is None:
        return [overflow_error_value], rest
    return line.upper(), rest


assert upper_case_pstr('Hello\n') == ('HELLO', '')
# and mem[0..31]: 05 48 45 4c 4c 4f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f
assert upper_case_pstr('world\n') == ('WORLD', '')
# and mem[0..31]: 05 57 4f 52 4c 44 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f
```

## _Examples_

### `dup`

```python
def dup(x):
    return [x, x]


assert dup([42]) == [42, 42]
```

### `factorial`

```python
def factorial(x):
    def factorial_inner(n):
        return 1 if n == 0 else n * factorial_inner(n - 1)

    return factorial_inner(x)


assert factorial(0) == 1
assert factorial(5) == 120
assert factorial(6) == 720
assert factorial(7) == 5040
assert factorial(8) == 40320
assert factorial(9) == 362880
```

### `get_put_char`

```python
def get_put_char(symbols):
    """On X -- return -1 (word). On Y -- return 0xCCCCCCCC"""
    char = symbols[0]
    if char == "X":
        return [-1], symbols[1:]
    elif char == "Y":
        return [overflow_error_value], symbols[1:]
    return (str(char), symbols[1:])


assert get_put_char('A') == ('A', '')
assert get_put_char('B') == ('B', '')
assert get_put_char('C') == ('C', '')
assert get_put_char('ABCD') == ('A', 'BCD')
```

### `hello`

```python
def hello(_):
    return ("\x1fHello\n\0World!", "")


assert hello('') == ('\x1fHello\n\0World!', '')
# and mem[0..16]: 1f 48 65 6c 6c 6f 0a 00 57 6f 72 6c 64 21 00 00 00
```

### `logical_not`

```python
def logical_not(x):
    return not x


assert logical_not(True) == False
assert logical_not(False) == True
```
