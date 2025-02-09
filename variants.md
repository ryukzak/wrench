# Wrench variants

Variants described as a Python function with several asserts. It is a
limit implementation because your variant may have additional
requirements like: specific string representation, limit integer
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
- String Manipulation
    - [capital_case_cstr](#capital_case_cstr)
    - [capital_case_pstr](#capital_case_pstr)
    - [hello_user_cstr](#hello_user_cstr)
    - [hello_user_pstr](#hello_user_pstr)
    - [reverse_string_cstr](#reverse_string_cstr)
    - [reverse_string_pstr](#reverse_string_pstr)
    - [upper_case_cstr](#upper_case_cstr)
    - [upper_case_pstr](#upper_case_pstr)
- _Examples
    - [factorial](#factorial)
    - [get_put_char](#get_put_char)
    - [hello](#hello)
    - [logical_not](#logical_not)

## Bitwise Operations

### `big_to_little_endian`

```python
def big_to_little_endian(n):
    return int.from_bytes(n.to_bytes(4, byteorder="big"), byteorder="little")


assert big_to_little_endian(2018915346) == 305419896
assert big_to_little_endian(3721182122) == 2864434397
```

### `count_leading_zeros`

```python
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
    """Count the number of trailing zeros in the binary representation of an integer"""
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
    """Count the number of zero in the binary representation of a number"""
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
    """Check if the 32-bit binary representation of a number is a palindrome"""
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
    """Sum of even numbers from 1 to n"""
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
    """Sum of numbers from 1 to n"""
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
    """Sum of odd numbers from 1 to n"""
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
    """Sum of the digits of a number"""
    total = 0
    n = abs(n)
    while n > 0:
        total += n % 10
        n //= 10
    return total


assert sum_of_digits(123) == 6
assert sum_of_digits(-456) == 15
```

## String Manipulation

### `capital_case_cstr`

```python
def capital_case_cstr(s):
    """Convert the first character of each word in a C string to upper case"""
    return capital_case_pstr(s)


assert capital_case_cstr('hello world') == ('Hello World', '')
assert capital_case_cstr('python programming') == ('Python Programming', '')
```

### `capital_case_pstr`

```python
def capital_case_pstr(s):
    """Convert the first character of each word in a Pascal string to upper case"""
    return (s.title(), "")


assert capital_case_pstr('hello world') == ('Hello World', '')
assert capital_case_pstr('python programming') == ('Python Programming', '')
```

### `hello_user_cstr`

```python
def hello_user_cstr(input):
    """Greet the user with C strings.

    External behavior is the same as hello_user_pstr.
    """
    return hello_user_pstr(input)


assert hello_user_cstr('Alice\n') == ('What is your name?\nHello, Alice!\n', '')
assert hello_user_cstr('Alice\nBob') == ('What is your name?\nHello, Alice!\n', 'Bob')
```

### `hello_user_pstr`

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

### `reverse_string_cstr`

```python
def reverse_string_cstr(s):
    """Reverse a C string"""
    return reverse_string_pstr(s)


assert reverse_string_cstr('hello') == ('olleh', '')
assert reverse_string_cstr('world') == ('dlrow', '')
```

### `reverse_string_pstr`

```python
def reverse_string_pstr(s):
    """Reverse a Pascal string"""
    return (s[::-1], "")


assert reverse_string_pstr('hello') == ('olleh', '')
assert reverse_string_pstr('world') == ('dlrow', '')
```

### `upper_case_cstr`

```python
def upper_case_cstr(s):
    """Convert a C string to upper case"""
    return upper_case_pstr(s)


assert upper_case_cstr('hello') == ('HELLO', '')
assert upper_case_cstr('world') == ('WORLD', '')
```

### `upper_case_pstr`

```python
def upper_case_pstr(s):
    """Convert a Pascal string to upper case"""
    return (s.upper(), "")


assert upper_case_pstr('hello') == ('HELLO', '')
assert upper_case_pstr('world') == ('WORLD', '')
```

## _Examples

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
    return (symbols[0:1], symbols[1:])


assert get_put_char('A') == ('A', '')
assert get_put_char('B') == ('B', '')
assert get_put_char('C') == ('C', '')
assert get_put_char('ABCD') == ('A', 'BCD')
```

### `hello`

```python
def hello(_):
    return ("Hello\n\0World!", "")


assert hello('') == ('Hello\n\0World!', '')
# and mem[0..16]: 48 65 6c 6c 6f 0a 00 57 6f 72 6c 64 21 00 00 00 00
```

### `logical_not`

```python
def logical_not(x):
    return not x


assert logical_not(True) == False
assert logical_not(False) == True
```
