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
1. Source code should be properly formatted (manually or using `wrench-fmt`).
1. Execution log should not be truncated (use configuration with understanding).
1. ISA-specific requirements:
    - `F32a`: use procedures.
    - `RISC-IV`: use nested procedures and stack. Where applicable -- recursive solutions are recommended.
    - `M68k`: use different instruction modes and addressing modes. Use nested procedures and stack.
1. When using procedures, develop a label naming convention that helps visualize code structure.

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
    """Make content for buffer with C string (default value for cell: `_`)."""
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
    - [hamming_distance](#hamming_distance)
    - [is_binary_palindrome](#is_binary_palindrome)
    - [little_to_big_endian](#little_to_big_endian)
    - [next_power_of_two](#next_power_of_two)
    - [parity](#parity)
    - [reverse_bits](#reverse_bits)
    - [rotate_left](#rotate_left)
    - [rotate_right](#rotate_right)
- Complex Tasks
    - [base64_decoding](#base64_decoding)
    - [base64_encoding](#base64_encoding)
    - [bracket_validator](#bracket_validator)
    - [brainfuck_interpreter](#brainfuck_interpreter)
    - [char_frequency](#char_frequency)
    - [format_string](#format_string)
    - [glob_match](#glob_match)
    - [infix_to_rpn](#infix_to_rpn)
    - [reverse_words_cstr](#reverse_words_cstr)
    - [rle_compress](#rle_compress)
    - [rle_compress_bytes](#rle_compress_bytes)
    - [rle_decompress](#rle_decompress)
    - [rle_decompress_bytes](#rle_decompress_bytes)
    - [stack_based_calculator](#stack_based_calculator)
    - [text_word_counter](#text_word_counter)
- Mathematics
    - [collatz_length](#collatz_length)
    - [count_divisors](#count_divisors)
    - [fibonacci](#fibonacci)
    - [gcd_many](#gcd_many)
    - [integer_sqrt](#integer_sqrt)
    - [is_prime](#is_prime)
    - [lcm](#lcm)
    - [power](#power)
    - [power_many](#power_many)
    - [sum_even_n](#sum_even_n)
    - [sum_n](#sum_n)
    - [sum_odd_n](#sum_odd_n)
    - [sum_of_digits](#sum_of_digits)
    - [sum_word_cstream](#sum_word_cstream)
    - [sum_word_pstream](#sum_word_pstream)
- String Manipulation
    - [caesar_cipher](#caesar_cipher)
    - [capital_case_cstr](#capital_case_cstr)
    - [capital_case_pstr](#capital_case_pstr)
    - [hello_user_cstr](#hello_user_cstr)
    - [hello_user_pstr](#hello_user_pstr)
    - [lower_case_cstr](#lower_case_cstr)
    - [lower_case_pstr](#lower_case_pstr)
    - [reverse_string_cstr](#reverse_string_cstr)
    - [reverse_string_pstr](#reverse_string_pstr)
    - [strstr_cstr](#strstr_cstr)
    - [upper_case_cstr](#upper_case_cstr)
    - [upper_case_pstr](#upper_case_pstr)
- VLIW
    - [affine2d_transform](#affine2d_transform)
    - [complex_multiply](#complex_multiply)
    - [determinant_2x2_stream](#determinant_2x2_stream)
    - [determinant_3x3](#determinant_3x3)
    - [djb2_hash](#djb2_hash)
    - [fnv32_1_hash](#fnv32_1_hash)
    - [fnv32_1a_hash](#fnv32_1a_hash)
    - [four_lane_mac](#four_lane_mac)
    - [linear_filter](#linear_filter)
    - [matrix_2x2_vector_stream](#matrix_2x2_vector_stream)
    - [min_max_sum](#min_max_sum)
    - [pairwise_add_sub](#pairwise_add_sub)
    - [rgb_to_grayscale](#rgb_to_grayscale)
    - [sdbm_hash](#sdbm_hash)
    - [sum_and_sum_squares](#sum_and_sum_squares)
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

### `hamming_distance`

```python
def hamming_distance(a, b):
    """Count the number of differing bits between two 32-bit integers.

    The Hamming distance is the number of set bits in (a XOR b).

    Args:
        a (int): First 32-bit integer.
        b (int): Second 32-bit integer.

    Returns:
        list: A one-element list containing the Hamming distance.
    """
    diff = (a ^ b) & 0xFFFFFFFF
    return [diff.bit_count()]


assert hamming_distance(0, 0) == [0]
assert hamming_distance(0, 1) == [1]
assert hamming_distance(4294967295, 0) == [32]
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

### `next_power_of_two`

```python
def next_power_of_two(n):
    """Return the smallest power of two greater than or equal to n.

    Args:
        n (int): A non-negative integer.

    Returns:
        list: A one-element list containing the next power of two.

    Special cases:
        n < 0: return -1.
        n == 0: return 1.
        Result greater than INT32_MAX: return overflow_error_value.
    """
    if n < 0:
        return [-1]

    if n <= 1:
        return [1]

    result = 1
    while result < n:
        result <<= 1
        if result > max_int32:
            return [overflow_error_value]

    return [result]


assert next_power_of_two(0) == [1]
assert next_power_of_two(1) == [1]
assert next_power_of_two(5) == [8]
```

### `parity`

```python
def parity(n):
    """Compute bit parity of a 32-bit integer.

    Returns 1 if the number of set bits is odd, 0 if even.

    Args:
        n (int): The 32-bit integer.

    Returns:
        int: 1 for odd parity, 0 if even parity.
    """
    count = (n & 0xFFFFFFFF).bit_count()
    return count % 2


assert parity(0) == 0
assert parity(1) == 1
assert parity(3) == 0
assert parity(7) == 1
assert parity(255) == 0
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

### `rotate_left`

```python
def rotate_left(val, n):
    """Rotate a 32-bit integer to the left by n bits.

    Bits that are shifted out from the left side are wrapped
    around and placed back on the right side. The rotation amount
    is taken modulo 32, so rotating by 32 bits leaves the value unchanged.

    Args:
        val (int): The 32-bit integer to rotate.
        n (int): Number of bits to rotate left.

    Returns:
        list: A one-element list containing the rotated 32-bit value.
    """
    val32 = val & 0xFFFFFFFF
    shift = n & 0x1F
    if shift == 0:
        return [uint32_to_int32(val32)]
    result = ((val32 << shift) | (val32 >> (32 - shift))) & 0xFFFFFFFF
    return [uint32_to_int32(result)]


assert rotate_left(1, 1) == [2]
assert rotate_left(305419896, 4) == [591751041]
assert rotate_left(1, 0) == [1]
```

### `rotate_right`

```python
def rotate_right(val, n):
    """Rotate a 32-bit integer to the right by n bits.

    Bits that are shifted out from the right side are wrapped
    around and placed back on the left side. The rotation amount
    is taken modulo 32, so rotating by 32 bits leaves the value unchanged.

    Args:
        val (int): The 32-bit integer to rotate.
        n (int): Number of bits to rotate right.

    Returns:
        list: A one-element list containing the rotated 32-bit value.
    """
    val32 = val & 0xFFFFFFFF
    shift = n & 0x1F
    if shift == 0:
        return [uint32_to_int32(val32)]
    result = ((val32 >> shift) | (val32 << (32 - shift))) & 0xFFFFFFFF
    return [uint32_to_int32(result)]


assert rotate_right(2, 1) == [1]
assert rotate_right(305419896, 4) == [-2128394905]
assert rotate_right(1, 0) == [1]
```

## Complex Tasks

### `base64_decoding`

```python
def base64_decoding(input):
    """Decode base64 input string.

    - Result string should be represented as a correct C string.
    - Buffer size for the decoded message -- `0x40`, starts from `0x00`.
    - End of input -- new line.

    Python example args:
        input (str): The input string containing base64 data to decode.

    Returns:
        tuple: A tuple containing the base64 decoded string and the remaining input.
    """
    line, rest = read_line(input, 0x40)
    if line is None:
        return [overflow_error_value], rest

    try:
        decoded_str = base64.b64decode(line).decode("utf-8")

        if len(decoded_str) + 1 > 0x40:  # +1 for null terminator
            return [overflow_error_value], rest

        return cstr(decoded_str, 0x40)[0], rest
    except Exception:
        # Invalid base64 input
        return [-1], rest


assert base64_decoding('SGVsbG8gd29ybGQh\n') == ('Hello world!', '')
assert base64_decoding('UHl0aG9u\n') == ('Python', '')
```

### `base64_encoding`

```python
def base64_encoding(input):
    """Encode input string to base64.

    - Result string should be represented as a correct C string.
    - Buffer size for the encoded message -- `0x40`, starts from `0x00`.
    - End of input -- new line.

    Python example args:
        input (str): The input string containing data to encode.

    Returns:
        tuple: A tuple containing the base64 encoded string and the remaining input.
    """
    line, rest = read_line(input, 0x40)
    if line is None:
        return [overflow_error_value], rest

    encoded_bytes = base64.b64encode(line.encode("utf-8"))
    encoded_str = encoded_bytes.decode("ascii")

    if len(encoded_str) + 1 > 0x40:  # +1 for null terminator
        return [overflow_error_value], rest

    return cstr(encoded_str, 0x40)[0], rest


assert base64_encoding('Hello!\n') == ('SGVsbG8h', '')
```

### `bracket_validator`

```python
def bracket_validator(input):
    """Validate (), [], and {} brackets in a line.

    - Brackets must be properly nested and matched.
    - Other characters are ignored.
    - An empty line is valid.
    - Returns 1 for valid brackets and -1 for invalid brackets.
    - End of input -- new line.

    Returns:
        tuple: A tuple containing the validation result and remaining input.
    """
    line, rest = read_line(input, 0x40)

    if line is None:
        return [overflow_error_value], rest

    try:
        stack = []

        pairs = {
            ")": "(",
            "]": "[",
            "}": "{",
        }

        for char in line:
            if char in "([{":
                stack.append(char)

            elif char in ")]}":
                if not stack or stack[-1] != pairs[char]:
                    return [-1], rest

                stack.pop()

        if stack:
            return [-1], rest

        return [1], rest

    except Exception:
        return [-1], rest


assert bracket_validator('([]{})\n') == ([1], '')
assert bracket_validator('([{}])\n') == ([1], '')
assert bracket_validator('([)]\n') == ([-1], '')
```

### `brainfuck_interpreter`

```python
def brainfuck_interpreter(input):
    """Brainfuck interpreter with 8 commands: ><+-.,[]

    Commands:
    - > : increment data pointer
    - < : decrement data pointer
    - + : increment 32-bit value at data pointer
    - - : decrement 32-bit value at data pointer
    - . : output low byte of 32-bit value at data pointer
    - , : input byte to low byte of 32-bit value at data pointer
    - [ : jump forward after matching ] if value at data pointer is 0
    - ] : jump back after matching [ if value at data pointer is not 0

    - Memory: 30 cells, each 32-bit signed integer, initially 0
    - Data pointer starts at 0
    - End of input -- new line
    - On error (invalid command, pointer out of bounds) return -1
    - Input comes from remaining characters after newline

    Python example args:
        input (str): The input string containing brainfuck code and input data.

    Returns:
        tuple: A tuple containing the output string and the remaining input.
    """
    line, rest = read_line(input, 0x40)
    if line is None:
        return [overflow_error_value], rest

    try:
        # Initialize Brainfuck state
        memory = [0] * 30  # 30 cells of 32-bit values
        data_ptr = 0
        code_ptr = 0
        output = []
        input_data = rest
        input_ptr = 0

        code = line

        # Validate bracket matching first
        bracket_count = 0
        for c in code:
            if c == "[":
                bracket_count += 1
            elif c == "]":
                bracket_count -= 1
                if bracket_count < 0:
                    return [-1], rest  # Unmatched closing bracket
        if bracket_count != 0:
            return [-1], rest  # Unmatched opening bracket

        while code_ptr < len(code):
            cmd = code[code_ptr]

            if cmd == ">":
                data_ptr += 1
                if data_ptr >= 30:
                    return [-1], rest
            elif cmd == "<":
                data_ptr -= 1
                if data_ptr < 0:
                    return [-1], rest
            elif cmd == "+":
                memory[data_ptr] = memory[data_ptr] + 1
                # Check for 32-bit overflow
                if memory[data_ptr] > 2147483647:
                    return [overflow_error_value], rest
            elif cmd == "-":
                memory[data_ptr] = memory[data_ptr] - 1
                # Check for 32-bit underflow
                if memory[data_ptr] < -2147483648:
                    return [overflow_error_value], rest
            elif cmd == ".":
                # Output low byte of 32-bit value
                byte_val = memory[data_ptr] & 0xFF
                output.append(chr(byte_val))
            elif cmd == ",":
                if input_ptr < len(input_data):
                    # Set low byte, keep high bits
                    memory[data_ptr] = (memory[data_ptr] & 0xFFFFFF00) | ord(
                        input_data[input_ptr]
                    )
                    input_ptr += 1
                else:
                    memory[data_ptr] = (
                        memory[data_ptr] & 0xFFFFFF00
                    )  # EOF sets low byte to 0
            elif cmd == "[":
                if memory[data_ptr] == 0:
                    # Jump forward to matching ]
                    bracket_count = 1
                    code_ptr += 1
                    while code_ptr < len(code) and bracket_count > 0:
                        if code[code_ptr] == "[":
                            bracket_count += 1
                        elif code[code_ptr] == "]":
                            bracket_count -= 1
                        code_ptr += 1
                    if bracket_count > 0:
                        return [-1], rest  # Unmatched opening bracket
                    code_ptr -= 1  # Adjust for the increment at end of loop
            elif cmd == "]":
                if memory[data_ptr] != 0:
                    # Jump back to matching [
                    bracket_count = 1
                    code_ptr -= 1
                    while code_ptr >= 0 and bracket_count > 0:
                        if code[code_ptr] == "]":
                            bracket_count += 1
                        elif code[code_ptr] == "[":
                            bracket_count -= 1
                        code_ptr -= 1
                    if bracket_count > 0:
                        return [-1], rest  # Unmatched closing bracket
                    code_ptr += 1  # Adjust for the increment at end of loop
            elif cmd in " \t\n\r":
                pass  # Ignore whitespace
            else:
                return [-1], rest  # Invalid command

            code_ptr += 1

        # Update rest to remove consumed input
        remaining_input = input_data[input_ptr:]

        return "".join(output), remaining_input

    except Exception:
        return [-1], rest


assert brainfuck_interpreter('++.\n') == ('\x02', '')
assert brainfuck_interpreter('++++++++++++++++++++++++++++++++++++++++++++++++++.\n') == ('2', '')
assert brainfuck_interpreter(',.\nA') == ('A', '')
assert brainfuck_interpreter('<\n') == ([-1], '')
```

### `char_frequency`

```python
def char_frequency(input):
    """Count occurrences of each character in a line.

    - Characters are counted in order of first appearance.
    - Spaces are counted as normal characters.
    - Maximum number of unique characters is 12.
    - Output format: "<char>:<count> ..."
    - Result must fit into a 0x40-byte C string.
    - End of input -- new line.

    Examples:
        "hello" -> "h:1 e:1 l:2 o:1"
        "aabbc" -> "a:2 b:2 c:1"

    Returns:
        tuple: A tuple containing the frequency string and remaining input.
    """
    line, rest = read_line(input, 0x40)

    if line is None:
        return [overflow_error_value], rest

    if not line:
        return "", rest

    try:
        order = []
        counts = {}

        for char in line:
            if char not in counts:
                if len(order) >= 12:
                    return [-1], rest

                order.append(char)
                counts[char] = 0

            counts[char] += 1

        parts = []

        for char in order:
            parts.append(f"{char}:{counts[char]}")

        result = " ".join(parts)

        if len(result) + 1 > 0x40:
            return [overflow_error_value], rest

        return cstr(result, 0x40)[0], rest

    except Exception:
        return [-1], rest


assert char_frequency('hello\n') == ('h:1 e:1 l:2 o:1', '')
assert char_frequency('aabbc\n') == ('a:2 b:2 c:1', '')
assert char_frequency('\n') == ('', '')
```

### `format_string`

```python
def format_string(input):
    """Format string with %d placeholders replaced by integers from input.

    Input format: "format_string\\nint1\\nint2\\n..."
    Examples:
    - "Foo %d bar %d\\n232\\n43\\n" -> "Foo 232 bar 43"
    - "%5d\\n42\\n" -> "   42" (right-aligned, 5 digits)
    - "%-5d\\n42\\n" -> "42   " (left-aligned, 5 digits)
    - "Just text\\n" -> "Just text" (no formatting)

    Format string input buffer size limit: 0x20 bytes
    Output: unlimited size

    Integer handling: Only accepts 32-bit signed integers (-2147483648 to 2147483647).
    Returns -1 if any integer is outside this range.

    Returns formatted string or error codes:
    - -1 for invalid input format or format string exceeds 0x20 bytes
    """
    try:
        lines = input.split("\n")
        if len(lines) < 1:
            return [-1], input

        format_str = lines[0]

        # Check format string buffer size limit (0x20 bytes)
        format_bytes = 0
        overflow_idx = None
        for idx, ch in enumerate(format_str):
            format_bytes += len(ch.encode("utf-8"))
            if format_bytes > 0x20:
                overflow_idx = idx
                break
        if overflow_idx is not None:
            remaining = input[overflow_idx + 1 :]
            return [-1], remaining

        # Find all format specifiers: %d, %5d, %-5d, etc.
        format_specs = []
        i = 0
        while i < len(format_str):
            if format_str[i] == "%":
                spec_start = i
                i += 1
                if i < len(format_str) and format_str[i] == "-":
                    i += 1
                while i < len(format_str) and format_str[i].isdigit():
                    i += 1
                if i < len(format_str) and format_str[i] == "d":
                    format_specs.append(format_str[spec_start : i + 1])
                    i += 1
                else:
                    i = spec_start + 1
            else:
                i += 1
        placeholder_count = len(format_specs)

        # Check if we have enough lines for the placeholders
        if placeholder_count > 0 and len(lines) < placeholder_count + 1:
            return [-1], input

        # Parse integers from remaining lines
        # Parse integers from remaining lines
        integers = []
        line_idx = 1
        for _ in range(placeholder_count):
            if line_idx >= len(lines):
                return [-1], input

            line = lines[line_idx]
            pos = 0
            sign = 1
            value = 0

            if pos < len(line) and line[pos] == "-":
                sign = -1
                pos += 1
            elif pos < len(line) and line[pos] == "+":
                pos += 1

            digit_start = pos

            while pos < len(line) and line[pos].isdigit():
                digit = ord(line[pos]) - ord("0")
                value = value * 10 + digit
                pos += 1

                # Check 32-bit boundary
                if sign == 1:
                    if value > 2147483647:
                        remaining = "\n".join([line[pos:]] + lines[line_idx + 1 :])
                        return [-1], remaining
                else:
                    if value > 2147483648:
                        remaining = "\n".join([line[pos:]] + lines[line_idx + 1 :])
                        return [-1], remaining

            if digit_start == pos:
                # Check if the line is empty (missing input) or invalid
                if pos < len(line):
                    # Non-empty invalid line - consume invalid character and return what's after
                    remaining = "\n".join([line[pos + 1 :]] + lines[line_idx + 1 :])
                else:
                    # Empty line - consume it and return what's after
                    remaining = (
                        "\n".join(lines[line_idx + 1 :])
                        if line_idx + 1 < len(lines)
                        else ""
                    )
                return [-1], remaining

            if pos < len(line):
                # Non-empty invalid line - consume invalid character and return what's after
                remaining = "\n".join([line[pos + 1 :]] + lines[line_idx + 1 :])
                return [-1], remaining

            parsed_int = sign * value
            integers.append(parsed_int)
            line_idx += 1

        # Format the string
        try:
            if placeholder_count == 0:
                result = format_str
            else:
                result = format_str % tuple(integers)
        except TypeError, ValueError:
            # Calculate remaining input
            remaining = "\n".join(lines[line_idx:]) if line_idx < len(lines) else ""
            return [-1], remaining

        # Calculate remaining input
        consumed_lines = line_idx
        if consumed_lines < len(lines):
            remaining = "\n".join(lines[consumed_lines:])
        else:
            remaining = ""

        return result, remaining

    except Exception:
        return [-1], input


assert format_string('Num: %d\n42\n') == ('Num: 42', '')
assert format_string('%5d\n42\n') == ('   42', '')
assert format_string('%-5d\n42\n') == ('42   ', '')
```

### `glob_match`

```python
def glob_match(input):
    """Match a text against a glob pattern.

    Input format:
        <pattern>\\n
        <text>\\n

    - `?` matches exactly one character.
    - `*` matches any sequence of characters, including an empty one.
    - Any other character matches only itself.
    - Buffer size for every line -- `0x20`, starts from `0x00`.
    - Returns 1 when the text matches the pattern and 0 otherwise.
    - A recursive solution with backtracking is recommended.

    Python example args:
        input (str): The input string with two lines.

    Returns:
        tuple: A tuple containing the match result and the remaining input.
    """
    pattern, rest = read_line(input, 0x20)

    if pattern is None:
        return [overflow_error_value], rest

    text, rest = read_line(rest, 0x20)

    if text is None:
        return [overflow_error_value], rest

    def match(p, t):
        if p == "":
            return t == ""

        if p[0] == "*":
            return match(p[1:], t) or (t != "" and match(p, t[1:]))

        if t == "":
            return False

        if p[0] == "?" or p[0] == t[0]:
            return match(p[1:], t[1:])

        return False

    return [1 if match(pattern, text) else 0], rest


assert glob_match('a*c\nabc\n') == ([1], '')
assert glob_match('a?c\nabc\n') == ([1], '')
assert glob_match('a?c\nabbc\n') == ([0], '')
assert glob_match('*.txt\nfile.txt\n') == ([1], '')
```

### `infix_to_rpn`

```python
def infix_to_rpn(input):
    """Convert an infix expression into Reverse Polish Notation.

    The recommended algorithm is the shunting-yard algorithm: numbers go
    directly to the output, operators are pushed to a stack and popped
    from it according to their priority.

    Examples:
    - "1 + 2 * 3" -> "1 2 3 * +"
    - "(1 + 2) * 3" -> "1 2 + 3 *"

    - Allowed tokens: non-negative decimal numbers, `+`, `-`, `*`, `/`,
      parentheses and spaces.
    - Priority: `*` and `/` are higher than `+` and `-`, operators with
      the same priority are left associative.
    - Output tokens are separated by exactly one space.
    - Result string should be represented as a correct C string.
    - Buffer size for the result -- `0x40`, starts from `0x00`.
    - End of input -- new line.
    - Unknown character or unbalanced parentheses: return -1.

    Python example args:
        input (str): The input string till new line.

    Returns:
        tuple: A tuple containing the RPN string and the remaining input.
    """
    line, rest = read_line(input, 0x40)

    if line is None:
        return [overflow_error_value], rest

    priority = {"+": 1, "-": 1, "*": 2, "/": 2}

    result = ""
    ops = []
    i = 0

    while i < len(line):
        char = line[i]

        if char == " ":
            i += 1

        elif "0" <= char <= "9":
            j = i
            while j < len(line) and "0" <= line[j] <= "9":
                j += 1

            result += line[i:j] + " "
            i = j

        elif char in priority:
            while ops and ops[-1] != "(" and priority[ops[-1]] >= priority[char]:
                result += ops.pop() + " "

            ops.append(char)
            i += 1

        elif char == "(":
            ops.append(char)
            i += 1

        elif char == ")":
            while ops and ops[-1] != "(":
                result += ops.pop() + " "

            if not ops:
                return [-1], rest

            ops.pop()
            i += 1

        else:
            return [-1], rest

    while ops:
        op = ops.pop()

        if op == "(":
            return [-1], rest

        result += op + " "

    # The last space is extra: in the buffer it is replaced by `\0`.
    result = result[:-1]

    if len(result) + 1 > 0x40:
        return [overflow_error_value], rest

    return cstr(result, 0x40)[0], rest


assert infix_to_rpn('1 + 2\n') == ('1 2 +', '')
assert infix_to_rpn('1 + 2 * 3\n') == ('1 2 3 * +', '')
assert infix_to_rpn('(1 + 2) * 3\n') == ('1 2 + 3 *', '')
assert infix_to_rpn('10 - 2 - 3\n') == ('10 2 - 3 -', '')
```

### `reverse_words_cstr`

```python
def reverse_words_cstr(input):
    """Reverse the order of words in a C string.

    Words are separated by spaces. The characters inside each word
    remain unchanged.

    Examples:
        "hello world" -> "world hello"
        "one two three" -> "three two one"

    The result must fit in a 0x40-byte C string.
    """
    line, rest = read_line(input, 0x40)

    if line is None:
        return [overflow_error_value], rest

    try:
        words = line.split(" ")
        words = [word for word in words if word]

        result = " ".join(reversed(words))

        if len(result) + 1 > 0x40:
            return [overflow_error_value], rest

        return cstr(result, 0x40)[0], rest

    except Exception:
        return [-1], rest


assert reverse_words_cstr('hello world\n') == ('world hello', '')
assert reverse_words_cstr('one two three\n') == ('three two one', '')
assert reverse_words_cstr('hello\n') == ('hello', '')
```

### `rle_compress`

```python
def rle_compress(input):
    """Run-length compression: compress consecutive characters.

    Examples:
    - "AAABBBBCCCC" -> "3A4B4C"
    - "aaaaaaaaaa" -> "9a1a" (splits runs > 9)

    - Buffer size for the compressed message -- `0x40`, starts from `0x00`.
    - End of input -- new line.

    Python example args:
        input (str): The input string containing data to compress.

    Returns:
        tuple: A tuple containing the compressed string and the remaining input.
    """
    line, rest = read_line(input, 0x40)
    if line is None:
        return [overflow_error_value], rest

    if not line:
        return "", rest

    try:
        compressed = []
        i = 0
        while i < len(line):
            current_char = line[i]
            count = 1
            while (
                i + count < len(line) and line[i + count] == current_char and count < 9
            ):
                count += 1
            compressed.append(str(count) + current_char)
            i += count
        result = "".join(compressed)
        if len(result) + 1 > 0x40:  # +1 for null terminator
            return [overflow_error_value], rest
        return cstr(result, 0x40)[0], rest

    except Exception:
        return [-1], rest


assert rle_compress('AAABBBBCCCC\n') == ('3A4B4C', '')
assert rle_compress('aaaaaaaaaa\n') == ('9a1a', '')
assert rle_compress('ABC\n') == ('1A1B1C', '')
```

### `rle_compress_bytes`

```python
def rle_compress_bytes(*input_words):
    """Run-length compression for bytes packed in 32-bit words.

    Input format:
    - First word: length of data in bytes
    - Following words: data bytes packed in words (4 bytes per word)
    - If byte count not divisible by 4, pad with zeros

    Output format:
    - First word: length of compressed data in bytes
    - Following words: compressed data as count+byte pairs

    Example: [4, 0x0A0A0A0A] -> [2, 0x040A0000] (4 bytes of 0x0A -> count=4, byte=0x0A)
    """
    if not input_words:
        return [-1]

    length = input_words[0]
    if length < 0:
        return [-1]

    if length == 0:
        return [0]

    try:
        # Extract bytes from words
        bytes_data = []
        word_count = (length + 3) // 4  # Round up to nearest word

        for i in range(1, min(len(input_words), word_count + 1)):
            word = input_words[i]
            for j in range(4):
                if len(bytes_data) < length:
                    byte_val = (word >> (24 - j * 8)) & 0xFF
                    bytes_data.append(byte_val)

        if len(bytes_data) < length:
            return [-1]  # Not enough input data

        # Compress bytes
        compressed = []
        i = 0
        while i < len(bytes_data):
            current_byte = bytes_data[i]
            count = 1

            # Count consecutive identical bytes
            while (
                i + count < len(bytes_data)
                and bytes_data[i + count] == current_byte
                and count < 255
            ):
                count += 1

            compressed.append(count)
            compressed.append(current_byte)
            i += count

        # Pack compressed data into words
        result = [len(compressed)]  # Length in bytes

        for i in range(0, len(compressed), 4):
            word = 0
            for j in range(4):
                if i + j < len(compressed):
                    word |= (compressed[i + j] & 0xFF) << (24 - j * 8)
            result.append(word)

        return result

    except Exception:
        return [-1]


assert rle_compress_bytes(4, 168430090) == [2, 67764224]
assert rle_compress_bytes(12, 2863315899, 3435973836, 3722304989) == [8, 44696251, 80479453]
assert rle_compress_bytes(1, 4278190080) == [2, 33488896]
```

### `rle_decompress`

```python
def rle_decompress(input):
    """Run-length decompression: decompress count+character format.

    Examples:
    - "3A4B4C" -> "AAABBBBCCCC"
    - "9a1a" -> "aaaaaaaaaa"
    .
    - Buffer size for the decompressed message -- `0x40`, starts from `0x00`.
    - End of input -- new line.

    Python example args:
        input (str): The input string containing compressed data to decompress.

    Returns:
        tuple: A tuple containing the decompressed string and the remaining input.
    """
    line, rest = read_line(input, 0x80)
    if line is None:
        return [overflow_error_value], rest

    if not line:
        return "", rest

    try:
        decompressed = []
        i = 0

        while i < len(line):
            if i + 1 >= len(line):
                return [-1], rest  # Invalid format: missing character after count

            # Read count (should be digit 1-9)
            if not line[i].isdigit() or line[i] == "0":
                return [-1], rest  # Invalid count

            count = int(line[i])
            char = line[i + 1]

            # Add repeated character
            decompressed.append(char * count)
            i += 2

        result = "".join(decompressed)
        if len(result) + 1 > 0x40:  # +1 for null terminator
            return [overflow_error_value], rest

        return cstr(result, 0x40)[0], rest

    except Exception:
        return [-1], rest


assert rle_decompress('3A4B4C\n') == ('AAABBBBCCCC', '')
assert rle_decompress('9a1a\n') == ('aaaaaaaaaa', '')
assert rle_decompress('1A1B1C\n') == ('ABC', '')
```

### `rle_decompress_bytes`

```python
def rle_decompress_bytes(*input_words):
    """Run-length decompression for bytes packed in 32-bit words.

    Input format:
    - First word: length of compressed data in bytes
    - Following words: compressed data as count+byte pairs

    Output format:
    - First word: length of decompressed data in bytes
    - Following words: decompressed bytes packed in words

    Example: [2, 0x040A0000] -> [4, 0x0A0A0A0A] (count=4, byte=0x0A -> 4 bytes of 0x0A)
    """
    if not input_words:
        return [-1]

    length = input_words[0]
    if length < 0:
        return [-1]

    if length == 0:
        return [0]

    if length % 2 != 0:
        return [-1]  # Compressed data must be count+byte pairs

    try:
        # Extract compressed bytes from words
        compressed_data = []
        word_count = (length + 3) // 4  # Round up to nearest word

        for i in range(1, min(len(input_words), word_count + 1)):
            word = input_words[i]
            for j in range(4):
                if len(compressed_data) < length:
                    byte_val = (word >> (24 - j * 8)) & 0xFF
                    compressed_data.append(byte_val)

        if len(compressed_data) < length:
            return [-1]  # Not enough input data

        # Decompress bytes
        decompressed = []
        for i in range(0, len(compressed_data), 2):
            if i + 1 >= len(compressed_data):
                return [-1]  # Invalid format

            count = compressed_data[i]
            byte_val = compressed_data[i + 1]

            if count == 0:
                return [-1]  # Invalid count

            decompressed.extend([byte_val] * count)

        # Pack decompressed data into words
        result = [len(decompressed)]  # Length in bytes

        for i in range(0, len(decompressed), 4):
            word = 0
            for j in range(4):
                if i + j < len(decompressed):
                    word |= (decompressed[i + j] & 0xFF) << (24 - j * 8)
            result.append(word)

        return result

    except Exception:
        return [-1]


assert rle_decompress_bytes(2, 67764224) == [4, 168430090]
assert rle_decompress_bytes(6, 44696251, 80478208) == [8, 2863315899, 3435973836]
assert rle_decompress_bytes(2, 33488896) == [1, 4278190080]
```

### `stack_based_calculator`

```python
def stack_based_calculator(input):
    """Stack-based calculator supporting +, -, *, / operations.

    Uses Reverse Polish Notation (RPN). Examples:
    - "1 1 +" -> 2
    - "1 2 3 4 + * /" -> 0 (integer division, floor)
    - "1 2 + 3 *" -> 9

    - Separator: spaces
    - End of input -- new line.
    - Division by zero returns -1.
    - Overflow returns 0xCCCCCCCC.
    - Invalid expressions return -1.

    Python example args:
        input (str): The input string containing RPN expression.

    Returns:
        tuple: A tuple containing the result as a list and the remaining input.
    """
    line, rest = read_line(input, 0x40)
    if line is None:
        return [overflow_error_value], rest

    if not line.strip():
        return [-1], rest

    try:
        tokens = line.strip().split()
        stack = []

        for token in tokens:
            if token in ["+", "-", "*", "/"]:
                if len(stack) < 2:
                    return [-1], rest  # Not enough operands

                b = stack.pop()
                a = stack.pop()

                if token == "+":
                    result = a + b
                elif token == "-":
                    result = a - b
                elif token == "*":
                    result = a * b
                elif token == "/":
                    if b == 0:
                        return [-1], rest  # Division by zero
                    result = a // b  # Integer division
                else:
                    return [-1], rest

                if result < -2147483648 or result > 2147483647:
                    return [overflow_error_value], rest

                stack.append(result)
            else:
                num = int(token)
                if num < -2147483648 or num > 2147483647:
                    return [overflow_error_value], rest
                stack.append(num)

            print(stack)
        if len(stack) != 1:
            return [-1], rest

        return [stack[0]], rest

    except Exception:
        return [-1], rest


assert stack_based_calculator('1 1 +\n') == ([2], '')
assert stack_based_calculator('1 2 + 3 *\n') == ([9], '')
assert stack_based_calculator('10 3 /\n') == ([3], '')
```

### `text_word_counter`

```python
def text_word_counter(input):
    """Count word frequencies in text with max word length of 3 symbols.

    Separators: space, comma, dot
    Max word length: 3 symbols
    Max total unique words: 12
    Output: counts in order of first appearance

    Examples:
    - "a bb ccc a ccc a" -> "3 1 2" (a appears 3 times, bb once, ccc twice)
    - "word" -> return -1 (word too long)
    - More than 12 unique words -> return -1

    - Result string should be represented as a correct C string.
    - Buffer size for the result -- `0x40`, starts from `0x00`.
    - End of input -- new line.
    - Initial buffer values -- `_`.

    Python example args:
        input (str): The input string containing text to analyze.

    Returns:
        tuple: A tuple containing the word counts and the remaining input.
    """
    line, rest = read_line(input, 0x40)
    if line is None:
        return [overflow_error_value], rest

    if not line:
        return "", rest

    try:
        # Split text by separators (space, comma, dot)
        words = []
        current_word = ""

        for char in line:
            if char in " ,.":
                if current_word:
                    words.append(current_word)
                    current_word = ""
            else:
                current_word += char

        # Add last word if exists
        if current_word:
            words.append(current_word)

        # Check for words longer than 3 symbols
        for word in words:
            if len(word) > 3:
                return [-1], rest

        # Count words in order of first appearance
        word_order = []
        word_counts = {}

        for word in words:
            if word not in word_counts:
                word_order.append(word)
                word_counts[word] = 0
                # Check if we exceed 12 unique words
                if len(word_order) > 12:
                    return [-1], rest
            word_counts[word] += 1

        # Build result string
        if not word_order:
            result = ""
        else:
            counts = [str(word_counts[word]) for word in word_order]
            result = " ".join(counts)

        if len(result) + 1 > 0x40:  # +1 for null terminator
            return [overflow_error_value], rest

        return cstr(result, 0x40)[0], rest

    except Exception:
        return [-1], rest


assert text_word_counter('a bb ccc a ccc a\n') == ('3 1 2', '')
assert text_word_counter('cat dog cat\n') == ('2 1', '')
assert text_word_counter('a,b.c a\n') == ('2 1 1', '')
```

## Mathematics

### `collatz_length`

```python
def collatz_length(n):
    """Count the number of steps to reach 1 in the Collatz sequence.

    Starting from n, apply:
    - n even: n = n // 2
    - n odd: n = 3 * n + 1
    Repeat until n == 1; return the number of steps.

    Note: intermediate values may temporarily exceed 32 bits for some inputs.

    - n <= 0: return -1
    - n == 1: return 0

    Args:
        n (int): The starting value.

    Returns:
        int: The number of steps to reach 1, or -1 for invalid input.
    """
    if n <= 0:
        return -1
    steps = 0
    while n != 1:
        if n % 2 == 0:
            n //= 2
        else:
            n = 3 * n + 1
        steps += 1
    return steps


assert collatz_length(1) == 0
assert collatz_length(2) == 1
assert collatz_length(6) == 8
assert collatz_length(10) == 6
```

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

### `gcd_many`

```python
def gcd_many(*input_words):
    """Find the GCD of multiple integers.

    Input format:
        [count, value0, value1, ...]

    The count must be positive and must match the number of values.

    Args:
        *input_words (int): Number of values followed by the values.

    Returns:
        list: A one-element list containing the GCD.
    """
    if not input_words:
        return [-1]

    count = input_words[0]

    if count <= 0 or len(input_words) != count + 1:
        return [-1]

    result = abs(input_words[1])

    for value in input_words[2:]:
        a = result
        b = abs(value)

        while b != 0:
            a, b = b, a % b

        result = a

    return [result]


assert gcd_many(2, 48, 18) == [6]
assert gcd_many(3, 12, 18, 24) == [6]
assert gcd_many(4, 48, 18, 30, 42) == [6]
```

### `integer_sqrt`

```python
def integer_sqrt(n):
    """Compute the integer square root (floor of sqrt(n)).

    - n < 0: return -1
    - n == 0: return 0

    Args:
        n (int): The non-negative integer.

    Returns:
        int: floor(sqrt(n)), or -1 for negative input.
    """
    if n < 0:
        return -1
    x = int(n**0.5)
    while x * x > n:
        x -= 1
    while (x + 1) * (x + 1) <= n:
        x += 1
    return x


assert integer_sqrt(0) == 0
assert integer_sqrt(1) == 1
assert integer_sqrt(4) == 2
assert integer_sqrt(9) == 3
assert integer_sqrt(16) == 4
assert integer_sqrt(25) == 5
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


assert is_prime(2) == 1
assert is_prime(5) == 1
assert is_prime(4) == 0
assert is_prime(7) == 1
assert is_prime(8) == 0
assert is_prime(283) == 1
assert is_prime(284) == 0
assert is_prime(293) == 1
```

### `lcm`

```python
def lcm(a, b):
    """Compute the least common multiple (LCM) of two positive integers.

    - a <= 0 or b <= 0: return -1
    - Overflow: return 0xCCCCCCCC

    Args:
        a (int): First positive integer.
        b (int): Second positive integer.

    Returns:
        list: A one-element list with the LCM.
    """
    if a <= 0 or b <= 0:
        return [-1]
    g = _gcd_helper(a, b)
    result = (a // g) * b
    if result > max_int32:
        return [overflow_error_value]
    return [result]


assert lcm(4, 6) == [12]
assert lcm(12, 18) == [36]
assert lcm(7, 5) == [35]
assert lcm(1, 100) == [100]
```

### `power`

```python
def power(base, exp):
    """Compute base raised to the power of a non-negative exponent.

    - exp < 0: return -1
    - Overflow (result outside int32 range): return 0xCCCCCCCC

    Args:
        base (int): The base value.
        exp (int): The non-negative exponent.

    Returns:
        list: A one-element list with the result.
    """
    if exp < 0:
        return [-1]
    result = 1
    for _ in range(exp):
        result *= base
        if result > max_int32 or result < min_int32:
            return [overflow_error_value]
    return [result]


assert power(2, 10) == [1024]
assert power(3, 5) == [243]
assert power(5, 0) == [1]
assert power(0, 5) == [0]
```

### `power_many`

```python
def power_many(*input_words):
    """Compute powers for multiple (base, exponent) pairs.

    Input format:
        [count, base0, exp0, base1, exp1, ...]

    Each exponent must be non-negative. Results must fit in int32.

    Args:
        *input_words (int): Number of pairs followed by base/exponent pairs.

    Returns:
        list: One result for each pair.
    """
    if not input_words:
        return [-1]

    count = input_words[0]

    if count <= 0 or len(input_words) != 1 + 2 * count:
        return [-1]

    results = []

    for i in range(count):
        base = input_words[1 + 2 * i]
        exp = input_words[2 + 2 * i]

        if exp < 0:
            return [-1]

        result = 1

        for _ in range(exp):
            result *= base

            if result < min_int32 or result > max_int32:
                return [overflow_error_value]

        results.append(result)

    return results


assert power_many(2, 2, 10, 3, 5) == [1024, 243]
assert power_many(3, 5, 0, 0, 5, 10, 2) == [1, 0, 100]
assert power_many(1, 7, 1) == [7]
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


assert sum_word_cstream(48, 18, 0) == [0, 66]
assert sum_word_cstream(1, 0) == [0, 1]
assert sum_word_cstream(48, 18, 0, 12, 0) == [0, 66]
assert sum_word_cstream(1, 0) == [0, 1]
assert sum_word_cstream(2147483647, 1, 0) == [0, 2147483648]
assert sum_word_cstream(2147483647, 1, 2147483647, 0) == [0, 4294967295]
assert sum_word_cstream(2147483647, 1, 2147483647, 1, 0) == [1, 0]
assert sum_word_cstream(2147483647, 1, 2147483647, 2, 0) == [1, 1]
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


assert sum_word_pstream(2, 48, 18) == [0, 66]
assert sum_word_pstream(1, 1) == [0, 1]
assert sum_word_pstream(2, 48, 18, 0, 12) == [0, 66]
assert sum_word_pstream(2, 48, 18, 12) == [0, 66]
assert sum_word_pstream(2, 2147483647, 1, 0) == [0, 2147483648]
assert sum_word_pstream(3, 2147483647, 1, 2147483647, 0) == [0, 4294967295]
assert sum_word_pstream(4, 2147483647, 1, 2147483647, 1, 0) == [1, 0]
assert sum_word_pstream(4, 2147483647, 1, 2147483647, 2, 0) == [1, 1]
assert sum_word_pstream(2, 1, -1) == [0, 0]
```

## String Manipulation

### `caesar_cipher`

```python
def caesar_cipher(input):
    """Apply a Caesar cipher to a line of text.

    Input format:
        <shift>\\n
        <text>\\n

    - Positive shift encrypts the text.
    - Negative shift is also allowed.
    - Only ASCII letters are shifted.
    - Uppercase and lowercase letters preserve their case.
    - Non-letter characters remain unchanged.
    - Shift is taken modulo 26.

    Returns:
        tuple: A tuple containing the transformed string and remaining input.
    """
    lines = input.split("\n")

    if len(lines) < 2:
        return [-1], input

    shift_line = lines[0]
    text = lines[1]

    try:
        if not shift_line:
            return [-1], "\n".join(lines[1:])

        shift = int(shift_line)

        if shift < -2147483648 or shift > 2147483647:
            return [-1], "\n".join(lines[2:])

        shift %= 26

        result = []

        for char in text:
            if "a" <= char <= "z":
                result.append(chr((ord(char) - ord("a") + shift) % 26 + ord("a")))
            elif "A" <= char <= "Z":
                result.append(chr((ord(char) - ord("A") + shift) % 26 + ord("A")))
            else:
                result.append(char)

        remaining = "\n".join(lines[2:])

        return "".join(result), remaining

    except Exception:
        return [-1], input


assert caesar_cipher('3\nHello, World!\n') == ('Khoor, Zruog!', '')
assert caesar_cipher('-3\nKhoor\n') == ('Hello', '')
assert caesar_cipher('0\nHello\n') == ('Hello', '')
```

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

### `lower_case_cstr`

```python
def lower_case_cstr(s):
    """Convert a C string to lower case.

    - Result string should be represented as a correct C string.
    - Buffer size for the message -- `0x20`, starts from `0x00`.
    - End of input -- new line.
    - Initial buffer values -- `_`.

    Python example args:
        s (str): The input C string.

    Returns:
        tuple: A tuple containing the lower case string and the remaining input.
    """
    line, rest = read_line(s, 0x20)
    if line is None:
        return [overflow_error_value], rest
    return cstr(line.lower(), 0x20)[0], rest


assert lower_case_cstr('HELLO\n') == ('hello', '')
# and mem[0..31]: 68 65 6c 6c 6f 00 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f
assert lower_case_cstr('World\n') == ('world', '')
# and mem[0..31]: 77 6f 72 6c 64 00 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f
```

### `lower_case_pstr`

```python
def lower_case_pstr(s):
    """Convert a Pascal string to lower case.

    - Result string should be represented as a correct Pascal string.
    - Buffer size for the message -- `0x20`, starts from `0x00`.
    - End of input -- new line.
    - Initial buffer values -- `_`.

    Python example args:
        s (str): The input string.

    Returns:
        tuple: A tuple containing the lower case string and the remaining input.
    """
    line, rest = read_line(s, 0x20)
    if line is None:
        return [overflow_error_value], rest
    return line.lower(), rest


assert lower_case_pstr('HELLO\n') == ('hello', '')
# and mem[0..31]: 05 68 65 6c 6c 6f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f
assert lower_case_pstr('World\n') == ('world', '')
# and mem[0..31]: 05 77 6f 72 6c 64 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f 5f
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
    return cstr(line, 0x20)[0][::-1], rest


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
        s (str): The string with `\n` as end of the input.

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

### `strstr_cstr`

```python
def strstr_cstr(input):
    """Find a substring inside a C string.

    Input format:
        "haystack|needle\\n"

    The '|' character separates the haystack from the needle.

    Returns:
        tuple: The zero-based index of the first occurrence of needle,
        or -1 if needle is not found.

    The input and strings are limited to the 0x20-byte C-string buffer.
    """
    line, rest = read_line(input, 0x40)

    if line is None:
        return [overflow_error_value], rest

    try:
        if "|" not in line:
            return [-1], rest

        haystack, needle = line.split("|", 1)

        if len(haystack) + 1 > 0x20 or len(needle) + 1 > 0x20:
            return [overflow_error_value], rest

        # Empty needle matches at the beginning.
        if needle == "":
            return [0], rest

        if len(needle) > len(haystack):
            return [-1], rest

        for i in range(len(haystack) - len(needle) + 1):
            if haystack[i : i + len(needle)] == needle:
                return [i], rest

        return [-1], rest

    except Exception:
        return [-1], rest


assert strstr_cstr('hello world|world\n') == ([6], '')
assert strstr_cstr('hello world|hello\n') == ([0], '')
assert strstr_cstr('hello world|xyz\n') == ([-1], '')
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

## VLIW

### `affine2d_transform`

```python
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


assert affine2d_transform(0) == []
assert affine2d_transform(1, 1, 2) == [12, 0]
assert affine2d_transform(2, 0, 0, 3, -1) == [5, -7, 12, -14]
assert affine2d_transform(3, -2, 5, 10, 0, -1, -1) == [9, 15, 35, -17, 0, -10]
```

### `complex_multiply`

```python
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


assert complex_multiply(1, 2, 3, 4) == [-5, 10]
assert complex_multiply(0, 0, 5, -7) == [0, 0]
assert complex_multiply(-1, 2, 3, -4) == [5, 10]
assert complex_multiply(123, 456, 7, 8) == [-2787, 4176]
```

### `determinant_2x2_stream`

```python
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


assert determinant_2x2_stream(0) == []
assert determinant_2x2_stream(1, 1, 2, 3, 4) == [-2]
assert determinant_2x2_stream(2, 1, 0, 0, 1, 2, 3, 5, 7) == [1, -1]
assert determinant_2x2_stream(3, 0, 0, 0, 0, -1, 2, 3, -4, 7, -5, 4, 8) == [0, -2, 76]
```

### `determinant_3x3`

```python
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


assert determinant_3x3(0, 0, 0, 0, 0, 0, 0, 0, 0) == [0]
assert determinant_3x3(1, 2, 3, 4, 5, 6, 7, 8, 9) == [0]
assert determinant_3x3(0, 0, 1, 0, 1, 0, 1, 0, 0) == [-1]
assert determinant_3x3(7, -5, 4, 32, 8, 3, 5, 2, 8) == [1707]
```

### `djb2_hash`

```python
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


assert djb2_hash('\0') == 5381
assert djb2_hash('a\0') == 177670
assert djb2_hash('abc\0') == 193485963
assert djb2_hash('Computers are awesome!\0') == 2262080881
```

### `fnv32_1_hash`

```python
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


assert fnv32_1_hash('a\0') == 84696446
assert fnv32_1_hash('abc\0') == 1134309195
assert fnv32_1_hash('Computers are awesome!\0') == 3917207935
```

### `fnv32_1a_hash`

```python
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


assert fnv32_1a_hash('a\0') == 3826002220
assert fnv32_1a_hash('abc\0') == 440920331
assert fnv32_1a_hash('Computers are awesome!\0') == 4243580747
```

### `four_lane_mac`

```python
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


assert four_lane_mac(1, 1, 2, 3, 4, 5, 6, 7, 8) == [5, 12, 21, 32]
assert four_lane_mac(2, 1, 2, 3, 4, 10, 20, 30, 40, -1, -2, -3, -4, 5, 6, 7, 8) == [10, 40, 90, 160, -5, -12, -21, -32]
assert four_lane_mac(0) == []
```

### `linear_filter`

```python
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


assert linear_filter(0) == []
assert linear_filter(1, 5) == [15]
assert linear_filter(2, 5, 10) == [15, 40]
assert linear_filter(3, 1, 2, 3) == [3, 8, 14]
assert linear_filter(5, 1, 2, 3, 4, 5) == [3, 8, 14, 20, 26]
```

### `matrix_2x2_vector_stream`

```python
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


assert matrix_2x2_vector_stream(0) == []
assert matrix_2x2_vector_stream(1, 1, 2, 3, 4, 5, 6) == [17, 39]
assert matrix_2x2_vector_stream(2, 1, 0, 0, 1, 5, 6, 2, 3, 4, 5, 1, -1) == [5, 6, -1, -1]
```

### `min_max_sum`

```python
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


assert min_max_sum(0) == [0, 0, 0]
assert min_max_sum(1, 42) == [42, 42, 42]
assert min_max_sum(4, 5, 2, 9, 1) == [1, 9, 17]
assert min_max_sum(5, -2, 7, -3, 4, 1) == [-3, 7, 7]
```

### `pairwise_add_sub`

```python
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


assert pairwise_add_sub(0) == []
assert pairwise_add_sub(1, 10, 3) == [13, 7]
assert pairwise_add_sub(2, 10, 3, 5, 8) == [13, 7, 13, -3]
assert pairwise_add_sub(3, -5, 2, 100, -40, 7, 7) == [-3, -7, 60, 140, 14, 0]
```

### `rgb_to_grayscale`

```python
def rgb_to_grayscale(*xs):
    """Input: first word N, then N pixels packed as 0x00RRGGBB.

    For each pixel calculate the grayscale value with fixed point weights:

        gray = (77 * R + 150 * G + 29 * B) >> 8

    Output: N gray values (0..255).

    - N < 0: return -1.
    - The highest byte of a pixel is ignored.
    """
    n = xs[0]

    if n < 0:
        return [-1]

    result = []

    for i in range(n):
        pixel = xs[1 + i]

        r = (pixel >> 16) & 0xFF
        g = (pixel >> 8) & 0xFF
        b = pixel & 0xFF

        result.append((77 * r + 150 * g + 29 * b) >> 8)

    return result


assert rgb_to_grayscale(0) == []
assert rgb_to_grayscale(2, 0, 16777215) == [0, 255]
assert rgb_to_grayscale(3, 16711680, 65280, 255) == [76, 149, 28]
assert rgb_to_grayscale(2, 8421504, 1056816) == [128, 29]
```

### `sdbm_hash`

```python
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


assert sdbm_hash('\0') == 0
assert sdbm_hash('a\0') == 97
assert sdbm_hash('abc\0') == 807794786
assert sdbm_hash('Computers are awesome!\0') == 79142482
```

### `sum_and_sum_squares`

```python
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


assert sum_and_sum_squares(0) == [0, 0]
assert sum_and_sum_squares(3, 1, 2, 3) == [6, 14]
assert sum_and_sum_squares(4, -2, 5, 0, -3) == [0, 38]
assert sum_and_sum_squares(5, 10, 20, 30, 40, 50) == [150, 5500]
```

## _Examples_

### `dup`

```python
def dup(x):
    return [x, x]


assert dup(42) == [42, 42]
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
