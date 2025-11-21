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
- Complex Tasks
    - [base64_decoding](#base64_decoding)
    - [base64_encoding](#base64_encoding)
    - [brainfuck_interpreter](#brainfuck_interpreter)
    - [format_string](#format_string)
    - [rle_compress](#rle_compress)
    - [rle_compress_bytes](#rle_compress_bytes)
    - [rle_decompress](#rle_decompress)
    - [rle_decompress_bytes](#rle_decompress_bytes)
    - [stack_based_calculator](#stack_based_calculator)
    - [text_word_counter](#text_word_counter)
- Mathematics
    - [count_divisors](#count_divisors)
    - [determinant_3x3](#determinant_3x3)
    - [djb2_hash](#djb2_hash)
    - [fibonacci](#fibonacci)
    - [fnv32_1_hash](#fnv32_1_hash)
    - [fnv32_1a_hash](#fnv32_1a_hash)
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
        if len(format_str) > 0x20:
            return [-1], input

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
        integers = []
        line_idx = 1
        for _ in range(placeholder_count):
            if line_idx >= len(lines):
                return [-1], input
            try:
                parsed_int = int(lines[line_idx])
                # Check 32-bit boundary
                if parsed_int < -2147483648 or parsed_int > 2147483647:
                    remaining = "\n".join(lines[line_idx:])
                    return [-1], remaining
                integers.append(parsed_int)
                line_idx += 1
            except ValueError:
                # Check if the line is empty (missing input) or invalid
                if lines[line_idx] == "":
                    # Empty line - consume it and return what's after
                    remaining = (
                        "\n".join(lines[line_idx + 1 :])
                        if line_idx + 1 < len(lines)
                        else ""
                    )
                else:
                    # Non-empty invalid line - include it in remaining
                    remaining = "\n".join(lines[line_idx:])
                return [-1], remaining

        # Format the string
        try:
            if placeholder_count == 0:
                result = format_str
            else:
                result = format_str % tuple(integers)
        except (TypeError, ValueError):
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


assert rle_compress_bytes([4, 168430090]) == [2, 67764224]
assert rle_compress_bytes([12, 2863315899, 3435973836, 3722304989]) == [8, 44696251, 80479453]
assert rle_compress_bytes([1, 4278190080]) == [2, 33488896]
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


assert rle_decompress_bytes([2, 67764224]) == [4, 168430090]
assert rle_decompress_bytes([6, 44696251, 80478208]) == [8, 2863315899, 3435973836]
assert rle_decompress_bytes([2, 33488896]) == [1, 4278190080]
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


assert determinant_3x3([0, 0, 0, 0, 0, 0, 0, 0, 0]) == [0]
assert determinant_3x3([1, 2, 3, 4, 5, 6, 7, 8, 9]) == [0]
assert determinant_3x3([0, 0, 1, 0, 1, 0, 1, 0, 0]) == [-1]
assert determinant_3x3([7, -5, 4, 32, 8, 3, 5, 2, 8]) == [1707]
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
