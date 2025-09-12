from testcases.core import (
    TestCase,
    overflow_error_value,
    String2String,
    Words2Words,
    TEST_CASES,
    read_line,
    cstr,
)
import base64

###########################################################


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


TEST_CASES["base64_encoding"] = TestCase(
    simple=base64_encoding,
    cases=[
        String2String(
            "Hello!\n",
            "SGVsbG8h",
            "",
        ),
    ],
    reference=base64_encoding,
    reference_cases=[
        String2String(
            "Hello\n",
            "SGVsbG8=",
            "",
        ),
        String2String(
            "Hello\nWorld\n",
            "SGVsbG8=",
            "World\n",
        ),
        String2String(
            "A\n",
            "QQ==",
            "",
        ),
        String2String(
            "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0\n",
            [overflow_error_value],
            "0\n",
        ),
        String2String(
            "\n",
            "",
            "",
        ),
    ],
    is_variant=True,
    category="Complex Tasks",
)

###########################################################


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


TEST_CASES["base64_decoding"] = TestCase(
    simple=base64_decoding,
    cases=[
        String2String(
            "SGVsbG8gd29ybGQh\n",
            "Hello world!",
            "",
        ),
        String2String(
            "UHl0aG9u\n",
            "Python",
            "",
        ),
    ],
    reference=base64_decoding,
    reference_cases=[
        String2String(
            "SGVsbG8gd29ybGQh\nNext line",
            "Hello world!",
            "Next line",
        ),
        String2String(
            "\n",
            "",
            "",
        ),
        String2String(
            "QQ==\n",
            "A",
            "",
        ),
        String2String(
            "InvalidBase64!\n",
            [-1],
            "",
        ),
        String2String(
            "A" * 65 + "\n",
            [overflow_error_value],
            "A\n",
        ),
    ],
    is_variant=True,
    category="Complex Tasks",
)

###########################################################


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


TEST_CASES["stack_based_calculator"] = TestCase(
    simple=stack_based_calculator,
    cases=[
        String2String(
            "1 1 +\n",
            [2],
            "",
        ),
        String2String(
            "1 2 + 3 *\n",
            [9],
            "",
        ),
        String2String(
            "10 3 /\n",
            [3],
            "",
        ),
    ],
    reference=stack_based_calculator,
    reference_cases=[
        String2String(
            "1 2 3   4 + * /\n",
            [0],
            "",
        ),
        String2String(
            "2 3   4 + * 1 /\n",
            [14],
            "",
        ),
        String2String(
            "5 0 /\n",
            [-1],
            "",
        ),
        String2String(
            "1 5 0 / *\n",
            [-1],
            "",
        ),
        String2String(
            "15 7 1 1 + / /\n",
            [5],
            "",
        ),
        String2String(
            "\n",
            [-1],
            "",
        ),
        String2String(
            "1 +\n",
            [-1],
            "",
        ),
        String2String(
            "1 2\n",
            [-1],
            "",
        ),
        String2String(
            "invalid\n",
            [-1],
            "",
        ),
        String2String(
            "1 1 +\nNext line",
            [2],
            "Next line",
        ),
        String2String(
            "A" * 65 + "\n",
            [overflow_error_value],
            "A\n",
        ),
    ],
    is_variant=True,
    category="Complex Tasks",
)

###########################################################


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


TEST_CASES["brainfuck_interpreter"] = TestCase(
    simple=brainfuck_interpreter,
    cases=[
        String2String(
            "++.\n",
            "\x02",
            "",
        ),
        String2String(
            "++++++++++++++++++++++++++++++++++++++++++++++++++.\n",
            "2",
            "",
        ),
        String2String(
            ",.\nA",
            "A",
            "",
        ),
        String2String(
            "<\n",
            [-1],
            "",
        ),
    ],
    reference=brainfuck_interpreter,
    reference_cases=[
        String2String(
            "+++++++++.\n",
            "\t",
            "",
        ),
        String2String(
            ",+.\nAC",
            "B",
            "C",
        ),
        String2String(
            ",-.\nBC",
            "A",
            "C",
        ),
        String2String(
            "++++++++++++++++++++++++++++++++.\n",
            " ",
            "",
        ),
        String2String(
            ">\n",
            "",
            "",
        ),
        String2String(
            ">" * 29 + "\n",
            "",
            "",
        ),
        String2String(
            ">" * 30 + "\n",
            [-1],
            "",
        ),
        String2String(
            "invalid_command\n",
            [-1],
            "",
        ),
        String2String(
            "\n",
            "",
            "",
        ),
        String2String(
            "A" * 65 + "\n",
            [overflow_error_value],
            "A\n",
        ),
        String2String(
            ",.>,.>,..<.<.\nfoo",
            "foooof",
            "",
        ),
        String2String(
            "++++++++++[>++++++++++<-]>.\n",
            "d",
            "",
            limit=5000,
        ),
        String2String(
            "+++++[>+++++[>++<-]<-]>>.\n",
            "2",
            "",
            limit=5000,
        ),
        String2String(
            "+++++[>+++++<-]>[\n",
            [-1],
            "",
        ),
        String2String(
            "+++++[>+++++<-]>]\n",
            [-1],
            "",
        ),
    ],
    is_variant=True,
    category="Complex Tasks",
)

###########################################################


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


TEST_CASES["rle_compress"] = TestCase(
    simple=rle_compress,
    cases=[
        String2String(
            "AAABBBBCCCC\n",
            "3A4B4C",
            "",
        ),
        String2String(
            "aaaaaaaaaa\n",
            "9a1a",
            "",
        ),
        String2String(
            "ABC\n",
            "1A1B1C",
            "",
        ),
    ],
    reference=rle_compress,
    reference_cases=[
        String2String(
            "AAABBBBCCCC\nNext line",
            "3A4B4C",
            "Next line",
        ),
        String2String(
            "\n",
            "",
            "",
        ),
        String2String(
            "Hello World!\n",
            "1H1e2l1o1 1W1o1r1l1d1!",
            "",
        ),
        String2String(
            "A\n",
            "1A",
            "",
        ),
        String2String(
            "AAAAAAAAAAAAAAA\n",
            "9A6A",
            "",
        ),
        String2String(
            "AAAAAAAAAAAAAAAAAA\n",
            "9A9A",
            "",
        ),
        String2String(
            "A" * 65 + "\n",
            [overflow_error_value],
            "A\n",
        ),
        String2String(
            "ab" * 15 + "c\n",
            "1a1b1a1b1a1b1a1b1a1b1a1b1a1b1a1b1a1b1a1b1a1b1a1b1a1b1a1b1a1b1c",
            "",
        ),
        String2String(
            "ab" * 16 + "\n",
            [overflow_error_value],
            "",
        ),
        String2String("ABCDEFGHIJKLMNOP\n", "1A1B1C1D1E1F1G1H1I1J1K1L1M1N1O1P", ""),
    ],
    is_variant=True,
    category="Complex Tasks",
)

###########################################################


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


TEST_CASES["rle_decompress"] = TestCase(
    simple=rle_decompress,
    cases=[
        String2String(
            "3A4B4C\n",
            "AAABBBBCCCC",
            "",
        ),
        String2String(
            "9a1a\n",
            "aaaaaaaaaa",
            "",
        ),
        String2String(
            "1A1B1C\n",
            "ABC",
            "",
        ),
    ],
    reference=rle_decompress,
    reference_cases=[
        String2String(
            "3A4B4C\nNext line",
            "AAABBBBCCCC",
            "Next line",
        ),
        String2String(
            "\n",
            "",
            "",
        ),
        String2String(
            "1A\n",
            "A",
            "",
        ),
        String2String(
            "9A6A\n",
            "AAAAAAAAAAAAAAA",
            "",
        ),
        String2String(
            "9A9A\n",
            "AAAAAAAAAAAAAAAAAA",
            "",
        ),
        String2String(
            "A\n",
            [-1],
            "",
        ),
        String2String(
            "0A\n",
            [-1],
            "",
        ),
        String2String(
            "1\n",
            [-1],
            "",
        ),
        String2String(
            "9Z9Z9Z9Z9Z9Z9Z9Z\n",
            [overflow_error_value],
            "",
        ),
        String2String(
            "1a" * 16 + "1b" * 16 + "1c" * 16 + "1d" * 15 + "\n",
            "aaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbccccccccccccccccddddddddddddddd",
            "",
        ),
        String2String(
            "1a" * 16 + "1b" * 16 + "1c" * 16 + "1d" * 16 + "\n",
            [overflow_error_value],
            "\n",
        ),
    ],
    is_variant=True,
    category="Complex Tasks",
)


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


TEST_CASES["rle_compress_bytes"] = TestCase(
    simple=rle_compress_bytes,
    cases=[
        Words2Words([4, 0x0A0A0A0A], [2, 0x040A0000]),
        Words2Words(
            [12, 0xAAAABBBB, 0xCCCCCCCC, 0xDDDDDDDD], [8, 0x02AA02BB, 0x04CC04DD]
        ),
        Words2Words([1, 0xFF000000], [2, 0x01FF0000]),
    ],
    reference=rle_compress_bytes,
    reference_cases=[
        Words2Words([0], [0]),
        Words2Words([3, 0xAAAB0000], [6, 0x01AA01AB, 0x01000000]),
        Words2Words([12, 0xAAAAAAAA, 0xAAAAAAAA, 0xAAAAAAAA], [2, 0x0CAA0000]),
    ],
    is_variant=True,
    category="Complex Tasks",
)

###########################################################


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


TEST_CASES["rle_decompress_bytes"] = TestCase(
    simple=rle_decompress_bytes,
    cases=[
        Words2Words([2, 0x040A0000], [4, 0x0A0A0A0A]),
        Words2Words([6, 0x02AA02BB, 0x04CC0000], [8, 0xAAAABBBB, 0xCCCCCCCC]),
        Words2Words([2, 0x01FF0000], [1, 0xFF000000]),
    ],
    reference=rle_decompress_bytes,
    reference_cases=[
        Words2Words([0], [0]),
        Words2Words([6, 0x01AA01AB, 0x01000000], [3, 0xAAAB0000]),
        Words2Words([2, 0x0CAA0000], [12, 0xAAAAAAAA, 0xAAAAAAAA, 0xAAAAAAAA]),
        Words2Words([3, 0x01AA0000], [-1]),  # Invalid: odd length
        Words2Words([2, 0x00AA0000], [-1]),  # Invalid: zero count
    ],
    is_variant=True,
    category="Complex Tasks",
)

###########################################################


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


TEST_CASES["text_word_counter"] = TestCase(
    simple=text_word_counter,
    cases=[
        String2String(
            "a bb ccc a ccc a\n",
            "3 1 2",
            "",
        ),
        String2String(
            "cat dog cat\n",
            "2 1",
            "",
        ),
        String2String(
            "a,b.c a\n",
            "2 1 1",
            "",
        ),
    ],
    reference=text_word_counter,
    reference_cases=[
        String2String(
            "a bb ccc a ccc a\nNext line",
            "3 1 2",
            "Next line",
        ),
        String2String(
            "\n",
            "",
            "",
        ),
        String2String(
            "abc\n",
            "1",
            "",
        ),
        String2String(
            "word\n",
            [-1],
            "",
        ),
        String2String(
            "a bb ccc word\n",
            [-1],
            "",
        ),
        String2String(
            "   ,,,   ...\n",
            "",
            "",
        ),
        String2String(
            "x y z x y z x y z x y z x y z x y z x y z x y z\n",
            "8 8 8",
            "",
            limit=5000,
        ),
        String2String(
            "a b c d e f g h i j k l m\n",
            [-1],
            "",
        ),
    ],
    is_variant=True,
    category="Complex Tasks",
)

###########################################################


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
                # Calculate remaining input from the failed line
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


TEST_CASES["format_string"] = TestCase(
    simple=format_string,
    cases=[
        String2String(
            "Num: %d\n42\n",
            "Num: 42",
            "",
        ),
        String2String(
            "%5d\n42\n",
            "   42",
            "",
        ),
        String2String(
            "%-5d\n42\n",
            "42   ",
            "",
        ),
    ],
    reference=format_string,
    reference_cases=[
        String2String(
            "Just text\n",
            "Just text",
            "",
        ),
        String2String(
            "%3d %3d\n1\n23\n",
            "  1  23",
            "",
        ),
        String2String(
            "%-3d %-3d\n1\n23\n",
            "1   23 ",
            "",
        ),
        String2String(
            "No placeholders\n123\n",
            "No placeholders",
            "123\n",
        ),
        String2String(
            "Missing int %d\n",
            [-1],
            "",
        ),
        String2String(
            "Bad int %d\nabc\n",
            [-1],
            "abc\n",
        ),
        String2String(
            "Negative %-4d\n-5\n",
            "Negative -5  ",
            "",
        ),
        String2String(
            "Zero %3d\n0\n",
            "Zero   0",
            "",
        ),
        String2String(
            "Max 32-bit: %d\n2147483647\n",
            "Max 32-bit: 2147483647",
            "",
        ),
        String2String(
            "Min 32-bit: %d\n-2147483648\n",
            "Min 32-bit: -2147483648",
            "",
        ),
        String2String(
            "Large negative: %-15d\n-1000000000\n",
            "Large negative: -1000000000    ",
            "",
        ),
        String2String(
            "32-bit boundary: %d %d\n2147483647\n-2147483648\n",
            "32-bit boundary: 2147483647 -2147483648",
            "",
        ),
        String2String(
            "Beyond 32-bit: %d\n2147483648\n",
            [-1],
            "2147483648\n",
        ),
        String2String(
            "Below 32-bit: %d\n-2147483649\n",
            [-1],
            "-2147483649\n",
        ),
        String2String(
            "Edge case: %d\n2147483647\nExtra\n",
            "Edge case: 2147483647",
            "Extra\n",
        ),
        String2String(
            "Edge case: %d\n-2147483648\nExtra\n",
            "Edge case: -2147483648",
            "Extra\n",
        ),
        String2String(
            "Multiple valid: %d %d\n0\n2147483647\n",
            "Multiple valid: 0 2147483647",
            "",
        ),
        String2String(
            "Multiple invalid: %d %d\n2147483647\n2147483648\n",
            [-1],
            "2147483648\n",
        ),
        String2String(
            "This format string is too long for input buffer %d\n1\n",
            [-1],
            "This format string is too long for input buffer %d\n1\n",
        ),
        String2String(
            "Long: %d %d %d %d %d\n1\n2\n3\n4\n5\n",
            "Long: 1 2 3 4 5",
            "",
        ),
        String2String(
            "Empty\n",
            "Empty",
            "",
        ),
        String2String(
            "%d\n10\nLeftover\n",
            "10",
            "Leftover\n",
        ),
    ],
    is_variant=True,
    category="Complex Tasks",
)
