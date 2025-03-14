from testcases.core import (
    TestCase,
    overflow_error_value,
    String2String,
    TEST_CASES,
    read_line,
    pbuf,
    cbuf,
    cstr,
)
import itertools


###########################################################


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


TEST_CASES["hello_user_pstr"] = TestCase(
    simple=hello_user_pstr,
    cases=[
        String2String(
            "Alice\n",
            "What is your name?\nHello, Alice!",
            "",
            mem_view=[(0x00, 0x1F, pbuf("Hello, Alice!", 0x20))],
        ),
        String2String(
            "Bob\n",
            "What is your name?\nHello, Bob!",
            "",
            mem_view=[(0x00, 0x1F, pbuf("Hello, Bob!", 0x20))],
        ),
    ],
    reference=hello_user_pstr,
    reference_cases=[
        String2String(
            "Alice\nBob",
            "What is your name?\nHello, Alice!",
            "Bob",
            mem_view=[(0x00, 0x1F, pbuf("Hello, Alice!", 0x20))],
        ),
        String2String(
            "1234567890123456789012\n",
            "What is your name?\nHello, 1234567890123456789012!",
            "",
            mem_view=[(0x00, 0x1F, pbuf("Hello, 1234567890123456789012!", 0x20))],
        ),
        String2String(
            "12345678901234567890123\n",
            ["What is your name?\n", overflow_error_value],
            "\n",
        ),
    ],
    is_variant=True,
    category="String Manipulation",
)

###########################################################


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


TEST_CASES["hello_user_cstr"] = TestCase(
    simple=hello_user_cstr,
    cases=[
        String2String(
            "Alice\n",
            "What is your name?\nHello, Alice!",
            "",
            mem_view=[(0x00, 0x1F, cbuf("Hello, Alice!", 0x20))],
        ),
        String2String(
            "Bob\n",
            "What is your name?\nHello, Bob!",
            "",
            mem_view=[(0x00, 0x1F, cbuf("Hello, Bob!", 0x20))],
        ),
    ],
    reference=hello_user_cstr,
    reference_cases=[
        String2String(
            "Alice\nBob",
            "What is your name?\nHello, Alice!",
            "Bob",
            mem_view=[(0x00, 0x1F, cbuf("Hello, Alice!", 0x20))],
        ),
        String2String(
            "1234567890123456789012\n",
            "What is your name?\nHello, 1234567890123456789012!",
            "",
            mem_view=[(0x00, 0x1F, cbuf("Hello, 1234567890123456789012!", 0x20))],
        ),
        String2String(
            "12345678901234567890123\n",
            ["What is your name?\n", overflow_error_value],
            "\n",
        ),
        String2String(
            "1234567890\x0012345\n",
            "What is your name?\nHello, 1234567890!",
            "",
            mem_view=[(0x00, 0x12, cbuf("Hello, 1234567890!", 0x13))],
        ),
    ],
    is_variant=True,
    category="String Manipulation",
)

###########################################################


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


TEST_CASES["upper_case_pstr"] = TestCase(
    simple=upper_case_pstr,
    cases=[
        String2String(
            "Hello\n",
            "HELLO",
            "",
            mem_view=[(0x00, 0x1F, pbuf("HELLO", 0x20))],
        ),
        String2String(
            "world\n",
            "WORLD",
            "",
            mem_view=[(0x00, 0x1F, pbuf("WORLD", 0x20))],
        ),
    ],
    reference=upper_case_pstr,
    reference_cases=[
        String2String(
            "Hello World!\n",
            "HELLO WORLD!",
            "",
            mem_view=[(0x00, 0x1F, pbuf("HELLO WORLD!", 0x20))],
        ),
        String2String(
            "Hello\nworld",
            "HELLO",
            "world",
            mem_view=[(0x00, 0x1F, pbuf("HELLO", 0x20))],
        ),
        String2String(
            "1234567890123456789012345678901\n23",
            "1234567890123456789012345678901",
            "23",
            mem_view=[(0x00, 0x1F, pbuf("1234567890123456789012345678901", 0x20))],
        ),
        String2String(
            "12345678901234567890123456789012\n3", [overflow_error_value], "\n3"
        ),
    ],
    is_variant=True,
    category="String Manipulation",
)

###########################################################


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


TEST_CASES["upper_case_cstr"] = TestCase(
    simple=upper_case_cstr,
    cases=[
        String2String(
            "Hello\n",
            "HELLO",
            "",
            mem_view=[(0x00, 0x1F, cbuf("HELLO", 0x20))],
        ),
        String2String(
            "world\n",
            "WORLD",
            "",
            mem_view=[(0x00, 0x1F, cbuf("WORLD", 0x20))],
        ),
    ],
    reference=upper_case_cstr,
    reference_cases=[
        String2String(
            "Hello World!\n",
            "HELLO WORLD!",
            "",
            mem_view=[(0x00, 0x1F, cbuf("HELLO WORLD!", 0x20))],
        ),
        String2String(
            "Hello\nworld",
            "HELLO",
            "world",
            mem_view=[(0x00, 0x1F, cbuf("HELLO", 0x20))],
        ),
        String2String(
            "1234567890123456789012345678901\n23",
            "1234567890123456789012345678901",
            "23",
            mem_view=[(0x00, 0x1F, cbuf("1234567890123456789012345678901", 0x20))],
        ),
        String2String(
            "12345678901234567890123456789012\n3", [overflow_error_value], "\n3"
        ),
        String2String(
            "1234567890\x0012345\n",
            "1234567890",
            "",
            mem_view=[(0x00, 0x1F, cbuf("1234567890\x0012345", 0x20))],
        ),
    ],
    is_variant=True,
    category="String Manipulation",
)

###########################################################


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


TEST_CASES["capital_case_pstr"] = TestCase(
    simple=capital_case_pstr,
    cases=[
        String2String(
            "hello world\n",
            "Hello World",
            mem_view=[(0x00, 0x1F, pbuf("Hello World", 0x20))],
        ),
        String2String(
            "python programming\n",
            "Python Programming",
            mem_view=[(0x00, 0x1F, pbuf("Python Programming", 0x20))],
        ),
    ],
    reference=capital_case_pstr,
    reference_cases=[
        String2String(
            "hello\nworld!\n",
            "Hello",
            "world!\n",
            mem_view=[(0x00, 0x1F, pbuf("Hello", 0x20))],
        ),
        String2String(
            "HELLO WORLD!\n",
            "Hello World!",
            mem_view=[(0x00, 0x1F, pbuf("Hello World!", 0x20))],
        ),
        String2String(
            "1234567890123456789012345678901\n",
            "1234567890123456789012345678901",
            mem_view=[(0x00, 0x1F, pbuf("1234567890123456789012345678901", 0x20))],
        ),
        String2String(
            "12345678901234567890123456789012\n",
            [overflow_error_value],
            "\n",
        ),
    ],
    is_variant=True,
    category="String Manipulation",
)

###########################################################


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


TEST_CASES["capital_case_cstr"] = TestCase(
    simple=capital_case_cstr,
    cases=[
        String2String(
            "hello world\n",
            "Hello World",
            mem_view=[(0x00, 0x1F, cbuf("Hello World", 0x20))],
        ),
        String2String(
            "python programming\n",
            "Python Programming",
            mem_view=[(0x00, 0x1F, cbuf("Python Programming", 0x20))],
        ),
    ],
    reference=capital_case_cstr,
    reference_cases=[
        String2String(
            "hello\nworld!\n",
            "Hello",
            "world!\n",
            mem_view=[(0x00, 0x1F, cbuf("Hello", 0x20))],
        ),
        String2String(
            "HELLO WORLD!\n",
            "Hello World!",
            mem_view=[(0x00, 0x1F, cbuf("Hello World!", 0x20))],
        ),
        String2String(
            "1234567890123456789012345678901\n",
            "1234567890123456789012345678901",
            mem_view=[(0x00, 0x1F, cbuf("1234567890123456789012345678901", 0x20))],
        ),
        String2String(
            "12345678901234567890123456789012\n",
            [overflow_error_value],
            "\n",
        ),
        String2String(
            "1234567890\x0012345\n",
            "1234567890",
            "",
            mem_view=[(0x00, 0x1F, cbuf("1234567890\x0012345", 0x20))],
        ),
    ],
    is_variant=True,
    category="String Manipulation",
)

###########################################################


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


TEST_CASES["reverse_string_pstr"] = TestCase(
    simple=reverse_string_pstr,
    cases=[
        String2String("hello\n", "olleh", mem_view=[(0x00, 0x1F, pbuf("olleh", 0x20))]),
        String2String(
            "world!\n", "!dlrow", mem_view=[(0x00, 0x1F, pbuf("!dlrow", 0x20))]
        ),
    ],
    reference=reverse_string_pstr,
    reference_cases=[
        String2String("\n", "", mem_view=[(0x00, 0x1F, pbuf("", 0x20))]),
        String2String(
            "1234567890123456789012345678901\n23",
            "1098765432109876543210987654321",
            "23",
            mem_view=[(0x00, 0x1F, pbuf("1098765432109876543210987654321", 0x20))],
        ),
        String2String(
            "12345678901234567890123456789012\n3", [overflow_error_value], "\n3"
        ),
    ],
    is_variant=True,
    category="String Manipulation",
)

###########################################################


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


TEST_CASES["reverse_string_cstr"] = TestCase(
    simple=reverse_string_cstr,
    cases=[
        String2String("hello\n", "olleh", mem_view=[(0x00, 0x1F, cbuf("olleh", 0x20))]),
        String2String(
            "world!\n", "!dlrow", mem_view=[(0x00, 0x1F, cbuf("!dlrow", 0x20))]
        ),
    ],
    reference=reverse_string_cstr,
    reference_cases=[
        String2String("\n", "", mem_view=[(0x00, 0x1F, cbuf("", 0x20))]),
        String2String(
            "1234567890123456789012345678901\n23",
            "1098765432109876543210987654321",
            "23",
            mem_view=[(0x00, 0x1F, cbuf("1098765432109876543210987654321", 0x20))],
        ),
        String2String(
            "12345678901234567890123456789012\n3", [overflow_error_value], "\n3"
        ),
        String2String(
            "1234567890\x0012345\n",
            "54321",
            "",
            mem_view=[(0x00, 0x1F, cbuf("54321\x000987654321", 0x20))],
        ),
    ],
    is_variant=True,
    category="String Manipulation",
)
