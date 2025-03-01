from testcases.core import (
    TestCase,
    String2String,
    TEST_CASES,
)

###########################################################


def hello_user_pstr(input):
    """Greet the user with Pascal strings.

    Args:
        input (str): The input string containing the user's name.

    Returns:
        tuple: A tuple containing the greeting message and the remaining input.
    """
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

TEST_CASES["hello_user_pstr"] = TestCase(
    simple=hello_user_pstr,
    cases=[
        # TODO: check buffer in memory for all strings.
        #        WithMemory(10, 20, ".......",
        String2String("Alice\n", "What is your name?\nHello, Alice!\n", ""),
        # )
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

    Args:
        input (str): The input string containing the user's name.

    Returns:
        tuple: A tuple containing the greeting message and the remaining input.
    """
    return hello_user_pstr(input)


hello_user_cstr_ref = hello_user_cstr

TEST_CASES["hello_user_cstr"] = TestCase(
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

###########################################################


def upper_case_pstr(s):
    """Convert a Pascal string to upper case.

    Args:
        s (str): The input Pascal string.

    Returns:
        tuple: A tuple containing the upper case string and an empty string.
    """
    return (s.upper(), "")


upper_case_pstr_ref = upper_case_pstr

TEST_CASES["upper_case_pstr"] = TestCase(
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
    """Convert a C string to upper case.

    Args:
        s (str): The input C string.

    Returns:
        tuple: A tuple containing the upper case string and an empty string.
    """
    return upper_case_pstr(s)


upper_case_cstr_ref = upper_case_cstr

TEST_CASES["upper_case_cstr"] = TestCase(
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
    """Convert the first character of each word in a Pascal string to upper case.

    Args:
        s (str): The input Pascal string.

    Returns:
        tuple: A tuple containing the capitalized string and an empty string.
    """
    return (s.title(), "")


capital_case_pstr_ref = capital_case_pstr

TEST_CASES["capital_case_pstr"] = TestCase(
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
    """Convert the first character of each word in a C string to upper case.

    Args:
        s (str): The input C string.

    Returns:
        tuple: A tuple containing the capitalized string and an empty string.
    """
    return capital_case_pstr(s)


capital_case_cstr_ref = capital_case_cstr

TEST_CASES["capital_case_cstr"] = TestCase(
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
    """Reverse a Pascal string.

    Args:
        s (str): The input Pascal string.

    Returns:
        tuple: A tuple containing the reversed string and an empty string.
    """
    return (s[::-1], "")


reverse_string_pstr_ref = reverse_string_pstr

TEST_CASES["reverse_string_pstr"] = TestCase(
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
    """Reverse a C string.

    Args:
        s (str): The input C string.

    Returns:
        tuple: A tuple containing the reversed string and the remaining input.
    """
    ss = tuple(s.split("\n", 2))
    if len(ss) == 1:
        return reverse_string_pstr(s)
    else:
        head, tail = ss
        return head[::-1], tail


reverse_string_cstr_ref = reverse_string_cstr

TEST_CASES["reverse_string_cstr"] = TestCase(
    simple=reverse_string_cstr,
    cases=[
        String2String("hello", "olleh"),
        String2String("world", "dlrow"),
    ],
    reference=reverse_string_cstr_ref,
    reference_cases=[
        String2String("hello\n World", "olleh", " World"),
    ],
    is_variant=True,
    category="String Manipulation",
)
