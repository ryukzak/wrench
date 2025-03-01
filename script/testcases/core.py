from collections import namedtuple

TEST_CASES = {}


min_int32 = -2_147_483_648
max_int32 = 2_147_483_647
overflow_error_value = -858993460  # 0xCCCCCCCC

# Define the named tuple structure
TestCase = namedtuple(
    "TestCase",
    ["simple", "cases", "reference", "reference_cases", "is_variant", "category"],
)


def py_str(s):
    s = repr(s).replace("\\x00", "\\0")
    return s


def yaml_symbol_nums(s, sep=","):
    return "[" + sep.join([str(ord(c)) for c in s]) + "]"


def yaml_symbols(s):
    if s == [-1] or s == [overflow_error_value]:
        return '"?"'
    s = repr(s).strip("'").replace("\\x00", "\\0").replace("\\x0A", "\\n")
    for code in [repr(chr(i)).strip("'") for i in range(32)]:
        if code == "\\n":
            continue
        s = s.replace(code, "?")
    return '"' + s + '"'


def hex_byte(x):
    return f"{x:02x}"


def dump_symbols(s):
    return " ".join([hex_byte(ord(c)) for c in s])


def limit_to_int32(f):
    def foo(*args, **kwargs):
        tmp = f(*args, **kwargs)
        if min_int32 <= tmp <= max_int32:
            return tmp
        return overflow_error_value

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
                f"      numio[0x84]: [] >>> [{','.join(map(str, self.ys))}]",
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
            f"{f.__name__}({self.x}) actual: {f(self.x)}, expect: {self.y}"
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
    def __init__(self, input, output, rest="", mem_view=[]):
        self.input = input
        self.output = output
        self.rest = rest
        for a, b, dump in mem_view:
            # Interval inclusive, so we need +1
            assert len(dump) == b - a + 1, (
                f"incorrect dump length, actual: {len(dump)}, expect: {b - a + 1}"
            )
        self.mem_view = mem_view

    def assert_string(self, name):
        res = f"assert {name}({py_str(self.input)}) == ({py_str(self.output)}, {py_str(self.rest)})"
        if len(self.mem_view) > 0:
            res += "\n# and " + ", ".join(
                [f"mem[{a}..{b}]: {dump_symbols(dump)}" for a, b, dump in self.mem_view]
            )
        return res

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
            + [f"      {{memory:{a}:{b}}}" for a, b, _ in self.mem_view]
        )

    def yaml_assert(self):
        return "\n".join(
            [
                f"      numio[0x80]: {yaml_symbol_nums(self.rest)} >>> []",
                f"      numio[0x84]: [] >>> {yaml_symbol_nums(self.output) if isinstance(self.output, str) else self.output}",
                f'      symio[0x80]: {yaml_symbols(self.rest)} >>> ""',
                f'      symio[0x84]: "" >>> {yaml_symbols(self.output)}',
            ]
            + [
                f"      mem[{a}..{b}]: \t{dump_symbols(dump)}"
                for a, b, dump in self.mem_view
            ]
        )
