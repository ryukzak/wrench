from __future__ import annotations

import itertools
from collections.abc import Callable
from typing import Any, NamedTuple, Protocol

TEST_CASES: dict[str, TestCase] = {}


min_int32 = -2_147_483_648
max_int32 = 2_147_483_647
overflow_error_value = -858993460  # 0xCCCCCCCC


def uint32_to_int32(n: int) -> int:
    if n > max_int32:
        # Subtract 2^32 to get the signed representation
        return n - 0x100000000
    return n


assert uint32_to_int32(2_147_483_647) == 2_147_483_647
assert uint32_to_int32(2_147_483_648) == -2_147_483_648
assert uint32_to_int32(2_147_483_649) == -2_147_483_647


class Case(Protocol):
    """Common shape implemented by the *2* test-case helper classes below."""

    limit: int

    def assert_string(self, name: str) -> str: ...
    def check_assert(self, f: Callable[..., Any]) -> None: ...
    def yaml_memory_mapped_io(self) -> str: ...
    def yaml_view(self) -> str: ...
    def yaml_assert(self) -> str: ...


class TestCase(NamedTuple):
    simple: Callable[..., Any]
    cases: list[Case]
    reference: Callable[..., Any]
    reference_cases: list[Case]
    is_variant: bool
    category: str


def py_str(s: object) -> str:
    s = repr(s).replace("\\x00", "\\0")
    return s


def yaml_symbol_nums_inner(s: int | str, sep: str = ",") -> str:
    if isinstance(s, str):
        return sep.join([str(ord(c)) for c in s])
    return str(s)


def yaml_symbol_nums(s: str | list[int | str], sep: str = ",") -> str:
    if isinstance(s, list):
        return "[" + sep.join(map(yaml_symbol_nums_inner, s)) + "]"
    return "[" + yaml_symbol_nums_inner(s, sep) + "]"


def yaml_symbols_innr(s: int | str) -> str:
    if isinstance(s, int):
        return "\\0" if s == 0 else "?"
    replaces = {"\\x00": "\\0", "\\x0A": "\\n"}

    s = repr(s).strip("'")
    for k, v in replaces.items():
        s = s.replace(k, v)
    for sym in [repr(chr(i)).strip("'") for i in range(32)]:
        if sym == "\\n":
            continue
        s = s.replace(sym, "?")
    return s


def yaml_symbols(s: str | list[int | str]) -> str:
    if isinstance(s, list):
        return '"' + "".join(map(yaml_symbols_innr, s)) + '"'
    return '"' + yaml_symbols_innr(s) + '"'


def hex_byte(x: int) -> str:
    return f"{x:02x}"


def dump_symbols(s: str) -> str:
    return " ".join([hex_byte(ord(c)) for c in s])


def limit_to_int32(f: Callable[..., int]) -> Callable[..., int]:
    def foo(*args: Any, **kwargs: Any) -> int:
        tmp = f(*args, **kwargs)
        if min_int32 <= tmp <= max_int32:
            return tmp
        return overflow_error_value

    foo.__name__ = f.__name__
    return foo


class Words2Words:
    def __init__(
        self,
        xs: list[int],
        ys: list[int],
        rest: list[int] | None = None,
        limit: int = 2000,
    ) -> None:
        if rest is None:
            rest = []
        self.xs = xs
        self.ys = ys
        self.rest = rest
        self.limit = limit

    def assert_string(self, name: str) -> str:
        params = ", ".join(repr(x) for x in self.xs)
        results = repr(self.ys)
        return f"assert {name}({params}) == {results}"

    def check_assert(self, f: Callable[..., Any]) -> None:
        assert f(*self.xs) == self.ys, (
            f"{f.__name__} actual: {f(*self.xs)}, expect: {self.ys}"
        )

    def yaml_memory_mapped_io(self) -> str:
        return "\n".join(
            [
                f"  0x80: {self.xs}",
                "  0x84: []",
            ]
        )

    def yaml_view(self) -> str:
        return "\n".join(
            [
                "      numio[0x80]: {io:0x80:dec}",
                "      numio[0x84]: {io:0x84:dec}",
            ]
        )

    def yaml_assert(self) -> str:
        return "\n".join(
            [
                f"      numio[0x80]: [{','.join(str(uint32_to_int32(x)) for x in self.rest)}] >>> []",
                f"      numio[0x84]: [] >>> [{','.join(str(uint32_to_int32(x)) for x in self.ys)}]",
            ]
        )


class CharSequence2Word(Words2Words):
    def __init__(self, x: str, y: int, limit: int = 2000) -> None:
        super().__init__([ord(it) for it in list(x)], [y], limit=limit)
        self.x = x
        self.y = y

    def assert_string(self, name: str) -> str:
        params = "".join([it if ord(it) > 0 else "\\0" for it in self.x])
        results = f"{self.y}"
        return f"assert {name}('{params}') == {results}"

    def check_assert(self, f: Callable[..., Any]) -> None:
        assert f(self.x) == self.y, (
            f"{f.__name__}({self.x}) actual: {f(self.x)}, expect: {self.y}"
        )


class Word2Word(Words2Words):
    def __init__(self, x: int, y: int, limit: int = 2000) -> None:
        super().__init__([x], [y], limit=limit)
        self.x = x
        self.y = y

    def assert_string(self, name: str) -> str:
        params = f"{self.x}"
        results = f"{self.y}"
        return f"assert {name}({params}) == {results}"

    def check_assert(self, f: Callable[..., Any]) -> None:
        assert f(self.x) == self.y, (
            f"{f.__name__}({self.x}) actual: {f(self.x)}, expect: {self.y}"
        )


class Bool2Bool(Word2Word):
    def __init__(self, x: bool, y: bool, limit: int = 2000) -> None:
        super().__init__(1 if x else 0, 1 if y else 0, limit=limit)

    def assert_string(self, name: str) -> str:
        x = self.x == 1
        y = self.y == 1
        return f"assert {name}({x}) == {y}"

    def check_assert(self, f: Callable[..., Any]) -> None:
        x = self.x == 1
        y = self.y == 1
        assert f(x) == y, f"actual: {f(x)}, expect: {y}"


class String2String:
    def __init__(
        self,
        input: str,
        output: str | list[int | str],
        rest: str = "",
        mem_view: list[tuple[int, int, str]] | None = None,
        limit: int = 2000,
    ) -> None:
        if mem_view is None:
            mem_view = []
        self.input = input
        self.output = output
        self.rest = rest
        self.limit = limit
        for i, (a, b, dump) in enumerate(mem_view):
            # Interval inclusive, so we need +1
            assert len(dump) <= b - a + 1, (
                f"incorrect dump length, actual: {len(dump)}, expect: {b - a + 1}"
            )
            mem_view[i] = (a, b, dump + ("_" * (b - a + 1 - len(dump))))
        self.mem_view = mem_view

    def assert_string(self, name: str) -> str:
        res = f"assert {name}({py_str(self.input)}) == ({py_str(self.output)}, {py_str(self.rest)})"
        if len(self.mem_view) > 0:
            res += "\n# and " + ", ".join(
                [
                    f"mem[0x{a:02x}..0x{b:02x}]: {dump_symbols(dump)}"
                    for a, b, dump in self.mem_view
                ]
            )
        return res

    def check_assert(self, f: Callable[..., Any]) -> None:
        assert f(self.input) == (
            self.output,
            self.rest,
        ), f"actual: {f(self.input)}, expect: {(self.output, self.rest)}"

    def yaml_memory_mapped_io(self) -> str:
        return "\n".join(
            [
                f"  0x80: {yaml_symbol_nums(self.input, ', ')}",
                "  0x84: []",
            ]
        )

    def yaml_view(self) -> str:
        return "\n".join(
            [
                "      numio[0x80]: {io:0x80:dec}",
                "      numio[0x84]: {io:0x84:dec}",
                "      symio[0x80]: {io:0x80:sym}",
                "      symio[0x84]: {io:0x84:sym}",
            ]
            + [f"      {{memory:{a}:{b}}}" for a, b, _ in self.mem_view]
        )

    def yaml_assert(self) -> str:
        return "\n".join(
            [
                f"      numio[0x80]: {yaml_symbol_nums(self.rest)} >>> []",
                f"      numio[0x84]: [] >>> {yaml_symbol_nums(self.output)}",
                f'      symio[0x80]: {yaml_symbols(self.rest)} >>> ""',
                f'      symio[0x84]: "" >>> {yaml_symbols(self.output)}',
            ]
            + [
                f"      mem[0x{a:02x}..0x{b:02x}]: \t{dump_symbols(dump)}"
                for a, b, dump in self.mem_view
            ]
        )


def read_line(s: str, buf_size: int) -> tuple[str | None, str]:
    """Read line from input with buffer size limits."""
    assert "\n" in s, "input should have a newline character"
    line = "".join(itertools.takewhile(lambda x: x != "\n", s))

    if len(line) > buf_size - 1:
        return None, s[buf_size:]

    return line, s[len(line) + 1 :]


assert read_line("\n1234\n567890", 5) == ("", "1234\n567890")
assert read_line("1\n234\n567890", 5) == ("1", "234\n567890")
assert read_line("1234\n567890", 5) == ("1234", "567890")
assert read_line("12345\n67890", 5) == (None, "\n67890")


def pstr(s: str, buf_size: int) -> tuple[str, str]:
    """Make content for buffer with pascal string (default value for cell: `_`)."""
    assert len(s) + 1 <= buf_size
    buf = chr(len(s)) + s + ("_" * (buf_size - len(s) - 1))
    return s, buf


assert pstr("hello", 10) == ("hello", "\x05hello____")


def pbuf(s: str, buf_size: int) -> str:
    return pstr(s, buf_size)[1]


def cstr(s: str, buf_size: int) -> tuple[str, str]:
    """Make content for buffer with C string (default value for cell: `_`)."""
    assert len(s) + 1 <= buf_size
    buf = s + "\0" + ("_" * (buf_size - len(s) - 1))
    return "".join(itertools.takewhile(lambda c: c != "\0", s)), buf


assert cstr("hello", 10) == ("hello", "hello\x00____")


def cbuf(s: str, buf_size: int) -> str:
    return cstr(s, buf_size)[1]
