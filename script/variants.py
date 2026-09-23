#!/usr/bin/env python3.14
from __future__ import annotations

import copy
import inspect
import itertools
import os
import random
from collections.abc import Iterator
from typing import Any

import testcases.bitwise
import testcases.complex
import testcases.examples
import testcases.mathematics
import testcases.string
import testcases.vliw  # noqa: F401
from testcases.core import (
    TEST_CASES,
    Case,
    TestCase,
    cbuf,
    cstr,
    pbuf,
    pstr,
    py_str,
    read_line,
)


def python_assert_string(
    name: str, params: dict[str, Any], results: dict[str, Any]
) -> str:
    if "word" in params and len(params) == 1:
        py_params = f"word={params['word']}"
    elif "symbols" in params and len(params) == 1:
        py_params = f"symbols={py_str(params['symbols'])}"
    elif len(params) == 0:
        py_params = ""
    else:
        raise ValueError(f"incorrect params: {params}")

    if "word" in results and len(results) == 1:
        py_results = results
    elif "symbols" in results and "rest_input" in results and len(results) == 2:
        py_results = f"{{'symbols': {py_str(results['symbols'])}, 'rest_input': {py_str(results['rest_input'])}}}"
    else:
        raise ValueError(f"incorrect results: {results}")

    return f"assert {name}({py_params}) == {py_results}"


def generate_python_test_cases(fname: str, cases: list[Case]) -> str:
    return "\n".join([case.assert_string(fname) for case in cases])


variant_readme_description = (
    """
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
"""
    + "\n\n".join(inspect.getsource(e) for e in [read_line, cstr, pstr, cbuf, pbuf])
    + "```\n"
)


def get_categories(cases: dict[str, TestCase]) -> dict[str, list[str]]:
    categories: dict[str, list[str]] = {}
    for name, variant in sorted(TEST_CASES.items()):
        if variant.category not in categories:
            categories[variant.category] = []
        categories[variant.category].append(name)
    return categories


def generate_variant_readme() -> str:
    res = ["# Wrench variants", variant_readme_description]
    res.append("Variants:")
    res.append("")

    categories = get_categories(TEST_CASES)

    for category, names in sorted(categories.items()):
        res.append(f"- {category}")
        for name in names:
            res.append(f"    - [{name}](#{name})")

    res.append("")

    for category, names in sorted(categories.items()):
        res.append(f"## {category}")
        res.append("")
        for name in names:
            variant = TEST_CASES[name]
            res.append(f"### `{name}`")
            res.append("")
            res.append("```python")
            res.append(inspect.getsource(variant.simple))
            res.append("")
            res.append(
                generate_python_test_cases(variant.simple.__name__, variant.cases)
            )
            res.append("```")
            res.append("")
    return "\n".join(res)


def run_python_test_cases(verbose: bool) -> None:
    for variant in TEST_CASES.values():
        for case in variant.cases:
            if verbose:
                print(case.assert_string(variant.simple.__name__))
            case.check_assert(variant.simple)
        for case in itertools.chain(*[variant.cases, variant.reference_cases]):
            if verbose:
                print(case.assert_string(variant.reference.__name__))
            case.check_assert(variant.reference)


def generate_wrench_test_cases(conf_name: str, case: Case) -> str:
    conf_name = case.assert_string(conf_name)
    return f"""name: "{conf_name}"
limit: {case.limit}
memory_size: 0x1000
memory_mapped_io:
{case.yaml_memory_mapped_io()}
reports:
  - name: Check results
    slice: last
    filter:
      - state
    view: |
{case.yaml_view()}
    assert: |
{case.yaml_assert()}
"""


###########################################################


def write_test_cases(path: str, name: str, variant: TestCase) -> None:
    os.makedirs(f"{path}/{name}", exist_ok=True)
    tests = variant.cases + variant.reference_cases
    for idx, case in enumerate(tests, 1):
        fn = f"{path}/{name}/{idx}.yaml"
        with open(fn, "w") as f:
            print("Write:", fn)
            f.write(generate_wrench_test_cases(name, case))


def generate_wrench_spec(path: str, test_names: list[str]) -> None:
    for name, variant in list(TEST_CASES.items()):
        if name not in test_names:
            continue
        write_test_cases(path, name, variant)


def generate_wrench_variant_test_cases(path: str) -> None:
    for name, variant in list(TEST_CASES.items()):
        os.makedirs(f"{path}/{name}", exist_ok=True)
        tests = variant.cases + variant.reference_cases
        for idx, case in enumerate(tests, 1):
            fn = f"{path}/{name}/{idx}.yaml"
            with open(fn, "w") as f:
                print(fn)
                f.write(generate_wrench_test_cases(name, case))


def inf_shuffle(xs: list[str]) -> Iterator[str]:
    while True:
        buf = copy.copy(xs)
        random.shuffle(buf)
        yield from buf


def gen_variants(
    cases: dict[str, TestCase],
) -> Iterator[tuple[str, str, str, str, str, str]]:
    categories = get_categories(cases)
    yield "acc32", "f32a", "risc-iv", "m68k", "vliw", "scheme"
    for string, bit, math, complex, vliw, schema in zip(
        inf_shuffle(categories["String Manipulation"]),
        inf_shuffle(categories["Bitwise Operations"]),
        inf_shuffle(categories["Mathematics"]),
        inf_shuffle(categories["Complex Tasks"]),
        inf_shuffle(categories["VLIW"]),
        inf_shuffle(
            [
                "acc32-neumann[-microcode]",
                "acc32-neumann[-pipeline-2]",
                "acc32-neumann[-cache]",
                "acc32-harv[-microcode]",
                "acc32-harv[-pipeline-2]",
                "acc32-harv[-dcache]",
                "m68k-neumann[-microcode]",
                "m68k-neumann[-pipeline-2]",
                "m68k-neumann[-cache]",
                "m68k-harv[-microcode]",
                "m68k-harv[-pipeline-2]",
                "m68k-harv[-dcache]",
                "f32a-neumann[-microcode]",
                "f32a-neumann[-pipeline-2]",
                "f32a-neumann[-cache]",
                "f32a-harv[-microcode]",
                "f32a-harv[-pipeline-2]",
                "f32a-harv[-dcache]",
                "risc-iv-32-neumann[-microcode]",
                "risc-iv-32-neumann[-pipeline-3]",
                "risc-iv-32-neumann[-pipeline-5]",
                "risc-iv-32-neumann[-cache]",
            ]
        ),
    ):
        basic = [string, bit, math]
        random.shuffle(basic)
        yield *basic, complex, vliw, schema


def generate_variants(n: int, fn: str) -> None:
    variants = list(itertools.islice(gen_variants(TEST_CASES), n + 1))
    distribution: dict[tuple[str, ...], int] = {}
    for row in variants:
        distribution[row] = distribution.get(row, 0) + 1
    grouped_by_rep: dict[int, int] = {}
    for v in distribution.values():
        grouped_by_rep[v] = grouped_by_rep.get(v, 0) + 1
    print("Generate random variants to csv file:", grouped_by_rep)
    with open(fn, "w") as f:
        for row in variants:
            f.write(",".join(row) + "\n")


if __name__ == "__main__":
    verbose = True
    run_python_test_cases(verbose=verbose)

    print("Generate golden tests:")
    generate_wrench_spec(
        "test/golden/generated",
        ["factorial", "get_put_char", "hello", "logical_not", "dup"],
    )

    print("Generate variant descriptions")
    with open("variants.md", "w") as f:
        print("Write: variants.md")
        f.write(generate_variant_readme())

    print("Generate variant tests")
    generate_wrench_variant_test_cases("variants")

    generate_variants(400, "variants.csv")
