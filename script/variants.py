#!/usr/bin/env python3

import itertools
import os
import inspect
import random
from testcases.core import (
    py_str,
    read_line,
    cstr,
    cbuf,
    pbuf,
    pstr,
    TEST_CASES,
)
import testcases.examples  # noqa: F401
import testcases.mathematics  # noqa: F401
import testcases.bitwise  # noqa: F401
import testcases.string  # noqa: F401


def python_assert_string(name, params, results):
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


def generate_python_test_cases(fname, cases):
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
1. ISA-specific requirements:
    - `F32a`: use procedures.

Also we have the following helper functions not from builtins:

```python
"""
    + "\n\n".join(
        map(lambda e: inspect.getsource(e), [read_line, cstr, pstr, cbuf, pbuf])
    )
    + "```\n"
)


def get_categories(cases):
    categories = {}
    for name, variant in sorted(TEST_CASES.items()):
        if variant.category not in categories:
            categories[variant.category] = []
        categories[variant.category].append(name)
    return categories


def generate_variant_readme():
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


def run_python_test_cases(verbose):
    for name, variant in TEST_CASES.items():
        for case in variant.cases:
            if verbose:
                print(case.assert_string(variant.simple.__name__))
            case.check_assert(variant.simple)
        for case in itertools.chain(*[variant.cases, variant.reference_cases]):
            if verbose:
                print(case.assert_string(variant.reference.__name__))
            case.check_assert(variant.reference)


def generate_wrench_test_cases(conf_name, case):
    conf_name = case.assert_string(conf_name)
    return f"""name: {conf_name}
limit: {case.limit}
memory_size: 0x1000
input_streams:
{case.yaml_input_streams()}
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


def write_test_cases(path, name, variant):
    os.makedirs(f"{path}/{name}", exist_ok=True)
    tests = variant.cases + variant.reference_cases
    for idx, case in enumerate(tests, 1):
        fn = f"{path}/{name}/{idx}.yaml"
        with open(fn, "w") as f:
            print("Write:", fn)
            f.write(generate_wrench_test_cases(name, case))


def generate_wrench_spec(path, test_names):
    for name, variant in list(TEST_CASES.items()):
        if name not in test_names:
            continue
        write_test_cases(path, name, variant)


def generate_wrench_variant_test_cases(path):
    for name, variant in list(TEST_CASES.items()):
        os.makedirs(f"{path}/{name}", exist_ok=True)
        tests = variant.cases + variant.reference_cases
        for idx, case in enumerate(tests, 1):
            fn = f"{path}/{name}/{idx}.yaml"
            with open(fn, "w") as f:
                print(fn)
                f.write(generate_wrench_test_cases(name, case))


def inf_shuffle(xs):
    while True:
        i = random.randint(0, len(xs) - 1)
        yield xs[i]


def fun_shuffle(xs):
    xs = list(xs)
    random.shuffle(xs)
    return xs


def gen_variants(cases):
    categories = get_categories(cases)
    for e in zip(
        inf_shuffle(categories["String Manipulation"]),
        inf_shuffle(categories["Bitwise Operations"]),
        inf_shuffle(categories["Mathematics"]),
    ):
        yield fun_shuffle(e)


def generate_variants(n, fn):
    variants = [next(gen_variants(TEST_CASES)) for _ in range(n)]
    distribution = {}
    for a, b, c in variants:
        distribution[(a, b, c)] = distribution.get((a, b, c), 0) + 1
    grouped_by_rep = {}
    for k, v in distribution.items():
        grouped_by_rep[v] = grouped_by_rep.get(v, 0) + 1
    print("Generate random variants to csv file:", grouped_by_rep)
    with open(fn, "w") as f:
        f.write("acc32,f32a,risc-iv-32\n")
        for a, b, c in variants:
            f.write(f"{a},{b},{c}\n")


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

    generate_variants(375, "variants.csv")
