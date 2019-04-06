#!/usr/bin/env python3
import pytest
from printer import *


def test_print_number():
    pr = PrettyPrinter()
    assert Number(0).accept(pr) == "0;"
    pr.depth = 1
    assert Number(1000).accept(pr) == "    1000;"


def test_print_func_def():
    empty = FunctionDefinition("empty", Function([], []))
    foo = FunctionDefinition("foo", Function(["a", "b", "c"], [Number(15)]))
    pr = PrettyPrinter()
    assert empty.accept(pr) == "def empty() {\n}"
    pr.depth = 1
    assert foo.accept(pr) == "    def foo(a, b, c) {\n        15;\n    }"


if __name__ == "__main__":
    pytest.main()
