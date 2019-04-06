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


def test_print_cond():
    empty = Conditional(Number(42), [], [])
    cond = Conditional(Number(0), [Number(1), Number(10)], [Number(-1)])
    pr = PrettyPrinter()
    assert cond.accept(pr) == "if (0) {\n    1;\n    10;\n} else {\n    -1;\n}"
    pr.depth = 1
    assert empty.accept(pr) == "    if (42) {\n    }"


def test_print_print():
    print_42 = Print(Number(42))
    pr = PrettyPrinter()
    assert print_42.accept(pr) == "print 42;"
    pr.depth = 2
    assert print_42.accept(pr) == "        print 42;"


def test_print_read():
    read_x = Read("x")
    pr = PrettyPrinter()
    assert read_x.accept(pr) == "read x;"
    pr.depth = 1
    assert read_x.accept(pr) == "    read x;"


def test_print_func_call():
    pass


def test_print_ref():
    x = Reference("x")
    pr = PrettyPrinter()
    assert x.accept(pr) == "x;"
    pr.depth = 1
    assert x.accept(pr) == "    x;"


if __name__ == "__main__":
    pytest.main()
