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
    empty = FunctionCall(Reference("empty"), [])
    foo = FunctionCall(Reference("foo"), [Number(1), Number(2), Number(3)])
    pr = PrettyPrinter()
    assert empty.accept(pr) == "empty();"
    pr.depth = 1
    assert foo.accept(pr) == "    foo(1, 2, 3);"


def test_print_ref():
    x = Reference("x")
    pr = PrettyPrinter()
    assert x.accept(pr) == "x;"
    pr.depth = 1
    assert x.accept(pr) == "    x;"


def test_print_bin_op():
    add = BinaryOperation(Number(2), "+", Number(3))
    mul = BinaryOperation(Number(1), "*", add)
    pr = PrettyPrinter()
    assert mul.accept(pr) == "((1) * ((2) + (3)));"
    pr.depth = 1
    assert add.accept(pr) == "    ((2) + (3));"


def test_print_bin_op():
    pr = PrettyPrinter()
    assert UnaryOperation("-", Number(3)).accept(pr) == "(-(3));"
    pr.depth = 1
    assert UnaryOperation("!", Number(10)).accept(pr) == "    (!(10));"


def test_end_to_end(capsys):
    pretty_print(FunctionDefinition("main", Function(["arg1"], [
        Read("x"),
        Print(Reference("x")),
        Conditional(
            BinaryOperation(Number(2), "==", Number(3)),
            [
                Conditional(Number(1), [], [])
            ],
            [
                FunctionCall(Reference("exit"), [
                    UnaryOperation("-", Reference("arg1"))
                ])
            ],
        ),
    ])))
    out, err = capsys.readouterr()
    assert not err
    assert out == ("def main(arg1) {\n    read x;\n" + "    print x;\n" +
                   "    if (((2) == (3))) {\n" + "        if (1) {\n" +
                   "        }\n" + "    } else {\n" +
                   "        exit((-(arg1)));\n" + "    }\n" + "}\n")


if __name__ == "__main__":
    pytest.main()
