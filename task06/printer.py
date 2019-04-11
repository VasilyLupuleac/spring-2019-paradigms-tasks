#!/usr/bin/env python3
from model import *


def remove_semicolon(string):
    if string.endswith(";"):
        string = string[:-1]
    return string


class PrettyPrinter(ASTNodeVisitor):
    INDENT = "    "

    def __init__(self):
        self.depth = 0

    def indent(self):
        return self.INDENT * self.depth

    def visit_num(self, num):
        return self.indent() + str(num.value) + ";"

    def visit_func(self, func):
        raise TypeError

    def visit_func_def(self, func_def):
        res_string = self.indent() + "def " + func_def.name + "("
        func = func_def.function
        res_string += ", ".join(func.args)
        res_string += ") {\n"
        self.depth += 1
        for stmt in func.body:
            res_string += stmt.accept(self) + "\n"
        self.depth -= 1
        return res_string + self.indent() + "}"

    def visit_cond(self, cond):
        res_string = self.indent() + "if ("
        condition = cond.condition.accept(PrettyPrinter())
        res_string += remove_semicolon(condition) + ") {\n"
        self.depth += 1
        for stmt in cond.if_true or []:
            res_string += stmt.accept(self) + "\n"
        self.depth -= 1
        res_string += self.indent() + "}"
        if not cond.if_false:
            return res_string
        res_string += " else {\n"
        self.depth += 1
        for stmt in cond.if_false:
            res_string += stmt.accept(self) + "\n"
        self.depth -= 1
        res_string += self.indent() + "}"
        return res_string

    def visit_print(self, pr):
        res_string = self.indent() + "print "
        expr_printer = PrettyPrinter()
        res_string += pr.expr.accept(expr_printer)
        return res_string

    def visit_read(self, read):
        return self.indent() + "read " + read.name + ";"

    def visit_func_call(self, func_call):
        func = func_call.fun_expr
        res_string = remove_semicolon(func.accept(self))
        res_string += "("
        arg_printer = PrettyPrinter()
        formatted_args = [remove_semicolon(arg.accept(arg_printer))
                          for arg in func_call.args]
        res_string += ", ".join(formatted_args)
        res_string += ");"
        return res_string

    def visit_ref(self, ref):
        return self.indent() + ref.name + ";"

    def visit_bin_op(self, bin_op):
        expr_printer = PrettyPrinter()
        res_string = self.indent() + "(("
        res_string += remove_semicolon(bin_op.lhs.accept(expr_printer))
        res_string += ") " + bin_op.op + " ("
        res_string += remove_semicolon(bin_op.rhs.accept(expr_printer))
        return res_string + "));"

    def visit_un_op(self, un_op):
        expr_printer = PrettyPrinter()
        res_string = self.indent() + "("
        res_string += un_op.op + "("
        res_string += remove_semicolon(un_op.expr.accept(expr_printer))
        return res_string + "));"


def pretty_print(program):
    pr = PrettyPrinter()
    print(program.accept(pr))
