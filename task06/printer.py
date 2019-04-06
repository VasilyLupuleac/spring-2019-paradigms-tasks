#!/usr/bin/env python3
from model import *


class PrettyPrinter(ASTNodeVisitor):
    IND = "    "

    def __init__(self):
        self.depth = 0

    def indent(self):
        return self.IND * self.depth

    def visit_num(self, num):
        res_string = self.indent() + str(num.value) + ";"
        return res_string

    def visit_func(self, func):
        raise NotImplementedError

    def visit_func_def(self, func_def):
        res_string = self.indent() + "def " + func_def.name + "("
        func = func_def.function
        for arg in func.args:
            res_string += arg + ", "
        if res_string.endswith(", "):
            res_string = res_string[:-2] + ") {\n"
        else:
            res_string += ") {\n"
        self.depth += 1
        for stmt in func.body:
            res_string += stmt.accept(self) + "\n"
        self.depth -= 1
        return res_string + self.indent() + "}"

    def visit_cond(self, cond):
        res_string = self.indent() + "if ("
        cond_printer = PrettyPrinter()
        condition = cond.condition.accept(cond_printer)
        if condition.endswith(";"):
            condition = condition[:-1]
        res_string += condition + ") {\n"
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
        pass

    def visit_read(self, read):
        pass

    def visit_func_call(self, func_call):
        pass

    def visit_ref(self, ref):
        pass

    def visit_bin_op(self, bin_op):
        pass

    def visit_un_op(self, un_op):
        pass


def pretty_print(program):
    pass
