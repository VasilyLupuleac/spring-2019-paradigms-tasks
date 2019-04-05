#!/usr/bin/env python3
from model import *


class PrettyPrinter(ASTNodeVisitor):
    def __init__(self):
        self.string = ""

    def visit_number(self, num):
        pass

    def visit_func(self, func):
        pass

    def visit_func_def(self, func_def):
        pass

    def visit_cond(self, cond):
        pass

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
