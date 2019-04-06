#!/usr/bin/env python3
import abc


class Scope:
    def __init__(self, parent=None):
        self.parent = parent
        self.local_vars = {}

    def __getitem__(self, var_name):
        if var_name in self.local_vars:
            return self.local_vars[var_name]
        if not self.parent:
            raise KeyError(var_name)
        return self.parent[var_name]

    def __setitem__(self, var_name, value):
        self.local_vars[var_name] = value


class ASTNode(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def evaluate(self, scope):
        pass

    @abc.abstractmethod
    def accept(self, visitor):
        pass


class ASTNodeVisitor(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def visit_num(self, num):
        pass

    @abc.abstractmethod
    def visit_func(self, func):
        pass

    @abc.abstractmethod
    def visit_func_def(self, func_def):
        pass

    @abc.abstractmethod
    def visit_cond(self, cond):
        pass

    @abc.abstractmethod
    def visit_print(self, pr):
        pass

    @abc.abstractmethod
    def visit_read(self, read):
        pass

    @abc.abstractmethod
    def visit_func_call(self, func_call):
        pass

    @abc.abstractmethod
    def visit_ref(self, ref):
        pass

    @abc.abstractmethod
    def visit_bin_op(self, bin_op):
        pass

    @abc.abstractmethod
    def visit_un_op(self, un_op):
        pass


class Number(ASTNode):
    def __init__(self, value):
        self.value = value

    def evaluate(self, scope):
        return self

    def __eq__(self, other):
        return self.value == other.value

    def __hash__(self):
        return hash(self.value)

    def accept(self, visitor):
        return visitor.visit_num(self)


class Function(ASTNode):
    def __init__(self, args, body):
        self.args = args
        self.body = body

    def evaluate(self, scope):
        return self

    def accept(self, visitor):
        return visitor.visit_func(self)


class FunctionDefinition(ASTNode):
    def __init__(self, name, function):
        self.name = name
        self.function = function

    def evaluate(self, scope):
        scope[self.name] = self.function
        return self.function

    def accept(self, visitor):
        return visitor.visit_func_def(self)


class Conditional(ASTNode):
    def __init__(self, condition, if_true, if_false=None):
        self.condition = condition
        self.if_true = if_true
        self.if_false = if_false

    def evaluate(self, scope):
        condition_res = self.condition.evaluate(scope) != Number(0)
        stmts = self.if_true if condition_res else self.if_false
        res = None
        for stmt in stmts or []:
            res = stmt.evaluate(scope)
        return res

    def accept(self, visitor):
        return visitor.visit_cond(self)


class Print(ASTNode):
    def __init__(self, expr):
        self.expr = expr

    def evaluate(self, scope):
        res = self.expr.evaluate(scope)
        print(res.value)
        return res

    def accept(self, visitor):
        return visitor.visit_print(self)


class Read(ASTNode):
    def __init__(self, name):
        self.name = name

    def evaluate(self, scope):
        value = Number(int(input()))
        scope[self.name] = value
        return value

    def accept(self, visitor):
        return visitor.visit_read(self)


class FunctionCall(ASTNode):
    def __init__(self, fun_expr, args):
        self.fun_expr = fun_expr
        self.args = args

    def evaluate(self, scope):
        func = self.fun_expr.evaluate(scope)
        call_scope = Scope(scope)
        for arg_name, arg_expr in zip(func.args, self.args):
            call_scope[arg_name] = arg_expr.evaluate(scope)
        res = None
        for stmt in func.body or []:
            res = stmt.evaluate(call_scope)
        return res

    def accept(self, visitor):
        return visitor.visit_func_call(self)


class Reference(ASTNode):
    def __init__(self, name):
        self.name = name

    def evaluate(self, scope):
        return scope[self.name]

    def accept(self, visitor):
        return visitor.visit_ref(self)


class BinaryOperation(ASTNode):
    OPS = {
        "+": lambda x, y: x + y,
        "-": lambda x, y: x - y,
        "*": lambda x, y: x * y,
        "/": lambda x, y: x // y,
        "%": lambda x, y: x % y,
        ">": lambda x, y: x > y,
        ">=": lambda x, y: x >= y,
        "<": lambda x, y: x < y,
        "<=": lambda x, y: x <= y,
        "==": lambda x, y: x == y,
        "!=": lambda x, y: x != y,
        "&&": lambda x, y: x and y,
        "||": lambda x, y: x or y
    }

    def __init__(self, lhs, op, rhs):
        if op not in self.OPS:
            raise NotImplementedError(op)
        self.lhs = lhs
        self.rhs = rhs
        self.op = op

    def evaluate(self, scope):
        rh_value = self.rhs.evaluate(scope).value
        lh_value = self.lhs.evaluate(scope).value
        res = self.OPS[self.op](lh_value, rh_value)
        return Number(int(res))

    def accept(self, visitor):
        return visitor.visit_bin_op(self)


class UnaryOperation(ASTNode):
    OPS = {
        "-": lambda x: -x,
        "!": lambda x: not x
    }

    def __init__(self, op, expr):
        if op not in self.OPS:
            raise NotImplementedError(op)
        self.op = op
        self.expr = expr

    def evaluate(self, scope):
        value = self.expr.evaluate(scope).value
        res = self.OPS[self.op](value)
        return Number(int(res))

    def accept(self, visitor):
        return visitor.visit_un_op(self)
