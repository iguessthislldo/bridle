import unittest

from bridle.errors import InternalError
from bridle.idl_const_expr import Op, OpKind


class MockValue:
    def __init__(self, value):
        self.value = value

    def can_eval(self):
        return self.value is not None

    def eval(self):
        return self.value

    def __str__(self):
        return str(self.value)

    def __repr__(self):
        return '<MockValue: {}>'.format(str(self))


class ConstExprTests(unittest.TestCase):
    def test_can_not_eval(self):
        self.assertFalse(Op(OpKind.ADD, MockValue(None), MockValue(None)).can_eval())
        self.assertFalse(Op(OpKind.ADD, MockValue(1), MockValue(None)).can_eval())
        self.assertFalse(Op(OpKind.ADD, MockValue(None), MockValue(1)).can_eval())

    def _op_helper(self, kind, *values):
        v = [i if isinstance(i, Op) else MockValue(i) for i in values]
        print(v)
        op = Op(kind, *v)
        self.assertTrue(op.can_eval())
        return op

    def op_helper(self, kind, expected_result, *values):
        op = self._op_helper(kind, *values)
        self.assertEqual(op.eval(), expected_result)

    def test_or(self):
        self.op_helper(OpKind.OR, 6, 2, 6)

    def test_xor(self):
        self.op_helper(OpKind.XOR, 1, 3, 2)

    def test_and(self):
        self.op_helper(OpKind.AND, 2, 2, 3)

    def test_rshift(self):
        self.op_helper(OpKind.RSHIFT, 3, 6, 1)

    def test_lshift(self):
        self.op_helper(OpKind.LSHIFT, 6, 3, 1)

    def test_add(self):
        self.op_helper(OpKind.ADD, 15, 7, 8)

    def test_subtract(self):
        self.op_helper(OpKind.SUBTRACT, 8, 15, 7)

    def test_multiply(self):
        self.op_helper(OpKind.MULTIPLY, 20, 4, 5)

    def test_divide(self):
        self.op_helper(OpKind.DIVIDE, 5, 20, 4)

    def test_modulo(self):
        self.op_helper(OpKind.MODULO, 1, 5, 2)

    def test_positive(self):
        # Don't know what this is used for...
        self.op_helper(OpKind.POSITIVE, 3, 3)

    def test_negative(self):
        self.op_helper(OpKind.NEGATIVE, -3, 3)

    def test_invert(self):
        self.op_helper(OpKind.INVERT, -1, 0)  # TODO

    def test_prioritize(self):
        self.op_helper(OpKind.PRIORITIZE, 1, 1)  # TODO

    def _expr_helper(self, expr):
        if isinstance(expr, tuple):
            kind, *raw_operands = expr
            operands = [self._expr_helper(i) for i in raw_operands]
            op = self._op_helper(kind, *operands)
            return op
        return expr

    def expr_helper(self, expr, expected_result):
        self.assertEqual(self._expr_helper(expr).eval(), expected_result)

    def test_expr1(self):
        self.expr_helper((OpKind.SUBTRACT, (OpKind.MULTIPLY, 5, 4), (OpKind.ADD, 2, 1)), 17)

    def test_wrong_operand_count_causes_error(self):
        with self.assertRaisesRegex(InternalError, 'Op expects'):
            Op(OpKind.ADD)

    # TODO: More Complete Tests
