import unittest

from bridle.errors import InternalError, ConstExprError
from bridle.idl_const_expr import Op, ConstExpr, ConstValue
from bridle.tree import PrimitiveKind


def V(arg1, arg2=None):
    if arg2 is None:
        if isinstance(arg1, int):
            return ConstValue(arg1, PrimitiveKind.i64)
        elif isinstance(arg1, (ConstExpr, ConstValue)):
            return arg1
        raise TypeError(repr(type(arg1)))
    return ConstValue(arg1, arg2)


def E(kind, *operands):
    return ConstExpr(kind, *[V(i) for i in operands])


class ConstExprTests(unittest.TestCase):
    def test_wrong_operand_count_causes_error(self):
        with self.assertRaisesRegex(InternalError, 'ADD expects 2 operands, got 0'):
            ConstExpr(Op.ADD)

    def h(self, expr, expected_result):
        expected_result = V(expected_result)
        self.assertEqual(expr.eval(expected_result.kind), expected_result.value)

    def test_or(self):
        self.h(E(Op.OR, 2, 6), 6)

    def test_xor(self):
        self.h(E(Op.XOR, 3, 2), 1)

    def test_and(self):
        self.h(E(Op.AND, 2, 3), 2)

    def test_rshift(self):
        self.h(E(Op.RSHIFT, 6, 1), 3)

    def test_lshift(self):
        self.h(E(Op.LSHIFT, 3, 1), 6)

    def test_add(self):
        self.h(E(Op.ADD, 7, 8), 15)

    def test_subtract(self):
        self.h(E(Op.SUBTRACT, 15, 7), 8)

    def test_multiply(self):
        self.h(E(Op.MULTIPLY, 4, 5), 20)

    def test_divide(self):
        self.h(E(Op.DIVIDE, 20, 4), 5)

    def test_modulo(self):
        self.h(E(Op.MODULO, 5, 2), 1)

    def test_positive(self):
        # Don't know what this is used for...
        self.h(E(Op.POSITIVE, 3), 3)

    def test_negative(self):
        self.h(E(Op.NEGATIVE, -3), 3)

    def test_invert(self):
        self.h(E(Op.INVERT, -1), 0)
        self.h(E(Op.INVERT, 0xfe), V(0x01, PrimitiveKind.u8))

    def test_prioritize(self):
        self.h(E(Op.PRIORITIZE, 1), 1)  # TODO

    def test_expr1(self):
        self.h(E(Op.SUBTRACT, E(Op.MULTIPLY, 5, 4), E(Op.ADD, 2, 1)), 17)

    def test_invalid_range(self):
        with self.assertRaisesRegex(ConstExprError, '300 is outside valid range for u8'):
            self.h(E(Op.MULTIPLY, 30, 10), V(0, PrimitiveKind.u8))

    # TODO: More Complete Tests
