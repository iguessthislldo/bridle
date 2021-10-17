import unittest

import bridle
from bridle.tree import ConstantNode, PrimitiveNode, PrimitiveKind

idl_parser = bridle.IdlParser()


class ConstantsTests(unittest.TestCase):

    # TODO: Test for error when literal is inconsistent with declared type
    # TODO: Test for error when value is too large for integer type

    def check(self, tree, name, value, primitive_kind, limit=None):
        node = tree.get(name)
        self.assertIsInstance(node, ConstantNode)
        self.assertIsInstance(node.primitive_node, PrimitiveNode)
        self.assertEqual(node.primitive_node.kind, primitive_kind)
        self.assertEqual(node.primitive_node.element_count_limit, limit)
        self.assertEqual(node.value, value)

    def test_int_literals(self):
        tree = idl_parser.parse(direct_inputs=['''\
            const uint8 zero = 0;
            const short dec = 103;
            const int16 oct = 07;
            const unsigned long hex1 = 0xFF;
            const uint32 hex2 = 0Xff;
            '''])[0]
        self.check(tree, 'zero', 0, PrimitiveKind.u8)
        self.check(tree, 'dec', 103, PrimitiveKind.i16)
        self.check(tree, 'oct', 0o7, PrimitiveKind.i16)
        self.check(tree, 'hex1', 0xff, PrimitiveKind.u32)
        self.check(tree, 'hex2', 0xff, PrimitiveKind.u32)

    def test_boolean_literals(self):
        tree = idl_parser.parse(direct_inputs=['''\
            const boolean istrue = TRUE;
            const boolean isfalse = FALSE;
            '''])[0]
        self.check(tree, 'istrue', True, PrimitiveKind.boolean)
        self.check(tree, 'isfalse', False, PrimitiveKind.boolean)

    def test_float_literals(self):
        tree = idl_parser.parse(direct_inputs=['''\
            const float float1 = 1.2;
            '''])[0]
        self.check(tree, 'float1', 1.2, PrimitiveKind.f32)
        # TODO: More cases

    def test_char_literals(self):
        tree = idl_parser.parse(direct_inputs=['''\
            const char c1 = 'c';
            '''])[0]
        self.check(tree, 'c1', 'c', PrimitiveKind.c8)
        # TODO: More cases

    @unittest.skip("TODO")
    def test_const_expr_ops_basic(self):
        idl_parser.parse(direct_inputs=['''\
            /*@bridle::assert_value(3)*/
            const short or_value = 2 | 1;
            /*@bridle::assert_value(2)*/
            const short xor_value = 3 ^ 1;
            /*@bridle::assert_value(1)*/
            const short and_value = 3 & 1;
            /*@bridle::assert_value(8)*/
            const short shift_left_value = 1 << 3;
            /*@bridle::assert_value(2)*/
            const short shift_right_value = 16 >> 3;
            /*@bridle::assert_value(0xEF)*/
            const short add_value = 0x0E + 0xE1;
            /*@bridle::assert_value(44)*/
            const short sub_value = 88 - 44;
            /*@bridle::assert_value(18)*/
            const short mul_value = 3 * 6;
            /*@bridle::assert_value(5)*/
            const short div_value = 25 / 5;
            /*@bridle::assert_value(1)*/
            const short rem_value = 33 % 2;
            /*@bridle::assert_value(364)*/
            const short paren_value = (364);
            /*@bridle::assert_value(10)*/
            const short negative_negative_value = -(-10);
            /*@bridle::assert_value(-10)*/
            const short positive_positive_value = +(-10);
            /*@bridle::assert_value(-4)*/
            const short inverse_value = ~3;
            /*@bridle::assert_value(-4)*/
            const float inverse_value = ~3;
            '''])

    @unittest.skip("TODO")
    def test_const_expr_ops(self):
        idl_parser.parse(direct_inputs=['''\
            /*@bridle::assert_value(9)*/
            const short expr1 = 1 + 2 + 3 + 4 - 1;
            /*@bridle::assert_value(10)*/
            const short expr2 = 4 * 2 + 6 / 2 - 7 % 6;
            /*@bridle::assert_value(3)*/
            const short expr3 = (4 * (2 + 6) / 2 - 7) % 6;
            '''])

    # TODO
    # @unittest.expectedFailure
    def test_const_expr_references(self):
        idl_parser.parse(direct_inputs=['''\
            const short const1 = 0;
            /*@bridle::assert_value(0)*/
            const short const2 = const1;
            '''])
