import unittest
from pathlib import Path
import os

import bridle
from bridle.utils.iters import PeekIter, ChainedIter

idl_parser = bridle.IdlParser()
test_path = Path(__file__).parent
opendds_path = Path(os.environ.get('DDS_ROOT', 'OpenDDS'))


# TODO
@unittest.skip("TODO")
class ConstExprTests(unittest.TestCase):

    @unittest.expectedFailure
    def test_expect_assert_value_fail(self):
        idl_parser.parse(direct_input=['''\
            @bridle::assert_value(1)
            const short fail_value = 0;
            '''])

    def test_int_literals(self):
        idl_parser.parse(direct_input=['''\
            @bridle::assert_value(0)
            const short zero = 0;
            @bridle::assert_value(7)
            const short oct = 07;
            @bridle::assert_value(103)
            const short dec = 103;
            @bridle::assert_value(255)
            const short hex1 = 0xFF;
            @bridle::assert_value(255)
            const short hex2 = 0Xff;
            '''])

    def test_boolean_literals(self):
        idl_parser.parse(direct_input=['''\
            @bridle::assert_value(true)
            const boolean boolean1 = true;
            @bridle::assert_value(true)
            const boolean boolean2 = TRUE;
            @bridle::assert_value(false)
            const boolean boolean3 = false;
            @bridle::assert_value(false)
            const boolean boolean4 = false;
            '''])

    def test_float_literals(self):
        # TODO: More Cases
        idl_parser.parse(direct_input=['''\
            @bridle::assert_value(1.2)
            const float float1 = 1.2;
            '''])

    def test_char_literals(self):
        # TODO
        pass

    def test_const_expr_ops_basic(self):
        idl_parser.parse(direct_input=['''\
            @bridle::assert_value(3)
            const short or_value = 2 | 1;
            @bridle::assert_value(2)
            const short xor_value = 3 ^ 1;
            @bridle::assert_value(1)
            const short and_value = 3 & 1;
            @bridle::assert_value(8)
            const short shift_left_value = 1 << 3;
            @bridle::assert_value(2)
            const short shift_right_value = 16 >> 3;
            @bridle::assert_value(0xEF)
            const short add_value = 0x0E + 0xE1;
            @bridle::assert_value(44)
            const short sub_value = 88 - 44;
            @bridle::assert_value(18)
            const short mul_value = 3 * 6;
            @bridle::assert_value(5)
            const short div_value = 25 / 5;
            @bridle::assert_value(1)
            const short rem_value = 33 % 2;
            @bridle::assert_value(364)
            const short paren_value = (364);
            @bridle::assert_value(10)
            const short negative_negative_value = -(-10);
            @bridle::assert_value(-10)
            const short positive_positive_value = +(-10);
            @bridle::assert_value(-4)
            const short inverse_value = ~3;
            @bridle::assert_value(-4)
            const float inverse_value = ~3;
            '''])

    def test_const_expr_ops(self):
        idl_parser.parse(direct_input=['''\
            @bridle::assert_value(9)
            const short expr1 = 1 + 2 + 3 + 4 - 1;
            @bridle::assert_value(10)
            const short expr2 = 4 * 2 + 6 / 2 - 7 % 6;
            @bridle::assert_value(3)
            const short expr3 = (4 * (2 + 6) / 2 - 7) % 6;
            '''])

    # TODO
    @unittest.expectedFailure
    def test_const_expr_references(self):
        idl_parser.parse(direct_input=['''\
            const short const1 = 0;
            @bridle::assert_value(0)
            const short const2 = const1;
            '''])


class IdlFileTests(unittest.TestCase):

    def test_general_test_idl(self):
        idl_parser.parse(
            [test_path / 'general_test.idl'], warn_about_unsupported_annotations=False)


# TODO
@unittest.skip("TODO")
class ExternalIdlFileTests(unittest.TestCase):

    @unittest.skipUnless(opendds_path.is_dir(),
        "DDS_ROOT environment variable is not a valid directory")
    def test_opendds_idl_files(self):
        tao_path = Path(os.environ.get('TAO_ROOT', opendds_path / 'ACE_TAO/TAO'))
        settings = dict(
            includes=[
                opendds_path,
                tao_path,
                tao_path / 'orbsvcs',
                opendds_path / 'performance-tests/DCPS/Sync',
                opendds_path / 'performance-tests/bench_2/builder_idl',
            ],
            defines=[
                'OPENDDS_SECURITY'
            ],
        )

        def idl_file_filter(p):
            return not p.samefile(opendds_path / 'tools/excelRTD/IRTDServer.idl') \
                and tao_path not in p.parents
        for f in filter(idl_file_filter, opendds_path.rglob('**/*.idl')):
            with self.subTest(path=f):
                idl_parser.parse([f], **settings)


class PreprocessorTests(unittest.TestCase):

    def test_builtin_define(self):
        idl_parser.parse(direct_inputs=['''\
            #ifndef __BRIDLE
            Something that's not IDL
            #endif
            '''])

    @unittest.expectedFailure
    def test_builtin_define_inverse(self):
        idl_parser.parse(direct_inputs=['''\
            #ifdef __BRIDLE
            Something that's not IDL
            #endif
            '''])


class TestIters(unittest.TestCase):

    def test_single_peek(self):
        it1 = PeekIter([1, 2, 3])
        self.assertFalse(it1.done())
        self.assertEqual(next(it1), 1)
        self.assertEqual(it1.peek(), [2])
        self.assertEqual(next(it1), 2)
        it1.advance()
        self.assertEqual(it1.peek(), [])
        self.assertTrue(it1.done())

    def test_multiple_peek(self):
        it2 = PeekIter([1, 2, 3, 4, 5])
        self.assertEqual(next(it2), 1)
        self.assertEqual(it2.peek(count=2), [2, 3])
        self.assertEqual(it2.peek(start=2, count=2), [3, 4])
        self.assertEqual(next(it2), 2)
        self.assertEqual(it2.peek(start=2, count=2), [3, 4])
        self.assertEqual(next(it2), 3)
        self.assertEqual(next(it2), 4)
        self.assertEqual(next(it2), 5)
        self.assertEqual(it2.peek(), [])
        self.assertTrue(it2.done())

    def test_chained(self):
        it3 = PeekIter([1, 2, 3, 4])
        self.assertEqual(next(it3), 1)
        it4 = ChainedIter(it3)
        it4.advance(2)
        self.assertEqual(next(it4), 4)
        self.assertTrue(it4.done())
        self.assertEqual(next(it3), 2)
        self.assertEqual(next(it3), 3)
        self.assertEqual(next(it3), 4)
        self.assertTrue(it3.done())


if __name__ == '__main__':
    unittest.main()
