import unittest
from pathlib import Path
import os

import bridle
compiler = bridle.Compiler()
test_path = Path(__file__).parent
opendds_path = Path(os.environ.get('DDS_ROOT', 'OpenDDS'))


class ConstExprTests(unittest.TestCase):

    @unittest.expectedFailure
    def test_expect_assert_value_fail(self):
        compiler.compile(direct=['''\
            @bridle::assert_value(1)
            const short fail_value = 0;
            '''])

    def test_int_literals(self):
        compiler.compile(direct=['''\
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
        compiler.compile(direct=['''\
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
        compiler.compile(direct=['''\
            @bridle::assert_value(1.2)
            const float float1 = 1.2;
            '''])

    def test_float_char_literals(self):
        # TODO
        pass

    def test_const_expr_ops_basic(self):
        compiler.compile(direct=['''\
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
        compiler.compile(direct=['''\
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
        compiler.compile(direct=['''\
            const short const1 = 0;
            @bridle::assert_value(0)
            const short const2 = const1;
            '''])


class IdlFileTests(unittest.TestCase):

    def test_general_test_idl(self):
        compiler.compile(
            [test_path / 'general_test.idl'], warn_about_unsupported_annotations=False)


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
                compiler.compile([f], **settings)


if __name__ == '__main__':
    unittest.main()
