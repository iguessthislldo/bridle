import unittest
from pathlib import Path
import os

import bridle
from bridle.utils.iters import PeekIter, ChainedIter

idl_parser = bridle.IdlParser()
test_path = Path(__file__).parent
opendds_path = Path(os.environ.get('DDS_ROOT', 'OpenDDS'))


# TODO
class IdlFileTests(unittest.TestCase):

    def test_general_test_idl(self):
        idl_parser.parse(
            [test_path / 'general_test.idl'], warn_about_unsupported_annotations=False,
            debug_all_parsing=True)


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
