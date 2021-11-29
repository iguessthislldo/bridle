import unittest
from pathlib import Path
import os

import bridle

idl_parser = bridle.IdlParser(raise_parse_errors=True)
test_path = Path(__file__).parent
opendds_path = Path(os.environ.get('DDS_ROOT', 'OpenDDS'))


# TODO
class IdlFileTests(unittest.TestCase):

    def test_general_test_idl(self):
        idl_parser.parse(
            [test_path / 'general_test.idl'],
            warn_about_unsupported_annotations=False,
        )

    def test_xtypes_type_object_idl(self):
        idl_parser.parse(
            [test_path / 'xtypes-type-object.idl'],
            warn_about_unsupported_annotations=False,
        )


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
            const long x = 1;
            '''])

    @unittest.expectedFailure
    def test_builtin_define_inverse(self):
        idl_parser.parse(direct_inputs=['''\
            #ifdef __BRIDLE
            Something that's not IDL
            #endif
            '''])
