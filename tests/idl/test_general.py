import unittest
from pathlib import Path
import os

import bridle

idl_parser = bridle.IdlParser(
    raise_parse_errors=True,
    unsupported_annotations=bridle.idl.UnsupportedAnnotations.ignore,
    allow_empty_modules=True,
)
test_path = Path(__file__).parent
opendds_path = Path(os.environ.get('OPENDDS_ROOT', 'OpenDDS'))


# TODO
class IdlFileTests(unittest.TestCase):

    def test_general_test_idl(self):
        idl_parser.parse([test_path / 'general_test.idl'])

    def test_xtypes_type_object_idl(self):
        idl_parser.parse([test_path / 'xtypes-type-object.idl'])


# TODO
@unittest.skip("TODO")
class ExternalIdlFileTests(unittest.TestCase):

    @unittest.skipUnless(opendds_path.is_dir(),
        "OPENDDS_ROOT environment variable is not a valid directory")
    def test_opendds_idl_files(self):
        tao_path = Path(os.environ.get('TAO_ROOT', 'ACE_TAO/TAO'))
        settings = dict(
            includes=[
                tao_path,
                tao_path / 'orbsvcs',
                opendds_path,
                opendds_path / 'dds/DCPS/RTPS',
                opendds_path / 'performance-tests/DCPS/Sync',
                opendds_path / 'performance-tests/bench/builder_idl',
                opendds_path / 'tests/cmake/Nested_IDL/transmission',
                opendds_path / 'tests/cmake/Nested_IDL/engine',
                opendds_path / 'tests/cmake/Nested_IDL/engine/engine_stats',
                opendds_path / 'tests/cmake/install/library/include',
            ],
            defines=[
                '__TAO_IDL=0xffffffff'
                '__OPENDDS_IDL=0xffffffff',
                'OPENDDS_SECURITY',
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
