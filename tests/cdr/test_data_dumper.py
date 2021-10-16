import unittest
from pathlib import Path

from bridle import IdlParser
from bridle.cdr.data_dumper import DataDumper

idl_parser = IdlParser()
test_path = Path(__file__).parent


class IdlFileTests(unittest.TestCase):
    def test_dump_simple_idl_data(self):
        trees = idl_parser.parse([test_path / 'simple.idl'])
        DataDumper(trees).dump((test_path / 'simple.cdr').read_bytes(), '::Simple')
