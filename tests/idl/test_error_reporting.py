import unittest
from pathlib import Path

import bridle
from bridle.idl.parser import SourceLines
from bridle.errors import ParseError, PreprocessorError

idl_parser = bridle.IdlParser(raise_parse_errors=True)
test_path = Path(__file__).parent


class ErrorReportingTests(unittest.TestCase):

    def test_multiple_unsigned(self):
        with self.assertRaises(ParseError) as cm:
            idl_parser.parse(direct_inputs=['''\
                const unsigned unsigned unsigned long x = 0;
                '''])
        self.assertEqual(cm.exception.location.col, 32)

    def test_error_inside_include(self):
        with self.assertRaises(ParseError) as cm:
            idl_parser.parse(includes=[test_path], direct_inputs=['''\
                #include <invalid_file.idl>
                '''])
        ex = cm.exception
        self.assertEqual(SourceLines().get_line(ex.location.source_key, ex.location.line),
            "const long x == 1;")

    def test_nonexistent_include(self):
        line = "#include <nonexistent_include.idl>"
        with self.assertRaises(PreprocessorError) as cm:
            idl_parser.parse(direct_inputs=[line])
        ex = cm.exception
        loc = ex.location
        self.assertEqual(loc.line, 1)
        self.assertEqual(loc.length, len(line))
        self.assertEqual(idl_parser.source_lines.get_line(loc.source_key, loc.line), line)
