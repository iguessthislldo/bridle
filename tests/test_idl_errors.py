import unittest

import bridle
from bridle.errors import ParseError

idl_parser = bridle.IdlParser(raise_parse_errors=True)


class IdlConstantsTests(unittest.TestCase):

    def test_multiple_unsigned(self):
        with self.assertRaises(ParseError) as cm:
            idl_parser.parse(direct_inputs=['''\
                const unsigned unsigned unsigned long x = 0;
                '''])
        self.assertEqual(cm.exception.location.col, 32)
