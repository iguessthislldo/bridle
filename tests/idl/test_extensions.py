import unittest

from bridle import IdlParser
from bridle.errors import ParseError


class TestExtensions(unittest.TestCase):

    def idl_is_invalid_unless(self, setting, *idls):
        idl_parser = IdlParser(raise_parse_errors=True)
        for idl in idls:
            with self.assertRaises(ParseError):
                idl_parser.parse(direct_inputs=[idl])
        idl_parser = IdlParser(raise_parse_errors=True, **{setting: True})
        for idl in idls:
            idl_parser.parse(direct_inputs=[idl])

    def test_allow_empty_modules(self):
        self.idl_is_invalid_unless(
            'allow_empty_modules',
            '',
            'module M {};',
        )

    def test_allow_trailing_comma(self):
        self.idl_is_invalid_unless(
            'allow_trailing_comma',
            'enum E {a, b, c,};',
            # TODO: More cases
        )
