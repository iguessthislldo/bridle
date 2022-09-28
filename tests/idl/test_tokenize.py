import unittest
from pathlib import Path

from bridle.idl.tokenizer import IdlTokenizer, TokenKind
from bridle.errors import ParseError


test_path = Path(__file__).parent


class IdlTokenizerTests(unittest.TestCase):
    def round_trip_check(self, text, tokens):
        self.assertEqual(text, ''.join([str(t) for t in tokens]))

    def literal_test_helper(self, token_kind, to_test):
        tokenizer = IdlTokenizer()
        idl_text = ''.join([text + '\n' for text, _ in to_test])
        # print(text)
        tokens = tokenizer.tokenize(idl_text)
        # print(tokens)
        last_kind = TokenKind.newline
        for i, token in enumerate(tokens):
            expect_literal = last_kind == TokenKind.newline
            self.assertEqual(token.kind, token_kind
                 if expect_literal else TokenKind.newline, token.loc)
            if expect_literal:
                text, value = to_test[i // 2]
                self.assertEqual(token.text, text, token.loc)
                self.assertEqual(token.value, value, token.loc)
            last_kind = token.kind
        self.round_trip_check(idl_text, tokens)

    def test_boolean_literals(self):
        self.literal_test_helper(TokenKind.boolean, [
            ("TRUE", True),
            ("FALSE", False),
        ])

    def test_integer_literals(self):
        self.literal_test_helper(TokenKind.integer, [
            ('0', 0),
            ('24', 24),
            ('0x0', 0),
            ('0xf', 0xf),
            ('0x10', 0x10),
            ('0xFaF', 0xfaf),
            ('01', 1),
            ('0777', 0o777),
        ])

    def test_floating_point_literals(self):
        self.literal_test_helper(TokenKind.floating_point, [
            ('1E1', 1e1),
            ('2e+2', 2e+2),
            ('3e-3', 3e-3),
            ('4.', 4.),
            ('5.E5', 5.E5),
            ('6.e+6', 6.e+6),
            ('7.e-7', 7.e-7),
            ('.8', .8),
            ('.9e9', .9e9),
            ('.10E+10', .10E+10),
            ('.11e-11', .11e-11),
            ('12.12', 12.12),
            ('13.13e13', 13.13e13),
            ('14.14e+14', 14.14e+14),
            ('15.15e-15', 15.15e-15),
        ])

    # TODO
    def test_char_literals(self):
        self.literal_test_helper(TokenKind.char, [
            ("'a'", 'a'),
            ("'\n'", '\n'),
        ])

    # TODO
    def test_string_literals(self):
        self.literal_test_helper(TokenKind.string, [
            ('""', ''),
            ('"Hello"', 'Hello'),
        ])

    def test_preprocessor(self):
        tokenizer = IdlTokenizer()
        # TODO Compare
        text = '''\
            #define X 1
            #if X == 1
            #  define Y 0
            #else
            #  define Y 2
            #endif
        '''
        tokens = tokenizer.tokenize(text)
        self.round_trip_check(text, tokens)

    def test_preprocessor_after_idl_causes_error(self):
        tokenizer = IdlTokenizer()
        with self.assertRaisesRegex(ParseError, 'Prepreprocessor statement after IDL'):
            tokenizer.tokenize('const x = 10; #define V 100')

    def test_comments(self):
        tokenizer = IdlTokenizer()
        # TODO Compare
        text = '''\
            //
            // Hello '
            const x = 10; // Hello ' again
            /**/
            /* Hello ' */
            const/**/y = /* inline ' comment */ 10; /* Another ' comment */ // End '
            /*
             * This is
             * a comment I'm writing.
             */
        '''
        tokens = tokenizer.tokenize(text)
        self.round_trip_check(text, tokens)

    def test_general_test_idl(self):
        tokenizer = IdlTokenizer()
        path = test_path / 'general_test.idl'
        text = path.read_text()
        tokens = tokenizer.tokenize(text, str(path))
        # for token in tokens:
        #   print(repr(token))
        self.round_trip_check(text, tokens)
