import re
import enum
from string import hexdigits, octdigits

from .errors import ExpectedError, ParseError
from .parser import Parser, nontrivial_rule
from .utils import Location


class AbstractToken:
    def is_idl(self):
        return True

    def value(self, text):
        return None

    def show_text(self):
        return False

    def replace_kind(self, text):
        return None

    def repr_template(self, fmt='', *args, short=False):
        info = ''
        if fmt:
            info = ': ' + fmt.format(*args)
        return '<{}{}>'.format('' if short else self.__class__.__name__, info)

    def __repr__(self):
        return self.repr_template()

    def __str__(self):
        return repr(self)


class FixedToken(AbstractToken):
    def __init__(self, text):
        self.text = text

    def __repr__(self):
        return self.repr_template('{}', repr(self.text))

    def __str__(self):
        return self.text


class Comment(AbstractToken):
    def is_idl(self):
        return False

    def show_text(self):
        return True


class PreprocessorStatement(AbstractToken):
    def is_idl(self):
        return False

    def show_text(self):
        return True


line_regex = re.compile(r'\s*#\s*line\s+(\d+)(?:\s+"(.*)")?')


def set_location_from_line_statement(loc, text):
    m = line_regex.fullmatch(text)
    if m:
        lineno, filename = m.groups()
        loc.set_line(filename, int(lineno) - 1)
        # It's lineno - 1 to account for the newline after this
        return True
    return False


class Whitespace(AbstractToken):
    def is_idl(self):
        return False


class Newline(Whitespace):
    pass


class Punctuation(FixedToken):
    all_punct = {}
    min_len = 10000
    max_len = 0

    @classmethod
    def add(cls, text, inst):
        cls.min_len = min(cls.min_len, len(text))
        cls.max_len = max(cls.max_len, len(text))
        cls.all_punct[text] = inst

    def __init__(self, text):
        super().__init__(text)
        self.add(text, self)

    @classmethod
    def in_stream(cls, stream):
        for l in range(cls.max_len, cls.min_len - 1, -1):
            text = stream.peek(l)
            if text in cls.all_punct:
                return cls.all_punct[text]
        return None


class Keyword(FixedToken):
    all_keywords = {}

    def __init__(self, text):
        super().__init__(text)
        self.all_keywords[text.lower()] = self

    @classmethod
    def from_identifier(cls, text):
        kw = cls.all_keywords.get(text.lower(), None)
        if kw is not None and text != kw.text:
            raise ValueError('{} and the canonical keyword {} differ in case'.format(
                repr(text), repr(kw.text)))
        return kw


class Boolean(AbstractToken):
    def value(self, text):
        if text == TokenKind.TRUE.value.text:
            return True
        if text == TokenKind.FALSE.value.text:
            return False
        raise ValueError('Boolean: invalid text value: ' + repr(text))

    def show_text(self):
        return True


class Identifier(AbstractToken):
    def show_text(self):
        return True

    @staticmethod
    def get_token_kind(loc, text):
        try:
            token_kind = Keyword.from_identifier(text)
        except ValueError as e:
            l = Location(loc)
            l.set_length(text)
            raise ParseError(l, str(e))
        if token_kind is None:
            return TokenKind.identifier
        token_kind = TokenKind(token_kind)
        if token_kind in (TokenKind.TRUE, TokenKind.FALSE):
            return TokenKind.boolean
        return token_kind


class Integer(AbstractToken):
    def value(self, text):
        base = 10
        if text.startswith('0x'):
            base = 16
        elif text.startswith('0X'):
            base = 16
            text = text[2:]  # 0X is valid in IDL, but int() won't accept it
        elif text.startswith('0'):
            base = 8
        return int(text, base=base)

    def show_text(self):
        return True


class CharLiteral(AbstractToken):
    def __init__(self, is_wide=False, is_string=False):
        self.is_wide = is_wide
        self.is_string = is_string

    def show_text(self):
        return True


class FloatingPoint(AbstractToken):
    def value(self, text):
        return float(text)

    def show_text(self):
        return True


class TokenKind(enum.Enum):
    # Not IDL
    preprocessor_statement = PreprocessorStatement()
    newline = Newline()
    whitespace = Whitespace()
    comment = Comment()

    # Primitive Literals and Identifiers
    boolean = Boolean()
    identifier = Identifier()
    floating_point = FloatingPoint()
    integer = Integer()
    char = CharLiteral()
    string = CharLiteral(is_string=True)
    wchar = CharLiteral(is_wide=True)
    wstring = CharLiteral(is_string=True, is_wide=True)

    # Punctuation
    semicolon = Punctuation(';')
    scope_sep = Punctuation('::')
    colon = Punctuation(':')
    comma = Punctuation(',')
    equals = Punctuation('=')
    plus = Punctuation('+')
    minus = Punctuation('-')
    lparens = Punctuation('(')
    rparens = Punctuation(')')
    less_than = Punctuation('<')
    greater_than = Punctuation('>')
    lbrace = Punctuation('{')
    rbrace = Punctuation('}')
    lbracket = Punctuation('[')
    rbracket = Punctuation(']')
    slash = Punctuation('/')
    blackslash = Punctuation('\\')
    pipe = Punctuation('|')
    carrot = Punctuation('^')
    ampersand = Punctuation('&')
    astreisk = Punctuation('*')
    percent = Punctuation('%')
    tilde = Punctuation('~')
    at = Punctuation('@')

    # Keywords
    ABSTRACT = Keyword('abstract')
    ALIAS = Keyword('alias')
    ANY = Keyword('any')
    ATTRIBUTE = Keyword('attribute')
    BITFIELD = Keyword('bitfield')
    BITMASK = Keyword('bitmask')
    BITSET = Keyword('bitset')
    BOOLEAN = Keyword('boolean')
    CASE = Keyword('case')
    CHAR = Keyword('char')
    COMPONENT = Keyword('component')
    CONNECTOR = Keyword('connector')
    CONST = Keyword('const')
    CONSUMES = Keyword('consumes')
    CONTEXT = Keyword('context')
    CUSTOM = Keyword('custom')
    DEFAULT = Keyword('default')
    DOUBLE = Keyword('double')
    EMITS = Keyword('emits')
    ENUM = Keyword('enum')
    EVENTTYPE = Keyword('eventtype')
    EXCEPTION = Keyword('exception')
    FACTORY = Keyword('factory')
    FALSE = Keyword('FALSE')
    FINDER = Keyword('finder')
    FIXED = Keyword('fixed')
    FLOAT = Keyword('float')
    GETRAISES = Keyword('getraises')
    HOME = Keyword('home')
    IMPORT = Keyword('import')
    IN = Keyword('in')
    INOUT = Keyword('inout')
    INT16 = Keyword('int16')
    INT32 = Keyword('int32')
    INT64 = Keyword('int64')
    INT8 = Keyword('int8')
    INTERFACE = Keyword('interface')
    LOCAL = Keyword('local')
    LONG = Keyword('long')
    MANAGES = Keyword('manages')
    MAP = Keyword('map')
    MIRRORPORT = Keyword('mirrorport')
    MODULE = Keyword('module')
    MULTIPLE = Keyword('multiple')
    NATIVE = Keyword('native')
    OBJECT = Keyword('Object')
    OCTET = Keyword('octet')
    ONEWAY = Keyword('oneway')
    OUT = Keyword('out')
    PORT = Keyword('port')
    PORTTYPE = Keyword('porttype')
    PRIMARYKEY = Keyword('primarykey')
    PRIVATE = Keyword('private')
    PROVIDES = Keyword('provides')
    PUBLIC = Keyword('public')
    PUBLISHES = Keyword('publishes')
    RAISES = Keyword('raises')
    READONLY = Keyword('readonly')
    SEQUENCE = Keyword('sequence')
    SETRAISES = Keyword('setraises')
    SHORT = Keyword('short')
    STRING = Keyword('string')
    STRUCT = Keyword('struct')
    SUPPORTS = Keyword('supports')
    SWITCH = Keyword('switch')
    TRUE = Keyword('TRUE')
    TRUNCATABLE = Keyword('truncatable')
    TYPEDEF = Keyword('typedef')
    TYPEID = Keyword('typeid')
    TYPENAME = Keyword('typename')
    TYPEPREFIX = Keyword('typeprefix')
    UINT16 = Keyword('uint16')
    UINT32 = Keyword('uint32')
    UINT64 = Keyword('uint64')
    UINT8 = Keyword('uint8')
    UNION = Keyword('union')
    UNSIGNED = Keyword('unsigned')
    USES = Keyword('uses')
    VALUEBASE = Keyword('ValueBase')
    VALUETYPE = Keyword('valuetype')
    VOID = Keyword('void')
    WCHAR = Keyword('wchar')
    WSTRING = Keyword('wstring')


class Token:
    def __init__(self, loc, text, kind, value=None):
        self.loc = loc
        self.text = text
        try:
            self.value = kind.value.value(text) if value is None else value
        except Exception as e:
            l = Location(loc)
            l.set_length(text)
            raise ParseError(l, str(e))
        self.kind = kind

    def is_idl(self):
        return self.kind.value.is_idl()

    def is_ws(self):
        return isinstance(self.kind.value, Whitespace)

    def __str__(self):
        return self.text

    def _repr(self, short=False):
        return '<{}{}{}>'.format('' if short else 'Token: ', self.kind.name,
            (' ' + repr(self.text)) if self.kind.value.show_text() else '')

    def __repr__(self):
        return self._repr()

    def short_repr(self):
        return self._repr(short=True)


class IdlTokenizer(Parser):
    def peek_chars_matching(self, match_func, perchar=False, include_last=False):
        s = ''
        while True:
            l = len(s) + 1
            new_s = self.stream.peek(l)
            if len(new_s) != l:
                break
            elif not match_func(new_s[-1] if perchar else new_s):
                if include_last:
                    s = new_s
                break
            s = new_s
        return s

    def expect_chars_matching(self, match_func, what, perchar=False, include_last=False):
        loc = Location(self.stream.loc())
        s = self.peek_chars_matching(match_func, perchar, include_last)
        if s:
            self.stream.advance(s)
            return s
        raise ExpectedError(loc, what, repr(self.stream.peek()))

    def next_char(self, what):
        self.assert_not_end(what)
        return next(self.stream)

    def peek_char(self, what):
        self.assert_not_end(what)
        return self.stream.peek()

    def m_exact(self, *strings):
        help_strings = [repr(i) for i in strings]
        self.assert_not_end(help_strings)
        m = 0
        for string in strings:
            l = len(string)
            m = max(m, l)
            if self.stream.peek(l) == string:
                self.stream.advance(l)
                return string
        raise ExpectedError(self.stream.loc(), help_strings, repr(self.stream.peek(m)))

    def m_newline(self):
        loc = self.stream.loc()
        text = self.expect_chars_matching(lambda c: c == '\n', ["newline"], perchar=True)
        return Token(loc, text, TokenKind.newline)

    @nontrivial_rule
    def m_single_line_comment(self):
        loc = self.stream.loc()
        text = self.m_exact('//') + \
            self.expect_chars_matching(lambda c: c != '\n',
                ["single line comment"], perchar=True)
        return Token(loc, text, TokenKind.comment)

    @nontrivial_rule
    def m_multi_line_comment(self):
        loc = self.stream.loc()
        text = self.m_exact('/*') + \
            self.expect_chars_matching(lambda s: not s.endswith("*/"),
                ["multi line comment"], include_last=True)
        return Token(loc, text, TokenKind.comment)

    preprocessor_statement_regex = re.compile('#[^\n]*')

    @nontrivial_rule
    def m_preprocessor_statement(self):
        loc = self.stream.loc()
        if self.stream.peek() == '#' and self.seen_idl_on_line:
            self.stream.advance()
            raise ParseError(loc, 'Prepreprocessor statment after IDL')
        text = self.expect_chars_matching(self.preprocessor_statement_regex.fullmatch,
            ['preprocessor statement'])
        set_location_from_line_statement(self.stream.loc(), text)
        return Token(loc, text, TokenKind.preprocessor_statement)

    def m_whitespace(self):
        loc = self.stream.loc()
        text = self.expect_chars_matching(str.isspace, ["whitespace"])
        return Token(loc, text, TokenKind.whitespace)

    # Don't use \d because we don't want it to match non-ASCII digits
    # Keep consistent with ScopedName
    identifier_regex = re.compile(r'[^0-9\W]\w*')

    def m_identifier(self, what=['identifier']):
        loc = self.stream.loc()
        text = self.expect_chars_matching(self.identifier_regex.fullmatch, what)
        return Token(loc, text, Identifier.get_token_kind(loc, text))

    dec_regex = re.compile(r'[0-9]+')
    hex_regex = re.compile(r'[0-9a-fA-F]+')
    oct_regex = re.compile(r'[0-7]+')

    def match_dec_integer(self, what):
        return self.expect_chars_matching(self.dec_regex.fullmatch, what)

    def m_integer_literal(self):
        loc = self.stream.loc()
        if self.stream.peek() == '0':
            c = self.stream.peek(offset=1)
            if self.stream.peek(offset=1) in 'xX':
                self.stream.advance(2)
                text = '0' + c + self.expect_chars_matching(self.hex_regex.fullmatch,
                    ['hexadecimal integer literal'])
            else:
                text = self.expect_chars_matching(self.oct_regex.fullmatch,
                    ['octal integer literal'])
        else:
            text = self.match_dec_integer(['decimal integer literal'])
        return Token(loc, text, TokenKind.integer)

    @nontrivial_rule
    def m_floating_point_exp(self):
        text = self.m_exact('E', 'e')
        sign = self.m_exact_maybe('+', '-')
        if sign:
            text += sign
        return text + self.match_dec_integer(['exponential part of floating point literal'])

    @nontrivial_rule
    def m_floating_point_form3_end(self):
        text = self.m_exact('.')
        text += self.match_dec_integer(['fractional part of floating point literal'])
        exp = self.m_floating_point_exp_maybe()
        if exp:
            text += exp
        return text

    @nontrivial_rule
    def m_floating_point_literal(self):
        loc = self.stream.loc()
        what = ['floating point literal']
        text = ''
        # EXP: [eE][+-]?\d+
        # Form 1: \d+EXP
        # Form 2: \d+\.EXP?
        # Form 3: \d*\.\d+EXP?
        c = self.stream.peek()
        if self.dec_regex.match(c):
            # Form 1, 2, and 3 with leading whole number
            text += self.match_dec_integer(what)
            dp = self.stream.peek()
            if dp != '.':
                # Form 1
                text += self.m_floating_point_exp()
            else:
                c = self.stream.peek(offset=1)
                if self.dec_regex.match(c):
                    # Form 3 with leading whole number
                    text += self.m_floating_point_form3_end()
                else:
                    # Form 2
                    text += dp
                    self.stream.advance()  # Accept decimal point
                    exp = self.m_floating_point_exp_maybe()
                    if exp:
                        text += exp
        elif c == '.':
            # Form 3 without leading whole number
            text = self.m_floating_point_form3_end()
        if text:
            return Token(loc, text, TokenKind.floating_point)
        raise ExpectedError(loc, what, repr(self.stream.peek()))

    def hex_escape(self, maxlen):
        what = ['hex escape']
        c = self.next_char(what)
        if c not in hexdigits:
            raise ParseError(self.stream.loc(),
                '{} is not a valid hexadecimal digit', repr(c))
        hexstr = c
        for i in range(maxlen - 1):
            c = self.peek_char(what)
            if c in hexdigits:
                hexstr += c
                self.stream.advance()
            else:
                break
        return hexstr, ord(int(hexstr, base=16))

    def get_character_literal_part(self, traits):
        # Spec says we shouldn't allow zero value, we will allow it because not
        # all languages have null terminated strings.
        what = ['part of string']
        while True:
            c = self.next_char(what)
            if c == '\\':
                text = c
                c = self.next_char(what)
                text += c
                if c == 'n':
                    yield text, '\n'
                elif c == 't':
                    yield text, '\t'
                elif c == 'v':
                    yield text, '\v'
                elif c == 'b':
                    yield text, '\b'
                elif c == 'r':
                    yield text, '\r'
                elif c == 'f':
                    yield text, '\f'
                elif c == 'a':
                    yield text, '\a'
                elif c == '\\':
                    yield text, '\\'
                elif c == '?':
                    yield text, '?'
                elif c == "'":
                    yield text, "'"
                elif c == '"':
                    yield text, '"'
                elif c in octdigits:
                    octstr = c
                    c = self.peek_char(what)
                    if c in octdigits:
                        octstr += c
                        self.stream.advance()
                        c = self.peek_char(what)
                        if c in octdigits:
                            octstr += c
                            self.stream.advance()
                    yield text + octstr, int(octstr, base=8)
                elif c == 'x':
                    hexstr, value = self.hex_escape(2)
                    yield text + hexstr, value
                elif c == 'u':
                    if not traits.is_wide:
                        raise ParseError(self.stream.loc(),
                            'Can\'t use \\u escape in a non-wide string or character literal')
                    hexstr, value = self.hex_escape(4)
                    yield text + hexstr, value
                else:
                    raise ParseError(self.stream.loc(), 'Invalid escape {}', repr(c))
            elif traits.is_string and c == '\"':
                yield c, None
            elif not traits.is_string and c == "'":
                yield c, None
            else:
                yield c, c

    def get_character_literal(self, token_kind):
        traits = token_kind.value
        loc = Location(self.stream.loc())
        quote = '"' if traits.is_string else "'"
        text = ''
        if traits.is_wide:
            text += self.m_exact('L')
        text += self.m_exact(quote)
        value = ''
        for t, v in self.get_character_literal_part(traits):
            text += t
            if v is None:
                break
            value += v
        if not traits.is_string and len(value) != 1:
            raise ParseError(loc, 'Character literals must contain just one character')
        return Token(loc, text, token_kind, value=value)

    @nontrivial_rule
    def m_char_literal(self):
        return self.get_character_literal(TokenKind.char)

    @nontrivial_rule
    def m_string_literal(self):
        return self.get_character_literal(TokenKind.string)

    @nontrivial_rule
    def m_wchar_literal(self):
        return self.get_character_literal(TokenKind.wchar)

    @nontrivial_rule
    def m_wstring_literal(self):
        return self.get_character_literal(TokenKind.wstring)

    def m_punctuation(self, what=['punctuation']):
        loc = Location(self.stream.loc())
        p = Punctuation.in_stream(self.stream)
        if p is not None:
            self.stream.advance(str(p))
            return Token(loc, p.text, TokenKind(p))
        raise ExpectedError(self.stream.loc(), what, repr(self.stream.peek()))

    def start(self):
        tokens = []
        self.seen_idl_on_line = False
        while not self.stream.done():
            token = self.match((
                'newline',
                'single_line_comment',
                'multi_line_comment',
                'whitespace',
                'preprocessor_statement',
                'floating_point_literal',
                'integer_literal',
                'char_literal',
                'wchar_literal',
                'string_literal',
                'wstring_literal',
                'punctuation',
                'identifier',
            ))
            if token.kind == TokenKind.newline:
                self.seen_idl_on_line = False
            elif token.is_idl():
                self.seen_idl_on_line = True
            tokens.append(token)
        return tokens

    def tokenize(self, source, name='Unknown', debug=False, parse_error_handler=None):
        return self._parse(source, name, over_chars=True, debug=debug,
            parse_error_handler=parse_error_handler)


def dump_tokens(tokens):
    # TODO: Use rich?

    # Get alignment needed for tokens
    newline = True
    align = 0
    for token in tokens:
        if newline:
            align = max(align, len(token.loc.short_str()))
            newline = False
        if token.kind == TokenKind.newline:
            newline = True

    # Now print the tokens
    newline = True
    for token in tokens:
        if newline:
            print('\n{0:<{align}}:'.format(token.loc.short_str(), align=align), end='')
            newline = False
        if token.is_ws():
            newline = token.kind == TokenKind.newline
            continue
        print(' ', token.short_repr(), end='')
    print()
