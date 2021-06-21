from pathlib import Path
import re
from io import StringIO
import enum
from string import hexdigits, octdigits

from pcpp import Preprocessor

from .utils import PeekIter, ChainedIter, Location, is_sequence
from .errors import BridleError, ParseError

macro_regex = re.compile(r'^\s*#.*$')
line_regex = re.compile(r'\s*#line (\d+)(?: "(.*)")?')


class Keyword:
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return self.value

    def __repr__(self):
        return '<Keyword: {}>'.format(repr(self.value))


class Token(enum.Enum):
    begin_scope = enum.auto()
    end_scope = enum.auto()
    u8 = enum.auto()
    i8 = enum.auto()
    u16 = enum.auto()
    i16 = enum.auto()
    u32 = enum.auto()
    i32 = enum.auto()
    u64 = enum.auto()
    i64 = enum.auto()
    f32 = enum.auto()
    f64 = enum.auto()
    f128 = enum.auto()
    boolean = enum.auto()
    octet = enum.auto()
    char = enum.auto()
    wchar = enum.auto()
    SHORT = Keyword('short')
    LONG = Keyword('long')
    INT8 = Keyword('int8')
    UINT8 = Keyword('uint8')
    INT16 = Keyword('int16')
    UINT16 = Keyword('uint16')
    INT32 = Keyword('int32')
    UINT32 = Keyword('uint32')
    INT64 = Keyword('int64')
    UINT64 = Keyword('uint64')
    ABSTRACT = Keyword('abstract')
    ANY = Keyword('any')
    ALIAS = Keyword('alias')
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
    EXCEPTION = Keyword('exception')
    EMITS = Keyword('emits')
    ENUM = Keyword('enum')
    EVENTTYPE = Keyword('eventtype')
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
    INTERFACE = Keyword('interface')
    LOCAL = Keyword('local')
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
    PRIMARYKEY = Keyword('primarykey')
    PRIVATE = Keyword('private')
    PORT = Keyword('port')
    PORTTYPE = Keyword('porttype')
    PROVIDES = Keyword('provides')
    PUBLIC = Keyword('public')
    PUBLISHES = Keyword('publishes')
    RAISES = Keyword('raises')
    READONLY = Keyword('readonly')
    SETRAISES = Keyword('setraises')
    SEQUENCE = Keyword('sequence')
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
    UNSIGNED = Keyword('unsigned')
    UNION = Keyword('union')
    USES = Keyword('uses')
    VALUEBASE = Keyword('ValueBase')
    VALUETYPE = Keyword('valuetype')
    VOID = Keyword('void')
    WCHAR = Keyword('wchar')
    WSTRING = Keyword('wstring')


keywords = {}
for name, value in Token.__members__.items():
    if isinstance(value.value, Keyword):
        keywords[str(value.value)] = value.value


class IdlFile:
    def __init__(self, path=None, direct_input=None):
        if direct_input is not None and path is None:
            self.path = 'DIRECT_INPUT'
            self.idl_file_contents = direct_input
        elif path is not None and direct_input is None:
            self.path = Path(path)
            self.idl_file_contents = self.path.read_text()
        else:
            raise ValueError('Either path or direct_input must be set. Not both or neither.')
        self.contents = None
        self.positions = []

    def name(self):
        return str(self.path)

    def load(self, preprocessor):
        sio = StringIO()
        preprocessor.parse(self.idl_file_contents, str(self.path))
        preprocessor.write(sio)
        if preprocessor.return_code != 0:
            raise BridleError('Encounted Preprocessor Error(s)')
        self.contents = sio.getvalue()


class Stream:
    def __init__(self, source, name):
        self.iters = [PeekIter(source)]
        self.locs = [Location(filename=name)]
        self.furthest_errors = [None]
        self.furthest_error_locs = [Location(filename=name)]
        self.in_annotation = False

    def __iter__(self):
        return self

    def __next__(self):
        rv = self.peek()
        if rv:
            self.advance()
            return rv
        raise StopIteration

    def done(self):
        return self.iters[-1].done()

    def peek(self, count=1):
        return ''.join(self.iters[-1].peek(count))

    def loc(self, value=None):
        if value is not None:
            self.locs[-1] = value
            return value
        return self.locs[-1]

    def advance(self, arg=1, loc=None):
        chars = self.iters[-1].advance(arg)
        if loc is None:
            self.loc().advance(''.join(chars))
        else:
            self.locs[-1] = Location(loc)

    def push(self):
        self.iters.append(ChainedIter(self.iters[-1]))
        self.locs.append(Location(self.loc()))
        self.furthest_errors.append(self.furthest_errors[-1])
        self.furthest_error_locs.append(Location(self.furthest_error_locs[-1]))

    def pop(self):
        return (
            self.iters.pop(),
            self.locs.pop(),
            self.furthest_errors.pop(),
            self.furthest_error_locs.pop(),
        )

    def accept(self):
        it, loc, error, error_loc = self.pop()
        self.advance(it, loc)
        return it, loc, error, error_loc

    def check_furthest_error_candidate(self, error):
        if error.location > self.furthest_error_locs[-1]:
            self.furthest_errors[-1] = error
            self.furthest_error_locs[-1] = error.location

    def reject(self, this_error):
        it, loc, popped_error, error_loc = self.pop()
        self.check_furthest_error_candidate(this_error)
        if popped_error is not None:
            self.check_furthest_error_candidate(popped_error)
        return it, loc, popped_error, error_loc

    def furthest_error(self, root_error):
        if self.furthest_error_locs[0] > root_error.location and \
                self.furthest_errors[0] is not None:
            return self.furthest_errors[0]
        return None


class IdlParser:
    default_default_settings = dict(
        includes=[], defines=[], dump_raw_tree=False, dump_tree=False,
        warn_about_unsupported_annotations=True)

    def __init__(self, **kw):
        self.default_settings = self.default_default_settings
        self.default_settings.update(kw)
        self.builtin_define_base_name = '__BRIDLE'
        self.builtin_defines = [self.builtin_define_base_name]
        self.builtin_defines.extend([self.builtin_define_base_name + i for i in [
            # TODO: Bridle Version
            '_IDL_VERSION_MAJOR=4',
            '_IDL_VERSION_MINOR=2',
        ]])

    def parse(self, paths=[], direct_inputs=[], **kw):
        settings = self.default_settings.copy()
        settings.update(kw)
        idl_files = [IdlFile(path=path) for path in paths] + \
            [IdlFile(direct_input=s) for s in direct_inputs]
        rv = []
        for idl_file in idl_files:
            preprocessor = Preprocessor()
            for include in settings['includes']:
                preprocessor.add_path(include)
            for define in self.builtin_defines + settings['defines']:
                preprocessor.define(define.replace('=', ' ', 1))
            idl_file.load(preprocessor)
            rv.append(self._parse(idl_file.contents, idl_file.name()))
        return rv

    def _parse(self, source, name):
        self.source = source
        self.stream = Stream(source, name)
        furthest_error = None
        try:
            rv = self.start()
        except ParseError as e:
            furthest_error = self.stream.furthest_error(e)
            if furthest_error is None:
                raise
        if furthest_error is not None:
            raise furthest_error
        self.assert_end()
        return rv

    def assert_not_end(self, what):
        if self.stream.done():
            raise ParseError(self.stream.loc(),
                'Expected {}, but reached end of input', ' or '.join(what))

    def assert_end(self):
        if not self.stream.done():
            raise ParseError(self.stream.loc(),
                'Expected end of input, but got {}', repr(self.stream.peek()))

    def rule_wrapper(self, func, name, maybe, *args, **kwargs):
        # print('| ' * (len(self.stream.iters) - 1) + name, args, kwargs, self.stream.loc())
        self.stream.push()
        try:
            rv = func(*args, **kwargs) if maybe else func(self, *args, **kwargs)
            it, loc, error, error_loc = self.stream.accept()
            # print('| ' * (len(self.stream.iters) - 1) + 'ACCEPT', loc)
            return rv
        except ParseError as e:
            self.stream.reject(e)
            # print('| ' * (len(self.stream.iters) - 1) + 'REJECT:',
            #     e, self.stream.furthest_error_locs[-1])
            if maybe:
                return None
            raise

    def nontrivial_rule(method):
        return lambda self, *args, **kwargs: self.rule_wrapper(
            method, method.__name__[2:], False, *args, **kwargs)

    def __getattr__(self, name):
        maybe = name.endswith('_maybe')
        if maybe:
            name = name[:-6]
        obj = object.__getattribute__(self, name)
        if maybe:
            return lambda *args, **kwargs: self.rule_wrapper(obj, name, True, *args, **kwargs)
        return obj

    def match(self, rules):
        if isinstance(rules, str):
            rules = {rules: []}
        elif not isinstance(rules, dict):
            rules = {rule: [] for rule in rules}
        for rule, args in rules.items():
            try:
                return getattr(self, 'm_' + rule)(*args)
            except ParseError:
                pass
        raise ParseError(self.stream.loc(), 'Expected ' + ' or '.join(rules))

    def comma_list_of(self, element_rules):
        rv = [self.match(element_rules)]
        while self.m_exact_maybe(',') is not None:
            rv.append(self.match(element_rules))
        return rv

    def get_preprocessor_statement(self):
        line = ''
        for c in self.stream:
            if c == '\n':
                break
            line += c
        return line

    def _ws(self, *, annotations=True):
        while not self.stream.done():
            c = self.stream.peek()
            if c.isspace():
                self.stream.advance()
            elif c == '#':  # TODO: Make sure this is a valid location?
                pps = self.get_preprocessor_statement()
                m = line_regex.fullmatch(pps)
                if m:
                    self.stream.loc().set_line(m.group(2), int(m.group(1)))
            elif c == '@' and annotations and not self.stream.in_annotation:
                self.m_annotation_appl()
                # TODO: Connect to setting
                # print('Ignored Annotation:', appl)
            else:
                break

    @nontrivial_rule
    def m_ws_before(self):
        self._ws(annotations=True)

    @nontrivial_rule
    def m_ws_after(self):
        self._ws(annotations=False)

    @nontrivial_rule
    def m_exact(self, *strings, ws_before=True, ws_after=True):
        if ws_before:
            self.m_ws_before()
        help_strings = [repr(i) for i in strings]
        self.assert_not_end(help_strings)
        m = 0
        for string in strings:
            l = len(string)
            m = max(m, l)
            if self.stream.peek(l) == string:
                self.stream.advance(l)
                if ws_after:
                    self.m_ws_after()
                return string
        raise ParseError(self.stream.loc(), 'Expected {}, but got {}',
            ' or '.join(help_strings), repr(self.stream.peek(m)))

    @nontrivial_rule
    def m_exact_seqs(self, string_seqs, ws_before=True, ws_after=True):
        string_seqs = {(k,) if not is_sequence(k) else k: v for k, v in string_seqs.items()}
        help_strings = [repr(' '.join(i)) for i in string_seqs.keys()]
        rv = None
        m = 0
        for string_seq, token in string_seqs.items():
            matched = True
            l = 0
            for string in string_seq:
                l += len(string)
                if self.m_exact_maybe(string) is None:
                    matched = False
                    break
            m = max(m, l)
            if matched:
                rv = token
                break
        if rv is None:
            raise ParseError(self.stream.loc(), 'Expected {}, but got {}',
                ' or '.join(help_strings), repr(self.stream.peek(m)))
        return rv

    def _regex(self, r, what=None):
        self.assert_not_end(what)
        first = True
        rv = ''
        while True:
            if not self.stream.done():
                char = self.stream.peek()
                if r.match(char):
                    rv += char
                    self.stream.advance()
                elif first and what is not None:
                    raise ParseError(self.stream.loc(),
                        'Expected {}, but got {}', ' or '.join(what), repr(char))
                else:
                    break
                first = False
        return rv

    id_start_re = re.compile(r'[^\d\W]')
    id_rest_re = re.compile(r'\w')

    @nontrivial_rule
    def m_identifier(self, what=['identifier']):
        self.m_ws_before()
        rv = self._regex(self.id_start_re, what) + self._regex(self.id_rest_re)
        if rv in keywords:
            raise ParseError(self.stream.loc(),
                'Expected {}, but got keyword {}', ' or '.join(what), repr(rv))
        self.m_ws_after()
        return rv

    @nontrivial_rule
    def m_integer_literal(self, what=['integer literal']):
        self.m_ws_before()
        rv = self._regex(re.compile(r'\d'), what)
        self.m_ws_after()
        return rv

    def expect_char(self, what):
        self.assert_not_end(what)
        return next(self.stream)

    def expect_char_peek(self, what):
        self.assert_not_end(what)
        return self.stream.peek()

    def hex_escape(self, maxlen):
        what = ['hex escape']
        c = self.expect_char(what)
        if c not in hexdigits:
            raise ParseError(self.stream.loc(),
                '{} is not a valid hexadecimal digit', repr(c))
        hexstr = c
        for i in range(maxlen - 1):
            c = self.expect_char_peek(what)
            if c in hexdigits:
                hexstr += c
                self.stream.advance()
            else:
                break
        return ord(int(hexstr, base=16))

    def get_character_literal(self, is_string=False, is_wide=False):
        # Spec says we shouldn't allow zero value, we will allow it because not
        # all languages have null terminated strings.
        what = ['part of string']
        while True:
            c = self.expect_char(what)
            if c == '\\':
                c = self.expect_char(what)
                if c == 'n':
                    yield '\n'
                elif c == 't':
                    yield '\t'
                elif c == 'v':
                    yield '\v'
                elif c == 'b':
                    yield '\b'
                elif c == 'r':
                    yield '\r'
                elif c == 'f':
                    yield '\f'
                elif c == 'a':
                    yield '\a'
                elif c == '\\':
                    yield '\\'
                elif c == '?':
                    yield '?'
                elif c == "'":
                    yield "'"
                elif c == '"':
                    yield '"'
                elif c in octdigits:
                    octstr = c
                    c = self.expect_char_peek(what)
                    if c in octdigits:
                        octstr += c
                        self.stream.advance()
                        c = self.expect_char_peek(what)
                        if c in octdigits:
                            octstr += c
                            self.stream.advance()
                    yield int(octstr, base=8)
                elif c == 'x':
                    yield self.hex_escape(2)
                elif c == 'u':
                    if not is_wide:
                        raise ParseError(self.stream.loc(),
                            'Can\'t use \\u escape in a non-wide string or character literal')
                    yield self.hex_escape(4)
                else:
                    raise ParseError(self.stream.loc(), 'Invalid escape {}', repr(c))
            elif is_string and c == '\"':
                break
            elif not is_string and c == "'":
                break
            else:
                yield c

    @nontrivial_rule
    def m_string_literal(self):
        self.m_exact('"', ws_after=False)
        rv = ''.join(self.get_character_literal(is_string=True))
        self.m_ws_after()
        return rv

    def m_begin_scope(self):
        self.m_exact('{')
        return Token.begin_scope

    def m_end_scope(self):
        self.m_exact('}')
        return Token.end_scope

    # =========================================================================
    # The methods below should follow the names and order of the grammar rules
    # in the spec for the most part.
    # =========================================================================

    def start(self):
        rv = []
        while not self.stream.done():
            rv.append(self.m_definition())
        return rv

    # Building Block Core Data Types ==========================================

    @nontrivial_rule
    def m_definition(self):
        rv = self.match((
            'module_dcl',
            'const_dcl',
            'type_dcl',
        ))
        self.m_exact(';')
        return rv

    @nontrivial_rule
    def m_module_dcl(self):
        self.m_exact('module')
        name = self.m_identifier()
        self.m_begin_scope()
        members = [self.m_definition()]
        while True:
            what = self.match((
                'definition',
                'end_scope',
            ))
            if what == Token.end_scope:
                break
            members.append(what)
        return [name, members]

    @nontrivial_rule
    def m_scoped_name(self):
        rv = []
        absolute = self.m_exact_maybe('::')
        if absolute:
            rv.append(absolute)
        rv.append(self.m_identifier())
        while self.m_exact_maybe('::'):
            rv.append(self.m_identifier())
        return rv

    @nontrivial_rule
    def m_const_dcl(self):
        rv = [self.m_exact('const'), self.m_const_type(), self.m_identifier()]
        self.m_exact('=')
        rv.append(self.m_const_expr())
        return rv

    @nontrivial_rule
    def m_const_type(self):
        return self.match((
            # floats and ints are flipped from the spec order because otherwise
            # "long double" will match "long".
            'floating_pt_type',
            'integer_type',
            # TODO: 'fixed_pt_const_type',
            'char_type',
            'wide_char_type',
            'boolean_type',
            'octet_type',
            'string_type',
            'wide_string_type',
            'scoped_name',
        ))

    @nontrivial_rule
    def m_const_expr(self):
        # TODO: Real const_expr Grammer
        return self.match((
            'integer_literal',
            'string_literal',
            'scoped_name',
        ))

    def m_positive_int_const(self):
        return self.m_const_expr()

    @nontrivial_rule
    def m_type_dcl(self):
        return self.match((
            'constr_type_dcl',
            # TODO: 'native_dcl',
            'typedef_dcl',
        ))

    @nontrivial_rule
    def m_type_spec(self):
        return self.match((
            'simple_type_spec',  # Building Block Core Data Types
            'template_type_spec',  # Building Block Anonymous Types
        ))

    @nontrivial_rule
    def m_simple_type_spec(self):
        return self.match((
            'base_type_spec',
            'scoped_name',
        ))

    @nontrivial_rule
    def m_base_type_spec(self):
        return self.match((
            # floats and ints are flipped from the spec order because otherwise
            # "long double" will match "long".
            'floating_pt_type',
            'integer_type',
            'char_type',
            'wide_char_type',
            'boolean_type',
            'octet_type',
        ))

    @nontrivial_rule
    def m_floating_pt_type(self):
        return self.m_exact_seqs({
            'float': Token.f32,
            'double': Token.f64,
            ('long', 'double'): Token.f128,
        })

    @nontrivial_rule
    def m_integer_type(self):
        return self.m_exact_seqs({
            'int8': Token.i8,
            'uint8': Token.u8,
            'int16': Token.i16,
            'short': Token.i16,
            'uint16': Token.u16,
            ('unsigned', 'short'): Token.u16,
            'int32': Token.i32,
            'long': Token.i32,
            'uint32': Token.u32,
            ('unsigned', 'long'): Token.u32,
            'int64': Token.i64,
            ('long', 'long'): Token.i64,
            'uint64': Token.u64,
            ('unsigned', 'long', 'long'): Token.u64,
        })

    def m_char_type(self):
        return self.m_exact_seqs({'char': Token.char})

    def m_wide_char_type(self):
        return self.m_exact_seqs({'wchar': Token.wchar})

    def m_boolean_type(self):
        return self.m_exact_seqs({'boolean': Token.boolean})

    def m_octet_type(self):
        return self.m_exact_seqs({'octet': Token.octet})

    @nontrivial_rule
    def m_template_type_spec(self):
        return self.match((
            'sequence_type',
            'string_type',
            'wide_string_type',
            # TODO: 'fixed_pt_type',
        ))

    @nontrivial_rule
    def m_sequence_type(self):
        rv = [self.m_exact('sequence')]
        self.m_exact('<')
        rv.append(self.m_type_spec())
        if self.m_exact_maybe(','):
            rv.append(self.m_positive_int_const())
        self.m_exact('>')
        return rv

    def _string_type(self, prefix=''):
        rv = [self.m_exact(prefix + 'string')]
        if self.m_exact_maybe('<'):
            rv.append(self.m_positive_int_const())
            self.m_exact('>')
        return rv

    @nontrivial_rule
    def m_string_type(self):
        return self._string_type()

    @nontrivial_rule
    def m_wide_string_type(self):
        return self._string_type(prefix='w')

    @nontrivial_rule
    def m_constr_type_dcl(self):
        return self.match((
            'struct_dcl',
            'union_dcl',
            'enum_dcl',
        ))

    @nontrivial_rule
    def m_struct_dcl(self):
        rv = [self.m_exact('struct')]
        rv.append(self.m_identifier())
        if self.m_begin_scope_maybe():
            members = []
            while True:
                what = self.match((
                    'member',
                    'end_scope'
                ))
                if what == Token.end_scope:
                    break
                members.append(what)
            rv.append(members)
        return rv

    @nontrivial_rule
    def m_member(self):
        rv = [self.m_type_spec(), self.m_declarators()]
        self.m_exact(';')
        return rv

    def match_until(self, repeating_rules, terminating_rules, at_least_one=True):
        repeating_results = []
        if at_least_one:
            repeating_results.append(self.match(repeating_rules))
        while True:
            repeating_result = self.match_maybe(repeating_rules)
            if repeating_result is not None:
                repeating_results.append(repeating_result)
            else:
                return repeating_results, self.match(terminating_rules)

    @nontrivial_rule
    def m_union_dcl(self):
        rv = [self.m_exact('union')]
        rv.append(self.m_identifier())
        if self.m_exact_maybe('switch'):
            self.m_exact('(')
            rv.append(self.m_switch_type_spec())
            self.m_exact(')')
            self.m_begin_scope()
            cases, _ = self.match_until(('case'), ('end_scope'))
            rv.append(cases)
        return rv

    @nontrivial_rule
    def m_switch_type_spec(self):
        return self.match((
            'integer_type',
            'char_type',
            'boolean_type',
            'scoped_name',
        ))

    @nontrivial_rule
    def m_case(self):
        rv = self.match_until(('case_label'), ('element_spec'))
        self.m_exact(';')
        return rv

    @nontrivial_rule
    def m_case_label(self):
        rv = [self.m_exact((
            'case',
            'default',
        ))]
        if rv[0] == 'case':
            rv.append(self.const_expr())
        self.m_exact(':')
        return rv

    @nontrivial_rule
    def m_element_spec(self):
        return [self.m_type_spec(), self.m_declarator()]

    def m_enumerator(self):
        return self.m_identifier()

    @nontrivial_rule
    def m_enum_dcl(self):
        rv = [self.m_exact('enum'), self.m_identifier()]
        self.m_begin_scope()
        rv.append(self.comma_list_of(('enumerator')))
        self.m_end_scope()
        return rv

    @nontrivial_rule
    def m_array_declarator(self):
        rv = ['array', self.m_identifier()]
        self.m_exact('[')
        rv.append(self.m_positive_int_const())
        self.m_exact(']')
        return rv

    def m_simple_declarator(self):
        return self.m_identifier()

    @nontrivial_rule
    def m_typedef_dcl(self):
        return [self.m_exact('typedef'), self.m_type_declarator()]

    @nontrivial_rule
    def m_type_declarator(self):
        return [self.match((
            'simple_type_spec',
            'template_type_spec',
            'constr_type_dcl',
        )), self.m_any_declarators()]

    @nontrivial_rule
    def m_any_declarators(self):
        return self.comma_list_of(('any_declarator'))

    @nontrivial_rule
    def m_any_declarator(self):
        return self.match((
            # Fliped from spec because otherwise array_declarator would never
            # match when it needs to.
            'array_declarator',
            'simple_declarator',
        ))

    @nontrivial_rule
    def m_declarators(self):
        return self.comma_list_of(('declarator'))

    @nontrivial_rule
    def m_declarator(self):
        return self.match((
            'simple_declarator',  # Building Block Core Data Types
            'array_declarator',  # Building Block Anonymous Types
        ))

    # Building Block Annotations ==============================================

    @nontrivial_rule
    def m_annotation_appl_params(self):
        self.m_exact('(')
        rv = self.match(('annotation_appl_named_params', 'const_expr'))
        self.m_exact(')')
        return rv

    @nontrivial_rule
    def m_annotation_appl(self):
        self.stream.in_annotation = True
        try:
            self.m_exact('@')
            name = self.m_scoped_name()
            params = self.m_annotation_appl_params_maybe()
            if params is None:
                params = []
        except Exception:
            raise
        finally:
            self.stream.in_annotation = False
        return [name, params]

    @nontrivial_rule
    def m_annotation_appl_named_param(self):
        name = self.m_identifier()
        self.m_exact('=')
        return name, self.m_const_expr()

    @nontrivial_rule
    def m_annotation_appl_named_params(self):
        return self.comma_list_of(('annotation_appl_named_param'))
