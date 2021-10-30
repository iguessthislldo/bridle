from pathlib import Path
from io import StringIO
import re

from pcpp import Preprocessor

from ..utils import Location, is_sequence
from ..errors import ErrorsReported, ExpectedError, PreprocessorError
from .. import tree
from ..parser import Parser, nontrivial_rule
from .tokenizer import IdlTokenizer, Token, TokenKind, dump_tokens, \
    set_location_from_line_statement
from ..log import print_location_error


class IdlFile:
    direct_count = 0

    def __init__(self, path=None, direct_input=None, effective_path=None):
        if direct_input is not None and path is None:
            if effective_path is None:
                self.direct_count += 1
                self.path = '__DIRECT_INPUT_{}__'.format(self.direct_count)
                self.source_key = self.direct_count
            else:
                self.path = Path(effective_path)
                self.source_key = self.path.resolve()
            self.idl_file_contents = direct_input
        elif path is not None and direct_input is None:
            self.path = Path(path)
            self.source_key = self.path.resolve()
            self.idl_file_contents = self.path.read_text()
        else:
            raise ValueError('Either path or direct_input must be set. Not both or neither.')
        self.contents = None
        self.positions = []

    def name(self):
        return self.path

    def load(self, preprocessor):
        sio = StringIO()
        preprocessor.parse(self.idl_file_contents, str(self.path))
        preprocessor.write(sio)
        if preprocessor.return_code != 0:
            raise ErrorsReported('Uncaught Preprocessor Error')
        self.contents = sio.getvalue()


direct_input_re = re.compile(r'^__DIRECT_INPUT_(\d+)__$')


class SourceLines:
    """\
    Helper class for getting lines for diagnostic messages that show lines.
    Keys should be unique.
    """

    def __init__(self):
        self.sources = {}

    def add_text_source(self, key, text):
        self.sources[key] = text.split('\n')

    def add_path_source(self, path):
        if not isinstance(path, Path):
            raise TypeError('Dont know what to do with ' + repr(path))
        self.add_text_source(path, path.read_text())

    def add_idl_file_source(self, idl_file):
        self.add_text_source(idl_file.source_key, idl_file.idl_file_contents)

    def get_line(self, source_key, lineno):
        if isinstance(source_key, str):
            m = direct_input_re.match(source_key)
            if m:
                source_key = int(m[1])
            else:
                source_key = Path(source_key).resolve()
        if source_key not in self.sources and isinstance(source_key, Path):
            self.add_path_source(source_key)
        return self.sources[source_key][lineno - 1]


class IdlPreprocessor(Preprocessor):
    def __init__(self, source_lines, location_error_handler):
        self.source_lines = source_lines
        self.location_error_handler = location_error_handler
        super().__init__()

    def on_error(self, file, line, msg):
        try:
            loc = Location(source=file, source_key=file, line=line)
            loc.set_length(self.source_lines.get_line(file, line))
            raise PreprocessorError(loc, '{}', msg)
        except PreprocessorError as e:
            if self.location_error_handler is None:
                raise
            self.location_error_handler(self, e)


class Declarator:
    def __init__(self, name, array_dims=None):
        self.name = name
        self.array_dims = array_dims

    def get_type(self, base_type):
        return base_type if self.array_dims is None \
            else tree.ArrayNode(base_type, self.array_dims)


class IdlParser(Parser):
    default_default_settings = dict(
        includes=[], defines=[],
        warn_about_unsupported_annotations=True,
        debug_all=False,
        dump_pp_output=False,
        debug_tokenizer=False, dump_tokens=False,
        debug_parser=False, dump_raw_tree=False, dump_tree=False,
        raise_parse_errors=False,
    )

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
        self.tokenizer = IdlTokenizer()
        self.source_lines = SourceLines()

    def parse_idl(self, settings, idl_file):
        if settings['raise_parse_errors']:
            location_error_handler = None
        else:
            def location_error_handler(self, error):
                line = self.source_lines.get_line(error.location.source_key, error.location.line)
                print_location_error(error, line)
                raise ErrorsReported('Syntax error occurred')

        # Preprocessor Phase
        preprocessor = IdlPreprocessor(self.source_lines, location_error_handler)
        for include in settings['includes']:
            preprocessor.add_path(include)
        for define in self.builtin_defines + settings['defines']:
            preprocessor.define(define.replace('=', ' ', 1))
        idl_file.load(preprocessor)
        if settings['dump_pp_output']:
            print(idl_file.contents)

        # Parse the Text into Tokens
        name = idl_file.name()
        tokens = self.tokenizer.tokenize(idl_file.contents, name, idl_file.source_key,
            debug=settings['debug_tokenizer'],
            parse_error_handler=location_error_handler,
        )
        if settings['dump_tokens']:
            dump_tokens(tokens)

        # Parse the Tokens into a Tree
        root = self._parse(
            tokens, name, idl_file.source_key, over_chars=False,
            debug=settings['debug_parser'],
            parse_error_handler=location_error_handler,
        )
        if settings['dump_raw_tree']:
            root.dump()

        # Process the Raw Tree
        root.finalize()
        if settings['dump_tree']:
            root.dump()

        return root

    def parse(self, paths=[], direct_inputs=[], effective_path=None, **kw):
        settings = self.default_settings.copy()
        settings.update(kw)
        if settings['debug_all']:
            settings['dump_pp'] = True
            settings['debug_tokenizer'] = True
            settings['dump_tokens'] = True
            settings['debug_parser'] = True
            settings['dump_raw_tree'] = True
            settings['dump_tree'] = True

        idl_files = [IdlFile(path=path) for path in paths] + \
            [IdlFile(direct_input=s, effective_path=effective_path) for s in direct_inputs]
        roots = []
        for idl_file in idl_files:
            self.source_lines.add_idl_file_source(idl_file)
            roots.append(self.parse_idl(settings, idl_file))
        return roots

    def comma_list_of(self, element_rules):
        rv = [self.match(element_rules)]
        while self.m_token_maybe(TokenKind.comma) is not None:
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
            t = self.stream.peek()[0]
            if t.is_ws():
                self.stream.advance()
            elif t.kind is TokenKind.preprocessor_statement:
                self.stream.advance()
                set_location_from_line_statement(self.stream.loc(), t.text)
            elif t.kind is TokenKind.at and annotations and not self.stream.in_annotation:
                self.stream.push_annotation(self.m_annotation_appl())
            else:
                break

    def m_ws_before(self):
        self._ws(annotations=True)

    def m_ws_after(self):
        self._ws(annotations=False)

    @nontrivial_rule
    def m_token(self, *token_kinds, ws_before=True, ws_after=True):
        if ws_before:
            self.m_ws_before()
        get_help_strings = lambda: [str(tk) for tk in token_kinds]
        self.assert_not_end(get_help_strings)
        loc = Location(self.stream.loc())
        token = next(self.stream)[0]
        if token.kind not in token_kinds:
            loc.set_length(token)
            raise ExpectedError(loc, get_help_strings(), repr(token))
        if ws_after:
            self.m_ws_after()
        return token

    def match_single_rule(self, rule, args):
        if isinstance(rule, TokenKind):
            try:
                return True, self.m_token(rule)
            except ExpectedError:
                return False, None
        return super().match_single_rule(rule, args)

    @nontrivial_rule
    def m_token_seq(self, token_seq, result):
        for token in token_seq:
            self.m_token(token)
        return result

    @nontrivial_rule
    def m_token_seqs(self, token_seqs):
        token_seqs = {(k,) if not is_sequence(k) else k: v for k, v in token_seqs.items()}
        loc = Location(self.stream.loc())
        rv = None
        for token_seq, result in token_seqs.items():
            rv = self.m_token_seq_maybe(token_seq, result)
            if rv is not None:
                break
        if rv is None:
            help_strings = [repr(' '.join([str(k) for k in i])) for i in token_seqs.keys()]
            raise ExpectedError(loc, help_strings, self.stream.peek())
        return rv

    @nontrivial_rule
    def m_boolean_literal(self):
        return self.m_token(TokenKind.boolean).value

    @nontrivial_rule
    def m_identifier(self):
        return self.m_token(TokenKind.identifier).text

    @nontrivial_rule
    def m_floating_pt_literal(self):
        return self.m_token(TokenKind.floating_point).value

    @nontrivial_rule
    def m_integer_literal(self):
        return self.m_token(TokenKind.integer).value

    @nontrivial_rule
    def m_character_literal(self):
        return self.m_token(TokenKind.char).value

    @nontrivial_rule
    def m_wide_character_literal(self):
        return self.m_token(TokenKind.wchar).value

    @nontrivial_rule
    def m_string_literal(self):
        return self.m_token(TokenKind.string).value

    @nontrivial_rule
    def m_wide_string_literal(self):
        return self.m_token(TokenKind.wstring).value

    def m_begin_scope(self):
        return self.m_token(TokenKind.lbrace)

    end_scope = TokenKind.rbrace

    @classmethod
    def is_end_scope(cls, what):
        return isinstance(what, Token) and what.kind == cls.end_scope

    def m_end_scope(self):
        return self.m_token(self.end_scope)

    # =========================================================================
    # The methods below should follow the names and order of the grammar rules
    # in the spec for the most part.
    # =========================================================================

    def start(self):
        root = tree.Tree(self.stream.loc())
        while not self.stream.done():
            root.add_child(self.m_definition())
        return root

    # Building Block Core Data Types ==========================================

    @nontrivial_rule
    def m_definition(self):
        rv = self.match((
            'module_dcl',
            'const_dcl',
            'type_dcl',
        ))
        self.m_token(TokenKind.semicolon)
        return rv

    @nontrivial_rule
    def m_module_dcl(self):
        self.m_token(TokenKind.MODULE)
        module = tree.ModuleNode(self.m_identifier())
        self.m_begin_scope()
        module.add_child(self.m_definition())
        while True:
            what = self.match((
                'definition',
                'end_scope',
            ))
            if self.is_end_scope(what):
                break
            module.add_child(what)
        return module

    @nontrivial_rule
    def m_scoped_name(self):
        parts = []
        absolute = self.m_token_maybe(TokenKind.scope_sep)
        parts.append(self.m_identifier())
        while self.m_token_maybe(TokenKind.scope_sep):
            parts.append(self.m_identifier())
        return tree.ScopedName(parts, absolute)

    @nontrivial_rule
    def m_const_dcl(self):
        self.m_token(TokenKind.CONST)
        constant = tree.ConstantNode(self.m_const_type())
        constant.name = self.m_identifier()
        self.m_token(TokenKind.equals)
        constant.value = self.m_const_expr()
        return constant

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
    def m_literal(self):
        return self.match((
            'integer_literal',
            'floating_pt_literal',
            # TODO: 'fixed_pt_literal',
            'character_literal',
            'wide_character_literal',
            'boolean_literal',
            'string_literal',
            'wide_string_literal',
        ))

    @nontrivial_rule
    def m_const_expr(self):
        # TODO: Real Rules
        return self.match((
            'literal',
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
        return tree.PrimitiveNode(self.m_token_seqs({
            TokenKind.FLOAT: tree.PrimitiveKind.f32,
            TokenKind.DOUBLE: tree.PrimitiveKind.f64,
            (TokenKind.LONG, TokenKind.DOUBLE): tree.PrimitiveKind.f128,
        }))

    @nontrivial_rule
    def m_integer_type(self):
        return tree.PrimitiveNode(self.m_token_seqs({
            (TokenKind.UNSIGNED, TokenKind.LONG, TokenKind.LONG): tree.PrimitiveKind.u64,
            TokenKind.UINT64: tree.PrimitiveKind.u64,
            (TokenKind.LONG, TokenKind.LONG): tree.PrimitiveKind.i64,
            TokenKind.INT64: tree.PrimitiveKind.i64,
            (TokenKind.UNSIGNED, TokenKind.LONG): tree.PrimitiveKind.u32,
            TokenKind.UINT32: tree.PrimitiveKind.u32,
            TokenKind.LONG: tree.PrimitiveKind.i32,
            TokenKind.INT32: tree.PrimitiveKind.i32,
            (TokenKind.UNSIGNED, TokenKind.SHORT): tree.PrimitiveKind.u16,
            TokenKind.UINT16: tree.PrimitiveKind.u16,
            TokenKind.SHORT: tree.PrimitiveKind.i16,
            TokenKind.INT16: tree.PrimitiveKind.i16,
            TokenKind.UINT8: tree.PrimitiveKind.u8,
            TokenKind.INT8: tree.PrimitiveKind.i8,
        }))

    def m_char_type(self):
        return tree.PrimitiveNode(self.m_token_seqs({
            TokenKind.CHAR: tree.PrimitiveKind.c8}))

    def m_wide_char_type(self):
        return tree.PrimitiveNode(self.m_token_seqs({
            TokenKind.WCHAR: tree.PrimitiveKind.c16}))

    def m_boolean_type(self):
        return tree.PrimitiveNode(self.m_token_seqs({
            TokenKind.BOOLEAN: tree.PrimitiveKind.boolean}))

    def m_octet_type(self):
        return tree.PrimitiveNode(self.m_token_seqs({
            TokenKind.OCTET: tree.PrimitiveKind.byte}))

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
        self.m_token(TokenKind.SEQUENCE)
        self.m_token(TokenKind.less_than)
        base_type = self.m_type_spec()
        if self.m_token_maybe(TokenKind.greater_than):
            max_count = None
        else:
            self.m_token(TokenKind.comma)
            max_count = self.m_positive_int_const()
            self.m_token(TokenKind.greater_than)
        return tree.SequenceNode(base_type, max_count)

    def _string_type(self, is_wide=False):
        self.m_token(TokenKind.WSTRING if is_wide else TokenKind.STRING)
        string = tree.PrimitiveNode(
            tree.PrimitiveKind.s16 if is_wide else tree.PrimitiveKind.s8)
        if self.m_token_maybe(TokenKind.less_than):
            string.element_count_limit = self.m_positive_int_const()
            self.m_token(TokenKind.greater_than)
        return string

    @nontrivial_rule
    def m_string_type(self):
        return self._string_type()

    @nontrivial_rule
    def m_wide_string_type(self):
        return self._string_type(is_wide=True)

    @nontrivial_rule
    def m_constr_type_dcl(self):
        return self.match((
            'struct_dcl',
            'union_dcl',
            'enum_dcl',
            # TODO 'bitset_decl',
            'bitmask_dcl',
        ))

    @nontrivial_rule
    def m_struct_dcl(self):
        # TODO: struct_def, struct_forward_dcl
        self.m_token(TokenKind.STRUCT)
        struct = tree.StructNode(self.m_identifier())
        if self.m_begin_scope_maybe():
            while True:
                what = self.match((
                    'member',
                    'end_scope'
                ))
                if self.is_end_scope(what):
                    break
                struct.add_child(what)
        return struct

    @nontrivial_rule
    def m_member(self):
        type_spec = self.m_type_spec()
        members = []
        for declarator in self.m_declarators():
            members.append(tree.FieldNode(
                declarator.name, declarator.get_type(type_spec)))
        self.m_token(TokenKind.semicolon)
        return members

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
        return self.match((
            'union_def',
            'union_forward_dcl',
        ))

    @nontrivial_rule
    def m_union_def(self):
        self.m_token(TokenKind.UNION)
        union = tree.UnionNode(self.m_identifier())
        self.m_token(TokenKind.SWITCH)
        self.m_token(TokenKind.lparens)
        union.disc_type = self.m_switch_type_spec()
        self.m_token(TokenKind.rparens)
        self.m_begin_scope()
        cases, _ = self.match_until(('case'), ('end_scope'))
        union.add_children(cases)
        return union

    @nontrivial_rule
    def m_union_forward_dcl(self):
        self.m_token(TokenKind.UNION)
        union = tree.UnionNode(self.m_identifier())
        union.forward_dcl = True
        return union

    @nontrivial_rule
    def m_switch_type_spec(self):
        return self.match((
            'integer_type',
            'char_type',
            'boolean_type',
            'scoped_name',
            # Building Block Extended Data-Types
            'wide_char_type',
            'octet_type',
        ))

    @nontrivial_rule
    def m_case(self):
        case_labels, element_spec = self.match_until(('case_label'), ('element_spec'))
        type_spec, declarator = element_spec
        rv = tree.UnionBranchNode(declarator.name, declarator.get_type(type_spec))
        for is_default_case, value in case_labels:
            if is_default_case:
                rv.is_default_branch = True
            else:
                rv.cases.append(value)
        self.m_token(TokenKind.semicolon)
        return rv

    @nontrivial_rule
    def m_case_label(self):
        branch_kind_token = self.m_token(
            TokenKind.CASE,
            TokenKind.DEFAULT,
        )
        if branch_kind_token.kind == TokenKind.CASE:
            rv = (False, self.m_const_expr())
        elif branch_kind_token.kind == TokenKind.DEFAULT:
            rv = (True, None)
        else:
            assert False, "Should be CASE or DEFAULT"
        self.m_token(TokenKind.colon)
        return rv

    @nontrivial_rule
    def m_element_spec(self):
        return [self.m_type_spec(), self.m_declarator()]

    def m_enumerator(self):
        return tree.EnumeratorNode(self.m_identifier())

    @nontrivial_rule
    def m_enum_dcl(self):
        self.m_token(TokenKind.ENUM)
        enum_node = tree.EnumNode()
        enum_node.name = self.m_identifier()
        self.m_begin_scope()
        enum_node.add_children(self.comma_list_of(('enumerator')))
        self.m_end_scope()
        return enum_node

    @nontrivial_rule
    def m_array_declarator(self):
        # TODO: Multidem arrays
        name = self.m_identifier()
        self.m_token(TokenKind.lbracket)
        size = self.m_positive_int_const()
        self.m_token(TokenKind.rbracket)
        return Declarator(name, [size])

    def m_simple_declarator(self):
        return Declarator(self.m_identifier())

    @nontrivial_rule
    def m_typedef_dcl(self):
        self.m_token(TokenKind.TYPEDEF)
        base_type, name_maybe_arrays = self.m_type_declarator()
        typedefs = []
        for name_maybe_array in name_maybe_arrays:
            typedefs.append(tree.TypedefNode(
                name_maybe_array.name, name_maybe_array.get_type(base_type)))
        return typedefs

    @nontrivial_rule
    def m_type_declarator(self):
        return (self.match((
            'simple_type_spec',
            'template_type_spec',
            'constr_type_dcl',
        )), self.m_any_declarators())

    @nontrivial_rule
    def m_any_declarators(self):
        return self.comma_list_of(('any_declarator'))

    @nontrivial_rule
    def m_any_declarator(self):
        return self.match((
            # Flipped from spec because otherwise array_declarator would never
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
            # Flipped from spec because otherwise array_declarator would never
            # match when it needs to.
            'array_declarator',  # Building Block Anonymous Types
            'simple_declarator',  # Building Block Core Data Types
        ))

    # Building Block Annotations ==============================================

    @nontrivial_rule
    def m_annotation_appl_params(self):
        self.m_token(TokenKind.lparens)
        rv = self.match(('annotation_appl_named_params', 'const_expr'))
        self.m_token(TokenKind.rparens)
        return rv

    @nontrivial_rule
    def m_annotation_appl(self):
        self.stream.in_annotation = True
        try:
            self.m_token(TokenKind.at)
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
        self.m_token(TokenKind.equals)
        return name, self.m_const_expr()

    @nontrivial_rule
    def m_annotation_appl_named_params(self):
        return self.comma_list_of('annotation_appl_named_param')

    def get_annotations(self, name):
        self.m_ws_before()
        return self.stream.get_annotations(name)

    def get_annotation(self, name):
        l = self.get_annotations(name)
        return l[-1] if l else None

    # Building Block Extended Data-Types ======================================

    @nontrivial_rule
    def m_bitmask_dcl(self):
        bit_bound = self.get_annotation('bit_bound')
        self.m_token(TokenKind.BITMASK)
        rv = tree.BitMaskNode(self.m_identifier(), bit_bound)
        self.m_begin_scope()
        rv.add_children(self.comma_list_of(('bit_value')))
        self.m_end_scope()
        return rv

    @nontrivial_rule
    def m_bit_value(self):
        position = self.get_annotation('position')
        return tree.BitValueNode(self.m_identifier(), position)
