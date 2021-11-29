from pathlib import Path
from io import StringIO

from pcpp import Preprocessor

from ..utils import Location, is_sequence, Configurable
from ..errors import (
    ErrorsReported,
    ExpectedError,
    PreprocessorError,
)
from .. import tree
from ..const_expr import ConstValue, Op, ConstExpr
from ..parser import Parser, nontrivial_rule
from .tokenizer import IdlTokenizer, Token, TokenKind, dump_tokens, \
    set_location_from_line_statement
from ..log import print_location_error


class IdlFile:

    def __init__(self, path=None, direct_input=None, effective_path=None):
        self.path, self.source_key = Location.make_new_source_key(
            path=path, effective_path=effective_path)
        if direct_input is not None and path is None:
            self.idl_file_contents = direct_input
        elif path is not None and direct_input is None:
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
        source_key = Location.get_source_key(source_key)
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


class IdlParser(Parser, Configurable):
    @classmethod
    def define_config_options(cls, options):
        options.add_options(dict(
            includes=[], defines=[],
            warn_about_unsupported_annotations=True,
            debug_all=False,
            dump_pp_output=False,
            dump_tokens=False,
            debug_parser=False, dump_raw_tree=False, dump_tree=False,
            raise_parse_errors=False,
            allow_empty_modules=False,
            allow_trailing_comma=False,
            finalize=True,
        ))
        options.add_child_options('tokenizer_', IdlTokenizer)

    def __init__(self, **config):
        Configurable.__init__(self, config)
        self.builtin_define_base_name = '__BRIDLE'
        self.builtin_defines = [self.builtin_define_base_name]
        self.builtin_defines.extend([self.builtin_define_base_name + i for i in [
            # TODO: Bridle Version
            '_IDL_VERSION_MAJOR=4',
            '_IDL_VERSION_MINOR=2',
        ]])
        self.tokenizer = IdlTokenizer(config_parent=('tokenizer_', self))
        self.source_lines = SourceLines()

    def parse_idl(self, idl_file):
        if self.config['raise_parse_errors']:
            location_error_handler = None
        else:
            def location_error_handler(self, error):
                line = self.source_lines.get_line(error.location.source_key, error.location.line)
                print_location_error(error, line)
                raise ErrorsReported('Syntax error occurred')

        # Preprocessor Phase
        preprocessor = IdlPreprocessor(self.source_lines, location_error_handler)
        for include in self.config['includes']:
            preprocessor.add_path(include)
        for define in self.builtin_defines + self.config['defines']:
            preprocessor.define(define.replace('=', ' ', 1))
        idl_file.load(preprocessor)
        if self.config['dump_pp_output']:
            print(idl_file.contents)

        # Parse the Text into Tokens
        name = idl_file.name()
        tokens = self.tokenizer.tokenize(idl_file.contents, name, idl_file.source_key,
            debug=self.config['tokenizer_debug'],
            parse_error_handler=location_error_handler,
        )
        if self.config['dump_tokens']:
            dump_tokens(tokens)

        # Parse the Tokens into a Tree
        self.in_annotation = False
        root = self._parse(
            tokens, name, idl_file.source_key, over_chars=False,
            debug=self.config['debug_parser'],
            parse_error_handler=location_error_handler,
        )
        if self.config['dump_raw_tree']:
            root.dump()

        # Process the Raw Tree
        if self.config['finalize']:
            root.finalize()
        if self.config['dump_tree']:
            root.dump()

        return root

    def parse(self, paths=[], direct_inputs=[], effective_path=None, **override_config):
        with self.config.new_ctx(override_config):
            if self.config['debug_all']:
                self.config['dump_pp'] = True
                self.config['tokenizer_debug'] = True
                self.config['dump_tokens'] = True
                self.config['debug_parser'] = True
                self.config['dump_raw_tree'] = True
                self.config['dump_tree'] = True

            idl_files = [IdlFile(path=path) for path in paths] + \
                [IdlFile(direct_input=s, effective_path=effective_path) for s in direct_inputs]
            roots = []
            for idl_file in idl_files:
                self.source_lines.add_idl_file_source(idl_file)
                roots.append(self.parse_idl(idl_file))
            return roots

    def comma_list_of(self, repeating_rules, terminating_token_kind=None, at_least_one=True):
        rv = []
        if at_least_one:
            first = self.match(repeating_rules)
        else:
            first = self.match_maybe(repeating_rules)
            if first is None:
                return rv
        rv.append(first)
        allow_trailing_comma = \
            self.config['allow_trailing_comma'] and terminating_token_kind is not None
        while True:
            if self.m_token_maybe(TokenKind.comma) is None:
                break
            if allow_trailing_comma:
                t = self.stream.peek()[0]
                if t.kind is terminating_token_kind:
                    break
            rv.append(self.match(repeating_rules))
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
            elif t.kind is TokenKind.at and annotations and not self.in_annotation:
                self.stream.push_ignored_element(self.m_annotation_appl())
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

        def get_help_strings():
            return [str(tk) for tk in token_kinds]

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
        return ConstValue(self.m_token(TokenKind.boolean).value,
            tree.PrimitiveKind.boolean)

    @nontrivial_rule
    def m_identifier(self):
        return self.m_token(TokenKind.identifier).text

    @nontrivial_rule
    def m_floating_pt_literal(self):
        return ConstValue(self.m_token(TokenKind.floating_point).value,
            tree.PrimitiveKind.f128)

    @nontrivial_rule
    def m_integer_literal(self):
        return ConstValue(self.m_token(TokenKind.integer).value,
            tree.PrimitiveKind.u128)

    @nontrivial_rule
    def m_character_literal(self):
        return ConstValue(self.m_token(TokenKind.char).value,
            tree.PrimitiveKind.c8)

    @nontrivial_rule
    def m_wide_character_literal(self):
        return ConstValue(self.m_token(TokenKind.wchar).value,
            tree.PrimitiveKind.c16)

    @nontrivial_rule
    def m_string_literal(self):
        return ConstValue(self.m_token(TokenKind.string).value,
            tree.PrimitiveKind.s8)

    @nontrivial_rule
    def m_wide_string_literal(self):
        return ConstValue(self.m_token(TokenKind.wstring).value,
            tree.PrimitiveKind.s16)

    def m_begin_scope(self):
        return self.m_token(TokenKind.lbrace)

    end_scope = TokenKind.rbrace

    @classmethod
    def is_end_scope(cls, what):
        return isinstance(what, Token) and what.kind == cls.end_scope

    def m_end_scope(self):
        return self.m_token(self.end_scope)

    def dcl_header_check_forward(self, token_kind):
        self.m_token(token_kind)
        name = self.m_identifier()
        self.assert_not_end(lambda: [str(TokenKind.semicolon)])
        next_token = self.stream.peek()[0]
        return name, next_token.kind is TokenKind.semicolon

    def add_children_in_scope(self, parent, child_rules, at_least_one=True):
        self.m_begin_scope()
        children, _ = self.match_until(child_rules, 'end_scope', at_least_one)
        for c in children:
            if isinstance(c, list):
                parent.add_children(c)
            else:
                parent.add_child(c)

    # =========================================================================
    # The methods below should follow the names and order of the grammar rules
    # in the spec for the most part.
    # =========================================================================

    def start(self):
        root = tree.Tree(self.stream.loc())
        if not self.config['allow_empty_modules']:
            root.add_child(self.m_definition())
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
            # TODO: 'except_dcl'  # Building Block Interfaces - Basic
            'interface_dcl'  # Building Block Interfaces - Basic
        ))
        self.m_token(TokenKind.semicolon)
        return rv

    @nontrivial_rule
    def m_module_dcl(self):
        self.m_token(TokenKind.MODULE)
        module = tree.ModuleNode(self.m_identifier())
        self.add_children_in_scope(module, ('definition',),
            at_least_one=not self.config['allow_empty_modules'])
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

    def m_const_expr(self):
        return self.m_or_expr()

    def expr_helper(self, this_rule, next_rule, op_tokens):
        expr = self.match(next_rule)
        while True:
            op = self.m_token_seqs_maybe(op_tokens)
            if not op:
                break
            expr = ConstExpr(op, expr, self.match(next_rule))
        return expr

    @nontrivial_rule
    def m_or_expr(self):
        return self.expr_helper('or_expr', 'xor_expr', {
            TokenKind.pipe: Op.OR,
        })

    @nontrivial_rule
    def m_xor_expr(self):
        return self.expr_helper('xor_expr', 'and_expr', {
            TokenKind.carrot: Op.XOR,
        })

    @nontrivial_rule
    def m_and_expr(self):
        return self.expr_helper('and_expr', 'shift_expr', {
            TokenKind.ampersand: Op.AND,
        })

    @nontrivial_rule
    def m_shift_expr(self):
        return self.expr_helper('shift_expr', 'add_expr', {
            TokenKind.rshift: Op.RSHIFT,
            TokenKind.lshift: Op.LSHIFT,
        })

    @nontrivial_rule
    def m_add_expr(self):
        return self.expr_helper('add_expr', 'mult_expr', {
            TokenKind.plus: Op.ADD,
            TokenKind.minus: Op.SUBTRACT,
        })

    @nontrivial_rule
    def m_mult_expr(self):
        return self.expr_helper('mult_expr', 'unary_expr', {
            TokenKind.astreisk: Op.MULTIPLY,
            TokenKind.slash: Op.DIVIDE,
            TokenKind.percent: Op.MODULO,
        })

    @nontrivial_rule
    def m_unary_expr(self):
        op = self.m_token_seqs_maybe({
            TokenKind.minus: Op.NEGATIVE,
            TokenKind.plus: Op.POSITIVE,
            TokenKind.tilde: Op.INVERT,
        })
        pe = self.m_primary_expr()
        if op is not None:
            return ConstExpr(op, pe)
        return pe

    @nontrivial_rule
    def m_primary_expr(self):
        return self.match((
            'scoped_name',
            'literal',
            'priority',
        ))

    @nontrivial_rule
    def m_priority(self):
        self.m_token(TokenKind.lparens)
        rv = self.m_const_expr()
        self.m_token(TokenKind.rparens)
        return rv

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
        name, forward_dcl = self.dcl_header_check_forward(TokenKind.STRUCT)
        struct = tree.StructNode(name, forward_dcl=forward_dcl)
        if forward_dcl:
            return struct
        self.add_children_in_scope(struct, ('member',), at_least_one=False)
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

    @nontrivial_rule
    def m_union_dcl(self):
        name, forward_dcl = self.dcl_header_check_forward(TokenKind.UNION)
        union = tree.UnionNode(name, forward_dcl=forward_dcl)
        if forward_dcl:
            return union
        self.m_token(TokenKind.SWITCH)
        self.m_token(TokenKind.lparens)
        union.disc_type = self.m_switch_type_spec()
        self.m_token(TokenKind.rparens)
        self.add_children_in_scope(union, 'case')
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
        enum_node.add_children(self.comma_list_of('enumerator', self.end_scope))
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
        return self.comma_list_of('any_declarator')

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
        return self.comma_list_of('declarator')

    @nontrivial_rule
    def m_declarator(self):
        return self.match((
            # Flipped from spec because otherwise array_declarator would never
            # match when it needs to.
            'array_declarator',  # Building Block Anonymous Types
            'simple_declarator',  # Building Block Core Data Types
        ))

    # Building Block Interfaces - Basic =======================================

    # TODO: expect_dcl, attr_dcl, and friends

    @nontrivial_rule
    def m_interface_dcl(self):
        # interface_forward_dcl / interface_header
        # local is Building Block CORBA-Specific - Interfaces
        local = self.m_token_maybe(TokenKind.LOCAL) is not None
        name, forward_dcl = self.dcl_header_check_forward(TokenKind.INTERFACE)
        interface = tree.InterfaceNode(name, forward_dcl=forward_dcl, local=local)
        if forward_dcl:
            return interface
        # interface_inheritance_spec
        if self.m_token_maybe(TokenKind.colon):
            self.comma_list_of('scoped_name', self.end_scope)
        # interface_body
        self.add_children_in_scope(interface, ('export',), at_least_one=False)
        return interface

    @nontrivial_rule
    def m_export(self):
        rv = self.match((
            'op_dcl',
            # TODO: 'attr_dcl',
        ))
        self.m_token(TokenKind.semicolon)
        return rv

    @nontrivial_rule
    def m_op_dcl(self):
        return_type = self.match((
            'type_spec',
            TokenKind.VOID,
        ))
        op = tree.OpNode(self.m_identifier(), return_type)
        self.m_token(TokenKind.lparens)
        op.add_children(self.comma_list_of('parameter_dcl', TokenKind.rparens, at_least_one=False))
        self.m_token(TokenKind.rparens)
        # TODO: raises_expr
        return op

    @nontrivial_rule
    def m_parameter_dcl(self):
        attr = self.m_token_seqs({
            TokenKind.IN: tree.ParameterAttr.In,
            TokenKind.OUT: tree.ParameterAttr.Out,
            TokenKind.INOUT: tree.ParameterAttr.InOut,
        })
        type = self.m_type_spec()
        return tree.ParameterNode(self.m_simple_declarator().name, attr, type)

    # Building Block Annotations ==============================================

    @nontrivial_rule
    def m_annotation_appl_params(self):
        self.m_token(TokenKind.lparens)
        rv = self.match(('annotation_appl_named_params', 'const_expr'))
        self.m_token(TokenKind.rparens)
        return rv

    @nontrivial_rule
    def m_annotation_appl(self):
        self.in_annotation = True
        try:
            self.m_token(TokenKind.at)
            name = self.m_scoped_name()
            params = self.m_annotation_appl_params_maybe()
            if params is None:
                params = []
        except Exception:
            raise
        finally:
            self.in_annotation = False
        return [name, params]

    @nontrivial_rule
    def m_annotation_appl_named_param(self):
        name = self.m_identifier()
        self.m_token(TokenKind.equals)
        return name, self.m_const_expr()

    @nontrivial_rule
    def m_annotation_appl_named_params(self):
        return self.comma_list_of('annotation_appl_named_param')

    def get_annotations(self, filter_func, max_count=None):
        self.m_ws_before()
        elements = self.stream.get_ignored_elements()
        indices = []
        for i, element in enumerate(elements):
            if filter_func(element):
                indices.append(i)
        if max_count is not None:
            max_count = min(len(indices), abs(max_count))
            indices = indices[:max_count] if max_count >= 0 else indices[max_count:]
        return [elements.pop(i) for i in indices]

    def get_annotations_by_name(self, name, max_count=None):
        name = str(name)
        return self.get_annotations(lambda e: str(e[0]) == name, max_count)

    def get_annotation_by_name(self, name):
        l = self.get_annotations_by_name(name, max_count=-1)
        return l[0] if len(l) > 0 else None

    def handle_accepted_ignored_elements(self, ignored_elements):
        if self.config['warn_about_unsupported_annotations']:
            for element in ignored_elements:
                print('Ignored unsupported annotation', element)
        return ignored_elements

    # Building Block Extended Data-Types ======================================

    @nontrivial_rule
    def m_bitmask_dcl(self):
        bit_bound = self.get_annotation_by_name('bit_bound')
        self.m_token(TokenKind.BITMASK)
        rv = tree.BitMaskNode(self.m_identifier(), bit_bound)
        self.m_begin_scope()
        rv.add_children(self.comma_list_of('bit_value', self.end_scope))
        self.m_end_scope()
        return rv

    @nontrivial_rule
    def m_bit_value(self):
        position = self.get_annotation_by_name('position')
        return tree.BitValueNode(self.m_identifier(), position)
