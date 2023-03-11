from pathlib import Path
from io import StringIO
import enum

from pcpp import Preprocessor

from ..utils import Location, is_sequence, must_be_sequence, Configurable
from ..errors import (
    ErrorsReported,
    ExpectedError,
    PreprocessorError,
)
from .. import tree
from ..const_expr import ConstValue, Op, ConstExpr
from ..parser import Parser, nontrivial_rule, Rule
from .tokenizer import IdlTokenizer, Token, TokenKind, dump_tokens, \
    set_location_from_line_statement
from ..log import log_error, log_warning


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


class LeadingTokensData:
    # Simplified structure example:
    # {
    #   TokenKind.DOUBLE: {None: (f64, FloatingPoint.get_node)},
    #   TokenKind.SHORT: {None: (i16, Integer.get_node)},
    #   TokenKind.LONG: {
    #       None: (i32, Integer.get_node),
    #       TokenKind.DOUBLE: {None: f128, FloatingPoint.get_node},
    #   },
    # }

    def __init__(self, data):
        self.data = data

    @classmethod
    def from_input(cls, input_data, get_node, inspect_annotations):
        if not isinstance(input_data, dict):
            input_data = {tk: None for tk in must_be_sequence(input_data)}
        data = {}
        for token_kinds, hint in input_data.items():
            put = data
            for token_kind in must_be_sequence(token_kinds):
                if token_kind not in put:
                    put[token_kind] = {}
                put = put[token_kind]
            put[None] = (hint, get_node, inspect_annotations)
        return cls(data)

    def _copy(self, this_dict, other_dict):
        for token_kind, what in other_dict.items():
            if token_kind is None:
                if token_kind in this_dict:
                    raise RuntimeError('Leading token rule conflict')
                this_dict[token_kind] = what
            else:
                if token_kind not in this_dict:
                    this_dict[token_kind] = {}
                self._copy(this_dict[token_kind], what)

    def copy_from(self, other):
        self._copy(self.data, other.data)

    def match_i(self, parser, help_string):
        parser.m_ws_after()  # Stop at any annotation

        # Peek over the tokens comming up in the stream and see if they match
        # any pattern we're looking for. Don't advance past annotations so we
        # can give a chance for a callback to inspect them.
        got_tokens = []
        leading_tokens = self.data
        token = None
        peek_pos = 0
        has_annotations = False
        while True:
            parser.assert_not_end(lambda: [str(tk) for tk in leading_tokens.keys()])
            token = parser.stream.peek(offset=peek_pos)[0]
            if token.kind in leading_tokens:
                got_tokens.append(token)
                leading_tokens = leading_tokens[token.kind]
            elif token.kind is TokenKind.preprocessor_statement:
                pass
            elif token.kind is TokenKind.preparsed_annotation:
                has_annotations = True
            elif not token.is_ws():
                break
            peek_pos += 1
        if None not in leading_tokens:
            # Advance the stream so error reporting is as correct as possible.
            for token in got_tokens:
                parser.m_token(token.kind)
            return True, [token], None, None, None

        # Now do the annotation callback and advance the stream
        hint, get_node, inspect_annotations = leading_tokens[None]
        annotations = inspect_annotations() if has_annotations else []
        for token in got_tokens:
            parser.m_token(token.kind)

        return False, got_tokens, hint, get_node, annotations

    def match(self, parser, help_string, failed_cb=None):
        failed, tokens, hint, get_node, annotations = self.match_i(parser, help_string)
        if failed:
            if failed_cb is not None:
                rv = failed_cb(tokens[0])
                if rv is not None:
                    return rv
            raise ExpectedError(Location(parser.stream.loc()), help_string, repr(tokens[0]))
        rv = get_node(tokens, hint, annotations)
        if isinstance(rv, tree.Node):
            rv.annotations = annotations
        return rv


class LeadingTokenRule(Rule):
    '''\
    Rule that exposes the leading expected token or tokens to the parser so it
    can optimize matching rules to the current token from the stream.
    '''

    def __init__(self, parser_inst, name, help_string, leading_tokens, trivial=None):
        super().__init__(parser_inst, name, trivial=trivial)
        self.help_string = help_string
        self.leading_tokens_data = LeadingTokensData.from_input(
            leading_tokens, self.get_node, self.inspect_annotations)

    def init_impl(self):
        pass

    def get_node(self, leading_tokens, hint, annotations):
        raise NotImplementedError

    def inspect_annotations(self):
        parser = self.parser_inst
        return parser.get_annotations()

    def match(self):
        return self.leading_tokens_data.match(self.parser_inst, self.help_string)


class LeadingTokenMultiRule(Rule):
    '''\
    This takes a number of possible rules that differ in what the leading
    token is and pick what should be the correct rule based on that token.
    '''

    def __init__(self, parser_inst, name, help_string, child_rule_names, trivial=None):
        super().__init__(parser_inst, name, trivial=trivial)
        self.help_string = help_string
        self.child_rule_names = child_rule_names
        self.leading_tokens_data = None

    def init_impl(self):
        self.init_child_rules(self.child_rule_names)
        self.leading_tokens_data = LeadingTokensData({})
        for child_rule_name, child_rule_inst in self.child_rule_insts.items():
            child_rule_inst.init()
            self.leading_tokens_data.copy_from(child_rule_inst.leading_tokens_data)
            if child_rule_inst.child_rule_methods is not None:
                for rule_name, method in child_rule_inst.child_rule_methods.items():
                    if rule_name not in self.child_rule_methods:
                        self.child_rule_methods[rule_name] = method

    def get_node(self, node):
        return node

    # TODO: Remove this and child_rule_methods?
    def method_rule_fallback(self, failed_token):
        # Fallback to method rules if there are any
        return self.parser_inst.match_maybe(self.child_rule_methods.keys())

    def match(self):
        return self.get_node(self.leading_tokens_data.match(
            self.parser_inst, self.help_string, failed_cb=self.method_rule_fallback))


class UnsupportedAnnotations(enum.Enum):
    warn_once = 'warn-once'
    warn_all = 'warn-all'
    error = 'error'
    ignore = 'ignore'

    def __bool__(self):
        return self != self.ignore

    def __str__(self):
        return self.value

    def __repr__(self):
        return str(self)


class IdlParser(Parser, Configurable):
    @classmethod
    def define_config_options(cls, options):
        options.add_options(dict(
            includes=[], defines=[],
            unsupported_annotations=UnsupportedAnnotations.warn_once,
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
        Parser.__init__(self)
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
        self.unsupported_annotations_seen_unknown = set()
        self.unsupported_annotations_seen_ignored = set()
        self.error_count = 0

    def log_error(self, arg, line):
        self.error_count += 1
        log_error(arg, line)

    def parse_idl(self, idl_file):
        if self.config['raise_parse_errors']:
            location_error_handler = None
        else:
            def location_error_handler(self, error):
                line = self.source_lines.get_line(error.location.source_key, error.location.line)
                self.log_error(error, line)
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

        # Pre-parse annotations
        self.in_annotation = False
        processed_tokens = self._parse(
            tokens, name, idl_file.source_key, over_chars=False,
            parse_error_handler=location_error_handler,
            start=self.start_preparse,
        )

        # Parse the Tokens into a Tree
        self.in_annotation = False
        root = self._parse(
            processed_tokens, name, idl_file.source_key, over_chars=False,
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
                set_location_from_line_statement(self.stream, t.text)
            elif annotations and t.kind is TokenKind.preparsed_annotation:
                self.stream.push_ignored_element(t.value)
                self.stream.advance()
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
    def m_identifier(self):
        return self.m_token(TokenKind.identifier).text

    def m_begin_scope(self):
        return self.m_token(TokenKind.lbrace)

    end_scope = TokenKind.rbrace

    @classmethod
    def is_end_scope(cls, what):
        return isinstance(what, Token) and what.kind == cls.end_scope

    def m_end_scope(self):
        return self.m_token(self.end_scope)

    def dcl_header_check_forward(self):
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

    def start_preparse(self):
        results = []
        while not self.stream.done():
            t = self.stream.peek()[0]
            if t.kind is TokenKind.at:
                result = Token(t.loc, 'TODO',
                    TokenKind.preparsed_annotation, value=self.m_annotation_appl())
            else:
                result = t
                self.stream.advance()
            results.append(result)
        return results

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

    class Rule_definition(LeadingTokenMultiRule):
        def __init__(self, parser_inst, name):
            super().__init__(parser_inst, name, 'module, constant, type, or interface', [
                'module_dcl',
                'const_dcl',
                'type_dcl',
                # TODO: 'except_dcl'  # Building Block Interfaces - Basic
                'interface_dcl',  # Building Block Interfaces - Basic
            ], trivial=False)

        def get_node(self, node):
            self.parser_inst.m_token(TokenKind.semicolon)
            return node

    class Rule_module_dcl(LeadingTokenRule):
        def __init__(self, parser_inst, name):
            super().__init__(parser_inst, name, 'module', TokenKind.MODULE, trivial=False)

        def get_node(self, leading_tokens, hint, annotations):
            module = tree.ModuleNode(self.parser_inst.m_identifier())
            self.parser_inst.add_children_in_scope(module, ('definition',),
                at_least_one=not self.parser_inst.config['allow_empty_modules'])
            return module

    @nontrivial_rule
    def m_scoped_name(self):
        parts = []
        absolute = self.m_token_maybe(TokenKind.scope_sep)
        parts.append(self.m_identifier())
        while self.m_token_maybe(TokenKind.scope_sep):
            parts.append(self.m_identifier())
        return tree.ScopedName(parts, absolute)

    class Rule_const_dcl(LeadingTokenRule):
        def __init__(self, parser_inst, name):
            super().__init__(
                parser_inst, name, 'constant declaration', TokenKind.CONST, trivial=False)

        def get_node(self, leading_tokens, hint, annotations):
            constant = tree.ConstantNode(self.parser_inst.m_const_type())
            constant.name = self.parser_inst.m_identifier()
            self.parser_inst.m_token(TokenKind.equals)
            constant.value = self.parser_inst.m_const_expr()
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

    class Rule_literal(LeadingTokenRule):
        def __init__(self, parser_inst, name):
            super().__init__(parser_inst, name, 'literal value', {
                TokenKind.boolean: tree.PrimitiveKind.boolean,  # boolean_literal
                TokenKind.floating_point: tree.PrimitiveKind.f128,  # floating_pt_literal
                # TODO: fixed_pt_literal
                TokenKind.integer: tree.PrimitiveKind.u128,  # integer_literal
                TokenKind.char: tree.PrimitiveKind.c8,  # character_literal
                TokenKind.wchar: tree.PrimitiveKind.c16,  # wide_character_literal
                TokenKind.string: tree.PrimitiveKind.s8,  # string_literal
                TokenKind.wstring: tree.PrimitiveKind.s16,  # wide_string_literal
            }, trivial=True)

        def get_node(self, leading_tokens, hint, annotations):
            return ConstValue(leading_tokens[0].value, hint)

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
        rv = self.match((
            'scoped_name',
            'literal',
            'priority',
        ))
        if isinstance(rv, tree.ScopedName):
            rv = ConstValue(rv, None)
        return rv

    @nontrivial_rule
    def m_priority(self):
        self.m_token(TokenKind.lparens)
        rv = self.m_const_expr()
        self.m_token(TokenKind.rparens)
        return ConstExpr(Op.PRIORITIZE, rv)

    def m_positive_int_const(self):
        return self.m_const_expr()

    class Rule_type_dcl(LeadingTokenMultiRule):
        def __init__(self, parser_inst, name):
            super().__init__(parser_inst, name, 'type declaration', [
                'constr_type_dcl',
                # TODO: 'native_dcl',
                'typedef_dcl',
            ], trivial=False)

    @nontrivial_rule
    def m_type_spec(self):
        return self.match((
            'simple_type_spec',  # Building Block Core Data Types
            'template_type_spec',  # Building Block Anonymous Types
        ))

    class Rule_simple_type_spec(LeadingTokenMultiRule):
        def __init__(self, parser_inst, name):
            super().__init__(parser_inst, name, 'type name', [
                'base_type_spec',
                'scoped_name',
            ], trivial=False)

    class Rule_base_type_spec(LeadingTokenMultiRule):
        def __init__(self, parser_inst, name):
            super().__init__(parser_inst, name, 'scalar primitive type', [
                # floats and ints are flipped from the spec order because otherwise
                # "long double" will match "long".
                'floating_pt_type',
                'integer_type',
                'char_type',
                'wide_char_type',
                'boolean_type',
                'octet_type',
            ], trivial=False)

    class PrimitiveNodeRule(LeadingTokenRule):
        def __init__(self, parser_inst, name, help_string, leading_tokens):
            super().__init__(parser_inst, name,
                help_string + ' type', leading_tokens, trivial=False)

        def get_node(self, leading_tokens, hint, annotations):
            return tree.PrimitiveNode(hint)

    class Rule_floating_pt_type(PrimitiveNodeRule):
        def __init__(self, parser_inst, name):
            super().__init__(parser_inst, name, 'floating point', {
                TokenKind.FLOAT: tree.PrimitiveKind.f32,
                TokenKind.DOUBLE: tree.PrimitiveKind.f64,
                (TokenKind.LONG, TokenKind.DOUBLE): tree.PrimitiveKind.f128,
            })

    class Rule_integer_type(PrimitiveNodeRule):
        def __init__(self, parser_inst, name):
            super().__init__(parser_inst, name, 'integer', {
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
            })

    class Rule_char_type(PrimitiveNodeRule):
        def __init__(self, parser_inst, name):
            super().__init__(parser_inst, name,
                'character', {TokenKind.CHAR: tree.PrimitiveKind.c8})

    class Rule_wide_char_type(PrimitiveNodeRule):
        def __init__(self, parser_inst, name):
            super().__init__(parser_inst, name,
                'wide character', {TokenKind.WCHAR: tree.PrimitiveKind.c16})

    class Rule_boolean_type(PrimitiveNodeRule):
        def __init__(self, parser_inst, name):
            super().__init__(parser_inst, name,
                'boolean', {TokenKind.BOOLEAN: tree.PrimitiveKind.boolean})

    class Rule_octet_type(PrimitiveNodeRule):
        def __init__(self, parser_inst, name):
            super().__init__(parser_inst, name,
                'octet', {TokenKind.OCTET: tree.PrimitiveKind.byte})

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

    class Rule_constr_type_dcl(LeadingTokenMultiRule):
        def __init__(self, parser_inst, name):
            super().__init__(parser_inst, name, 'composite type declaration', [
                'struct_dcl',
                'union_dcl',
                'enum_dcl',
                # TODO 'bitset_decl',
                'bitmask_dcl',
            ], trivial=False)

    class Rule_struct_dcl(LeadingTokenRule):
        def __init__(self, parser_inst, name):
            super().__init__(parser_inst, name, 'structure', TokenKind.STRUCT, trivial=False)

        def get_node(self, leading_tokens, hint, annotations):
            p = self.parser_inst
            name, forward_dcl = p.dcl_header_check_forward()
            struct = tree.StructNode(name, forward_dcl=forward_dcl)
            if forward_dcl:
                return struct
            p.add_children_in_scope(struct, ('member',), at_least_one=False)
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

    class Rule_union_dcl(LeadingTokenRule):
        def __init__(self, parser_inst, name):
            super().__init__(parser_inst, name, 'union', TokenKind.UNION, trivial=False)

        def get_node(self, leading_tokens, hint, annotations):
            name, forward_dcl = self.parser_inst.dcl_header_check_forward()
            union = tree.UnionNode(name, forward_dcl=forward_dcl)
            if forward_dcl:
                return union
            self.parser_inst.m_token(TokenKind.SWITCH)
            self.parser_inst.m_token(TokenKind.lparens)
            union.disc_type = self.parser_inst.m_switch_type_spec()
            self.parser_inst.m_token(TokenKind.rparens)
            self.parser_inst.add_children_in_scope(union, 'case')
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

    class Rule_enum_dcl(LeadingTokenRule):
        def __init__(self, parser_inst, name):
            super().__init__(parser_inst, name, 'enumeration', TokenKind.ENUM, trivial=False)

        def get_node(self, leading_tokens, hint, annotations):
            parser = self.parser_inst
            enum_node = tree.EnumNode()
            enum_node.name = parser.m_identifier()
            parser.m_begin_scope()
            enum_node.add_children(parser.comma_list_of('enumerator', parser.end_scope))
            parser.m_end_scope()
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

    class Rule_typedef_dcl(LeadingTokenRule):
        def __init__(self, parser_inst, name):
            super().__init__(parser_inst, name, 'typedef', TokenKind.TYPEDEF, trivial=False)

        def get_node(self, leading_tokens, hint, annotations):
            base_type, name_maybe_arrays = self.parser_inst.m_type_declarator()
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

    class Rule_interface_dcl(LeadingTokenRule):
        def __init__(self, parser_inst, name):
            super().__init__(parser_inst, name, 'interface', {
                (TokenKind.LOCAL, TokenKind.INTERFACE): (None,),
                TokenKind.INTERFACE: (None,),
            }, trivial=False)

        def get_node(self, leading_tokens, hint, annotations):
            # interface_forward_dcl / interface_header
            # local is Building Block CORBA-Specific - Interfaces
            parser = self.parser_inst
            name, forward_dcl = parser.dcl_header_check_forward()
            interface = tree.InterfaceNode(name, forward_dcl=forward_dcl,
                local=leading_tokens[0] is TokenKind.LOCAL)
            if forward_dcl:
                return interface
            # interface_inheritance_spec
            if parser.m_token_maybe(TokenKind.colon):
                parser.comma_list_of('scoped_name', parser.end_scope)
            # interface_body
            parser.add_children_in_scope(interface, ('export',), at_least_one=False)
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
        if self.m_token_maybe(TokenKind.RAISES):
            self.m_token(TokenKind.lparens)
            op.raises = self.comma_list_of('scoped_name', TokenKind.rparens)
            self.m_token(TokenKind.rparens)
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
        return rv if isinstance(rv, list) else [rv]

    @nontrivial_rule
    def m_annotation_appl(self):
        self.in_annotation = True
        try:
            at = self.m_token(TokenKind.at)
            name = self.m_scoped_name()
            node = tree.UnknownAnnotationNode(name, self.m_annotation_appl_params_maybe())
            node.loc = at.loc
        except Exception:
            raise
        finally:
            self.in_annotation = False
        return node

    @nontrivial_rule
    def m_annotation_appl_named_param(self):
        name = self.m_identifier()
        self.m_token(TokenKind.equals)
        return name, self.m_const_expr()

    @nontrivial_rule
    def m_annotation_appl_named_params(self):
        return self.comma_list_of('annotation_appl_named_param')

    def get_annotations(self, filter_func=None, max_count=None):
        self.m_ws_before()
        elements = self.stream.get_ignored_elements()
        indices = []
        for i, element in enumerate(elements):
            if filter_func is None or filter_func(element):
                indices.append(i)
        if max_count is not None:
            max_count = min(len(indices), abs(max_count))
            indices = indices[:max_count] if max_count >= 0 else indices[max_count:]
        return [elements[i] for i in indices]

    def get_annotations_by_name(self, name, max_count=None):
        name = str(name)
        return self.get_annotations(lambda e: e.name == name, max_count)

    def get_annotation_by_name(self, name):
        l = self.get_annotations_by_name(name, max_count=-1)
        return l[0] if len(l) > 0 else None

    def unsupported_annotations(self):
        return UnsupportedAnnotations(self.config['unsupported_annotations'])

    def handle_accepted_ignored_elements(self, ignored_elements):
        handle = self.unsupported_annotations()
        if handle:
            for anno in ignored_elements:
                if handle == handle.warn_once and \
                        anno.name in self.unsupported_annotations_seen_ignored:
                    continue
                what = (anno.loc, 'Unsupported annotation')
                line = self.source_lines.get_line(anno.loc.source_key, anno.loc.line)
                if handle == handle.error:
                    self.log_error(what, line)
                else:
                    log_warning(what, line)
                self.unsupported_annotations_seen_ignored.add(anno.name)
        return ignored_elements

    # Building Block Extended Data-Types ======================================

    class Rule_bitmask_dcl(LeadingTokenRule):
        def __init__(self, parser_inst, name):
            super().__init__(parser_inst, name, 'bitmask', TokenKind.BITMASK, trivial=False)

        def get_node(self, leading_tokens, hint, annotations):
            parser = self.parser_inst
            # TODO: at this point we can't get annotation from before the bitmask keyword
            # bit_bound = parser.get_annotation_by_name('bit_bound')
            rv = tree.BitMaskNode(parser.m_identifier(), 16)
            parser.m_begin_scope()
            rv.add_children(parser.comma_list_of('bit_value', parser.end_scope))
            parser.m_end_scope()
            return rv

    @nontrivial_rule
    def m_bit_value(self):
        position = self.get_annotation_by_name('position')
        return tree.BitValueNode(self.m_identifier(), position)
