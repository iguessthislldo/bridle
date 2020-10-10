from pathlib import Path
import sys
import re
from io import StringIO

import lark
from pcpp import Preprocessor

macro_regex = re.compile(r'^\s*#.*$', re.M)
line_regex = re.compile(r'^\s*#line (\d+)(?: "(.*)")?$', re.M)


class Error(Exception):
    pass


class ConstExpr:
    ops = {
        'or_expr': (2, '|'),
        'xor_expr': (2, '^'),
        'and_expr': (2, '&'),
        'shift_right_expr': (2, '>>'),
        'shift_left_expr': (2, '<<'),
        'add_expr': (2, '+'),
        'sub_expr': (2, '-'),
        'mul_expr': (2, '*'),
        'div_expr': (2, '/', '//'),
        'rem_expr': (2, '%'),
        'positive_expr': (1, '+'),
        'negative_expr': (1, '-'),
        'inverse_expr': (1, '~'),
    }

    class Operation:
        def __init__(self, a, op=None, b=None):
            self.operands = op[0] or None
            self.idl_op = op[1] or None
            self.py_op = op[2] if len(op) == 3 else self.idl_op
            self.a = a
            self.b = b

        def can_eval(self):
            if self.b is not None and not self.b.can_eval():
                return False
            return self.a.can_eval()

        def eval(self):
            if self.b is not None:
                return eval("{} {} {}".format(self.a.eval(), self.py_op, self.b.eval()))
            elif self.py_op is not None:
                return eval("{} {}".format(self.py_op, self.a.eval()))
            else:
                return self.a.value

        def __repr__(self):
            if self.b is not None:
                return "{} {} {}".format(repr(self.a), self.idl_op, repr(self.b))
            elif self.idl_op is not None:
                return "{}{}".format(self.idl_op, repr(self.a))
            else:
                return "({})".format(repr(self.op))

    literals = {
        'octal_literal': lambda s: int(s, 8),
        'decimal_literal': lambda s: int(s),
        'hexadecimal_literal': lambda s: int(s, 16),
        'string_literal': lambda s: eval(s),
    }

    def __init__(self, tree):
        self.value = None
        self.evaled = False
        # print(tree)
        if tree.data not in ('const_expr', 'literal') and tree.data not in self.ops:
            raise Error("ConstExpr requires an expression")
        if len(tree.children) == 1:
            if tree.data == 'const_expr':
                child = tree.children[0]
            else:
                child = tree
            if child.data == 'literal':
                child = child.children[0]
                self.value = self.literals[child.data](str(child.children[0]))
                self.evaled = True
            elif child.data in self.ops:
                # print(child)
                self.value = self.Operation(
                    ConstExpr(child.children[0]), self.ops[child.data], ConstExpr(child.children[1]))
                # print(repr(self.value))
                if self.value.can_eval():
                    self.value = self.value.eval()
                else:
                    assert(False)
            else:
                raise Error("Unexpected: " + repr(child))
        else:
            raise Error("const_expr {} has {} children".format(repr(tree), len(tree.children)))
        # print(repr(self))


    def can_eval(self):
        if isinstance(self.value, self.Operation):
            return self.value.can_eval()
        return self.evaled

    def eval(self):
        if isinstance(self.value, self.Operation):
            return self.value.eval()
        return self.value

    def repr_value(self):
        return repr(self.value)

    def __repr__(self):
        return '<ConstExpr: {}>'.format(self.repr_value())

class RawTreeVistior(lark.visitors.Interpreter):
    @staticmethod
    def get_scoped_name(tree):
        return [str(i) for i in tree.children]

    def __init__(self):
        self.indent = 0
        self.current_annotations = []
        self.results = []

    def p(self, *args, **kwargs):
        print('  ' * self.indent, end='')
        print(*args, **kwargs)

    def visit_children_indent(self, *args, **kwargs):
        self.indent += 1
        self.visit_children(*args, **kwargs)
        self.indent -= 1

    def print_current_annotations(self, where):
        if self.current_annotations:
            self.p('Start Annotations on', where)
            self.indent += 1
            for a in self.current_annotations:
                self.p(a)
            self.indent -= 1
            self.p('End Annotations on', where)
            self.current_annotations = []

    def const_expr(self, tree):
        self.results.append(ConstExpr(tree))

    def const_dcl(self, tree):
        self.results = []
        self.visit_children(tree)
        value = self.results.pop()
        name = str(tree.children[1])
        self.p(name, value.repr_value())

        unused_annotations = []
        for annotation in self.current_annotations:
            if annotation[0] == 'avidly::assert_value':
                expected = annotation[1].eval()
                actual = value.eval()
                if expected != actual:
                    raise Error('Expected {} to be {}, but it was {}'.format(name, repr(expected), repr(actual)))
            else:
                unused_annotations.append(annotation)
        self.current_annotations = unused_annotations

    def module_dcl(self, tree):
        name = str(tree.children[0])
        self.p('Start Module', name)
        self.visit_children_indent(tree)
        self.p('End Module', name)

    def enum_dcl(self, tree):
        name = str(tree.children[0])
        self.p('Start Enum', name)
        self.visit_children_indent(tree)
        self.p('End Enum', name)

    def enumerator(self, tree):
        name = str(tree.children[0])
        self.p('Enumerator', name)

    def struct_def(self, tree):
        name = str(tree.children[0])
        self.p('Start Struct', name)
        self.indent += 1
        self.print_current_annotations('Struct')
        self.indent -= 1
        self.visit_children_indent(tree)
        self.p('End Struct', name)

    def member(self, tree):
        children = iter(tree.children)

        # Annotations
        for child in children:
            if child.data != 'annotation_appl':
                break
            self.annotation_appl(child)
        self.print_current_annotations('Next Struct Member')

        type_spec = child.children[0]
        if type_spec.data == 'simple_type_spec':
            if type_spec.children[0].data == 'base_type_spec':
                type_spec = type_spec.children[0].children[0].data
            elif type_spec.children[0].data == 'scoped_name':
                type_spec = '::'.join(self.get_scoped_name(type_spec.children[0]))
            else:
                raise NotImplementedError
        elif type_spec.data == 'template_type_spec':
            type_spec = type_spec.children[0].data
        else:
            raise NotImplementedError
        child = next(children)
        for declarator in child.children:
            if declarator.data == 'simple_declarator':
                self.p(type_spec, str(declarator.children[0]))
            elif declarator.data == 'array_declarator':
                pass
                # array_size =
                # self.p(type_spec + , str(declarator.children[0]))
                # print(declarator.children)
            else:
                raise NotImplementedError

    def definition(self, tree):
        self.visit_children(tree)
        if self.current_annotations:
            print('Warning: unused annotations:', self.current_annotations)
            self.current_annotations = []

    def annotation_appl(self, tree):
        name = '::'.join(self.get_scoped_name(tree.children[0]))
        if len(tree.children) > 1:
            args = tree.children[1].children
            if len(args) == 1 and args[0].data == 'const_expr':
                annotation = (name, ConstExpr(args[0]), None)
            else:
                # TODO
                annotation = (name, None, None) # Mutiple args will go in 3rd place in tuple
        else:
            annotation = (name, None, None)
        self.current_annotations.append(annotation)

    def specification(self, tree):
        self.p('Start Root')
        self.visit_children_indent(tree)
        self.p('End Root')


def unexpected_token_handler(e):
    if e.token.type == 'AT':
        tokens = [e.token] + list(e.puppet._stream)
        skip = 1
        while skip < len(tokens):
            # print('CHECKING', ''.join([str(i) for i in tokens[:skip]]),
            #         '>>HERE<<', ''.join([str(i) for i in tokens[skip:]]))
            done = 'ANNOTATION_APPL_DONE'
            try:
                e.puppet.parser.avidly_annotations_only_parser.parse(
                    tokens[:skip] + [lark.lexer.Token(done, None)])
                # print('SUCCESS')
            except lark.exceptions.UnexpectedToken as ex:
                # print(ex.expected)
                if done in ex.expected:
                    skip -= 1
                    print(e.puppet.parser.avidly_idl_file.get_position(e.pos_in_stream),
                        'Skipping annotation in unsupported place',
                        ''.join([str(i) for i in tokens[:skip]]))
                    e.puppet._stream = iter(tokens[skip:])
                    return True
            skip += 1
    return False


class IdlFile:
    def __init__(self, path):
        self.path = path
        self.contents = None
        self.positions = []

    def load(self, preprocessor):
        sio = StringIO()
        preprocessor.parse(self.path.read_text(), str(self.path))
        preprocessor.write(sio)
        if preprocessor.return_code != 0:
            sys.exit('Encounted Preprocessor Error(s)')
        raw_contents = sio.getvalue()
        # print('================================================================================')
        # print(raw_contents)
        # print('================================================================================')

        def preprocessor_result_iter(contents, regex):
            prev_match = None
            for match in regex.finditer(contents):
                if prev_match is not None:
                    if prev_match.end() - match.start():
                        start = prev_match.end()
                        if contents[start] == '\n':
                            start += 1
                        yield (start, match.start(), prev_match)
                prev_match = match
            start = 0
            if prev_match is not None:
                start = prev_match.end()
                if contents[start] == '\n':
                    start += 1
            end = len(contents)
            if start - end:
                yield (start, end, prev_match)

        for start, end, match in preprocessor_result_iter(raw_contents, line_regex):
            # print('================================================================================')
            # print(start, end, match)
            # print('--------------------------------------------------------------------------------')
            # print(repr(raw_contents[start:end]))
            self.positions.append((start, end, match.group(2) or str(self.path), int(match.group(1))))

        self.contents = ''
        for start, end, match in preprocessor_result_iter(raw_contents, macro_regex):
            self.contents += ' ' * (match.end() - match.start() + 1) + raw_contents[start:end]

    def get_position(self, pos_in_stream):
        for start, end, filename, lineno in self.positions:
            if start <= pos_in_stream < end:
                return '{}:{}:'.format(
                    filename,
                    lineno + self.contents[start:pos_in_stream].count('\n'))
        raise ValueError(str(self.path) + ': Invalid Stream Position?: ' + repr(pos_in_stream))

    def parse(self, parser, visitor):
        try:
            parser.parser.parser.parser.avidly_idl_file = self
            self.raw_tree = parser.parse(self.contents, on_error=unexpected_token_handler)
            visitor.visit(self.raw_tree)
        except lark.exceptions.UnexpectedInput as e:
            sys.exit('{}\n{}\n{}'.format(self.get_position(e.pos_in_stream),
                e.get_context(self.contents), str(e)))


class Compiler:
    @staticmethod
    def get_idl_parser(annotations_only=False):
        class PassThroughLexer(lark.lexer.Lexer):
            def __init__(self, lexer_conf):
                pass

            def lex(self, data):
                for token in data:
                    yield token

        return lark.Lark.open(
            'idl.lark', rel_to=__file__, parser='lalr',
            start='annotation_appl_only' if annotations_only else 'specification',
            lexer=PassThroughLexer if annotations_only else 'standard')

    def __init__(self):
        self.parser = self.get_idl_parser()
        self.parser.parser.parser.parser.avidly_annotations_only_parser = \
            self.get_idl_parser(annotations_only=True)

    def compile(self, paths, includes=[], defines=[]):
        idl_files = [IdlFile(path) for path in paths]
        visitor = RawTreeVistior()
        for idl_file in idl_files:
            print(str(idl_file.path))
            preprocessor = Preprocessor()
            for include in includes:
                preprocessor.add_path(include)
            for define in defines:
                preprocessor.define(define.replace('=', ' ', 1))
            idl_file.load(preprocessor)
            idl_file.parse(self.parser, visitor)
