from .idl import IdlTokenizer, IdlFile
from .idl.tokenizer import TokenKind
from pathlib import Path
from .type_files import add_type_file_argument_parsing, type_files_to_trees

class Formatter:

    def __init__(self):
        self.indent_level = 0
        self.indent_unit = '  '
        self.newline_count = 0
        self.need_newlines = 0
        self.rbrace = False
        # self.seen_newline = False
        # self.seen_semicolon = False
        self.first_on_line = True

    def newline_check(self):
        count = self.need_newlines - self.newline_count
        if count > 0:
            self._put('\n' * count)
        self.need_newlines = 0;

    def _put(self, *what):
        for c in ''.join([str(i) for i in what]):
            if c == '\n':
                self.newline_count += 1
                if self.newline_count >= 3:
                    continue
            else:
                self.newline_check()
            print(c, end='')

    def newline(self, indent_by=0):
        self._put('\n')
        if indent_by != 0:
            self.indent_level += indent_by
        self._put(self.indent_unit * self.indent_level)
        # self.seen_newline = False
        # self.seen_semicolon = False
        self.first_on_line = True

    def put(self, *what):
        self._put(*what)
        self.first_on_line = False

    def format_tokens(self, tokens):
        for token in tokens:
            if token.kind is TokenKind.lbrace:
                self.put(' ', token)
                self.newline(1)
            elif token.kind is TokenKind.rbrace:
                self.newline(-1)
                self.put(token)
                self.rbrace = True
            elif token.kind is TokenKind.semicolon:
                self.need_newlines = 2 if self.rbrace else 1
                self.put(token)
                self.rbrace = False
            elif token.kind is TokenKind.single_line_comment:
                # if self.seen_newline:
                #     self.newline()
                if not self.first_on_line:
                    self.put(' ')
                self.put(token)
                self.newline()
            elif not token.is_ws() or (token.kind is TokenKind.newline and len(token.text) > 2):
                if token.is_keyword() and token.kind.value.startline and self.newline_count == 0:
                    self.newline()
                self.put(token)
                if token.is_keyword():
                    self.put(' ')


# def format(paths=[], direct_inputs=[], effective_path=None):
#     tokenizer = IdlTokenizer()
#     idl_files = IdlFile.get_from(paths, direct_inputs, effective_path)
#     for idl_file in idl_files:
#         idl_file.load(preprocessor=None)
#         tokens = tokenizer.tokenize(idl_file.contents, idl_file, idl_file.source_key)
#         Formatter().format_tokens(tokens)

# def dump_tree(args):
#         tree.dump()


# def add_dump_tree_subcmd(subcmds):
#     subcmd = subcmds.add_parser('dump-tree', help='Describe a set of types')
#     subcmd.set_defaults(subcmd=dump_tree)
    # format(args.idl_files)

def format_subcmd(args):
    for tree in type_files_to_trees(args):
        # print(tree.loc)
        tree.dump()

def add_format_subcmd(subcmds):
    subcmd = subcmds.add_parser('format', help='Reformat IDL to a normalized style')
    subcmd.set_defaults(subcmd=format_subcmd)
    add_type_file_argument_parsing(subcmd)
    # subcmd.add_argument('idl_files',
    #     metavar='IDL_FILE', type=Path, nargs='+',
    #     help='OMG IDL File(s)')
