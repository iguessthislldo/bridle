from .type_files import add_type_file_argument_parsing, type_files_to_trees


def dump_tree(args):
    for tree in type_files_to_trees(args):
        tree.dump()


def add_dump_tree_subcmd(subcmds):
    subcmd = subcmds.add_parser('dump-tree', help='Describe a set of types')
    subcmd.set_defaults(subcmd=dump_tree)
    add_type_file_argument_parsing(subcmd)
