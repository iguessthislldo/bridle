from bridle.get_parser import add_type_file_argument_parsing, get_parser
from bridle.errors import ErrorsReported
from bridle.log import error_exit


def dump_tree(args):
    parser = get_parser(args)
    try:
        trees = parser.parse(args.type_files)
    except ErrorsReported as e:
        error_exit(str(e))

    for tree in trees:
        tree.dump()


def add_dump_tree_subcmd(subcmds):
    subcmd = subcmds.add_parser('dump-tree', help='Describe a set of types')
    subcmd.set_defaults(subcmd=dump_tree)
    add_type_file_argument_parsing(subcmd)
