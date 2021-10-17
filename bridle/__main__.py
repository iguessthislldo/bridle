from argparse import ArgumentParser
from pathlib import Path

from .get_parser import add_type_file_argument_parsing, get_parser
from .errors import ErrorsReported
from .log import error_exit
from .cdr.data_dumper import DataDumper


def dump_tree(args, parser, trees):
    for tree in trees:
        tree.dump()


def dump_data(args, parser, trees):
    DataDumper(trees).dump(args.data_file.read_bytes(), args.topic_type_name)


def main():
    # Parse Arguments
    argparser = ArgumentParser(description='OMG IDL Analysis Tool')

    subcmds = argparser.add_subparsers()

    dump_tree_subcmd = subcmds.add_parser('dump-tree')
    dump_tree_subcmd.set_defaults(subcmd=dump_tree)

    dump_data_subcmd = subcmds.add_parser('dump-data')
    dump_data_subcmd.set_defaults(subcmd=dump_data)
    dump_data_subcmd.add_argument('topic_type_name',
        metavar='TOPIC_TYPE_NAME',
        help='Name of the topic/base/top-level type')
    dump_data_subcmd.add_argument('data_file',
        metavar='DATA_FILE', type=Path,
        help='CDR File')

    add_type_file_argument_parsing(argparser)

    args = argparser.parse_args()
    parser = get_parser(args)
    try:
        trees = parser.parse(args.type_files)
    except ErrorsReported as e:
        error_exit(str(e))

    args.subcmd(args, parser, trees)


if __name__ == "__main__":
    main()
