from argparse import ArgumentParser

from .dump_tree import add_dump_tree_subcmd
from .cdr.data_dumper import add_dump_data_subcmd
from .lsp import add_lang_server_subcmd
from .type_files import add_type_file_argument_parsing, type_files_to_trees


def main():
    argparser = ArgumentParser(description='OMG IDL Analysis Tool')
    subcmds = argparser.add_subparsers(dest='subcommand', required=True)

    subcmd = subcmds.add_parser('dry-run', help='Just parse type files')
    subcmd.set_defaults(subcmd=type_files_to_trees)
    add_type_file_argument_parsing(subcmd)

    add_dump_tree_subcmd(subcmds)
    add_dump_data_subcmd(subcmds)
    add_lang_server_subcmd(subcmds)

    args = argparser.parse_args()
    args.subcmd(args)


if __name__ == "__main__":
    main()
