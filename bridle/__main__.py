from argparse import ArgumentParser
from pathlib import Path

from . import IdlParser


def main():
    # Parse Arguments
    argparser = ArgumentParser(description='Describe an OMG IDL file.')
    argparser.add_argument('files',
        metavar='FILE', type=Path, nargs='+',
        help='OMG IDL File(s)')
    argparser.add_argument('-I', '--include', dest='includes',
        type=Path, action='append', default=[],
        metavar='DIR_PATH',
        help='Add path to the preprocessor include directories.')
    argparser.add_argument('-D', '--define', dest='defines',
        action='append', default=[], metavar='NAME=VALUE',
        help='Define macro value for the preprocessor.')
    argparser.add_argument('--dump-raw-tree',
        action='store_true',
        help='Dump tree as parsed by Lark.')
    argparser.add_argument('--dump-tree',
        action='store_true',
        help='Dump processed tree.')
    # TODO: Control Ignored Macros (pragma)
    # TODO: Control Ignored Annotations
    # TODO: Control Unknown Annotations
    args = argparser.parse_args()

    args_dict = vars(args)
    parser = IdlParser(**{k: args_dict[k] for k in set((
        'includes', 'defines', 'dump_raw_tree', 'dump_tree'))})
    print(parser.parse(args.files))


if __name__ == "__main__":
    main()
