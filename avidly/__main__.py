from argparse import ArgumentParser
from pathlib import Path

from .compiler import Compiler


def main():
    # Parse Arguments
    argparser = ArgumentParser(description='IDL Parser')
    argparser.add_argument('idl_files',
        metavar='IDL_FILE', type=Path, nargs='+',
        help='OMG IDL File(s)')
    argparser.add_argument('-I', '--include',
        type=Path, action='append', default=[],
        metavar='DIR_PATH',
        help='Add path to the preprocessor include directories.')
    argparser.add_argument('-D', '--define',
        action='append', default=[], metavar='NAME=VALUE',
        help='Define macro value for the preprocessor.')
    # TODO: Control Ignored Macros (pragma)
    # TODO: Control Ignored Annotations
    # TODO: Control Unknown Annotations
    args = argparser.parse_args()

    compiler = Compiler()
    compiler.compile(args.idl_files, args.include, args.define)


if __name__ == "__main__":
    main()
