from pathlib import Path

from .idl import IdlParser


valid_args = set((
    'includes',
    'defines',
    'dump_raw_tree',
    'dump_tree',
    'debug_parser',
))


def check_kwargs(kwargs):
    invalid_args = set(kwargs.keys()) - valid_args
    if invalid_args:
        raise KeyError('Invalid kwarg key(s): ' + ', '.join(invalid_args))


def add_type_file_argument_parsing(argparser, **override_defaults):
    argparser.add_argument('type_files',
        metavar='TYPE_FILE', type=Path, nargs='+',
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
    argparser.add_argument('--debug-parser',
        action='store_true',
        help='Trace parser matching rules')
    # TODO: Other debug options
    # TODO: Control Ignored Macros (pragma)
    # TODO: Control Ignored Annotations
    # TODO: Control Unknown Annotations

    check_kwargs(override_defaults)
    argparser.set_defaults(**override_defaults)


def get_parser(parsed_args, **override_values):
    parsed_args_dict = vars(parsed_args)
    parser_args = {k: parsed_args_dict[k] for k in valid_args}
    check_kwargs(override_values)
    parser_args.update(override_values)
    return IdlParser(**parser_args)
