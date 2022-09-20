from pathlib import Path

from .idl import IdlParser, UnsupportedAnnotations
from .errors import ErrorsReported
from .log import error_exit
from .itl import parse_itl_files


valid_args = set((
    'includes',
    'defines',
    'dump_tokens',
    'dump_raw_tree',
    'dump_tree',
    'debug_parser',
    'unsupported_annotations',
))


def check_kwargs(kwargs):
    invalid_args = set(kwargs.keys()) - valid_args
    if invalid_args:
        raise KeyError('Invalid kwarg key(s): ' + ', '.join(invalid_args))


def add_type_file_argument_parsing(argparser, **override_defaults):
    argparser.add_argument('type_files',
        metavar='TYPE_FILE', type=Path, nargs='+',
        help='OMG IDL or OpenDDS ITL File(s)')
    argparser.add_argument('-I', '--include', dest='includes',
        type=Path, action='append', default=[],
        metavar='DIR_PATH',
        help='Add path to the preprocessor include directories.')
    argparser.add_argument('-D', '--define', dest='defines',
        action='append', default=[], metavar='NAME=VALUE',
        help='Define macro value for the preprocessor.')
    argparser.add_argument('--dump-tokens',
        action='store_true',
        help='Dump tokens before parsing them')
    argparser.add_argument('--dump-raw-tree',
        action='store_true',
        help='Dump tree right after parsing')
    argparser.add_argument('--dump-tree',
        action='store_true',
        help='Dump finalized tree')
    argparser.add_argument('--debug-parser',
        action='store_true',
        help='Trace parser matching rules')
    argparser.add_argument('--unsupported-annotations',
        choices=UnsupportedAnnotations.__members__.values(),
        default=UnsupportedAnnotations.warn_once,
        type=UnsupportedAnnotations)
    # TODO: Other debug options
    # TODO: Control Ignored Macros (pragma)
    # TODO: Control Unknown Annotations

    check_kwargs(override_defaults)
    argparser.set_defaults(**override_defaults)


def get_idl_parser(parsed_args, **override_values):
    parsed_args_dict = vars(parsed_args)
    parser_args = {k: parsed_args_dict[k] for k in valid_args}
    check_kwargs(override_values)
    parser_args.update(override_values)
    return IdlParser(**parser_args)


def type_files_to_trees(parsed_args, **override_values):
    idl_parser = get_idl_parser(parsed_args, **override_values)
    trees = []
    for type_file in parsed_args.type_files:
        if type_file.suffix == '.idl':
            try:
                trees.append(idl_parser.parse([type_file])[0])
            except ErrorsReported as e:
                error_exit(str(e))
        elif type_file.suffix == '.itl':
            trees.append(parse_itl_files([type_file]))
        else:
            error_exit("Don't know what kind of file {} is".format(repr(type_file)))
    if idl_parser.error_count:
        error_exit('{} errors found'.format(idl_parser.error_count))
    return trees
