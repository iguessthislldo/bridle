from bridle.log import error_exit

can_run_language_server = True
try:
    from .idl_language_server import run_idl_language_server
except ImportError as e:
    if e.name == 'pygls':
        can_run_language_server = False
    else:
        raise


def add_idl_language_server_arugment_parsing(argparser, **override_defaults):
    argparser.add_argument(
        "--tcp",
        action="store_true",
        help="Use TCP server",
    )
    argparser.add_argument(
        "--ws",
        action="store_true",
        help="Use WebSocket server",
    )
    argparser.add_argument(
        "--host",
        default="127.0.0.1",
        help="Bind server to this address",
    )
    argparser.add_argument(
        "--port",
        type=int, default=2087,
        help="Bind server to this port",
    )
    argparser.add_argument(
        "--log",
        action="store_true",
        help="Log to ",
    )


def lang_server(args):
    if can_run_language_server:
        run_idl_language_server(args, 'bridle_lang_server.log')
    else:
        error_exit("pygls couldn't be imported so can't run language server")


def add_lang_server_subcmd(subcmds):
    subcmd = subcmds.add_parser('lang-server', help='Run an IDL language server'
        + ('' if can_run_language_server else ' (missing pygls)'))
    add_idl_language_server_arugment_parsing(subcmd)
    subcmd.set_defaults(subcmd=lang_server)
