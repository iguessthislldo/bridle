import sys
from pathlib import Path

import rich.console
import rich.syntax
from rich.markup import escape

from .errors import BridleError

errcon = rich.console.Console(stderr=True)
error_marker = " [red]ERROR:[no red]"
warn_marker = " [yellow]WARNING:[no yellow]"
program_name = Path(sys.argv[0]).name


def log_location(con, marker, location, message, line):
    con.print(escape(str(location)), style='bold', end='')
    con.print(marker, escape(message))
    if line is not None:
        con.print(rich.syntax.Syntax(line, "omg-idl", start_line=location.line))
        con.print(' ' * (location.col - 1), '^', '~' * (location.length - 1),
            style='bold red', sep='')


def log_error(what, line=None):
    if isinstance(what, BridleError):
        location = what.location
        message = what.message_without_location
    else:
        location, message = what
    log_location(errcon, error_marker, location, message, line)


def log_warning(what, line=None):
    location, message = what
    log_location(errcon, warn_marker, location, message, line)


def error_exit(reason, exit_status=1):
    errcon.print(escape(program_name), error_marker, " ", escape(reason), sep='')
    sys.exit(exit_status)
