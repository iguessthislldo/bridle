import sys
from pathlib import Path

import rich.console
import rich.syntax
from rich.markup import escape
from rich.text import Text

from .errors import BridleError

errcon = rich.console.Console(stderr=True)
error_marker = " [red]ERROR:[no red]"
warn_marker = " [yellow]WARNING:[no yellow]"
program_name = Path(sys.argv[0]).name


def log_location(con, kind, color, location, message, lines):
    loc = Text(str(location) + ':')
    loc.stylize('bold')
    con.print(loc, '[{0}]{1}:[no {0}]'.format(color, kind), escape(message))
    if lines:
        col = location.col - 1
        left = col + location.length
        first = True
        for line in lines:
            con.print(rich.syntax.Syntax(
                line, 'omg-idl', start_line=location.line, background_color='default'))
            con.print(' ' * col, end='')
            underline = min(len(line) - col, left)
            if first:
                con.print('^', style='bold ' + color, end='')
                underline -= 1
                first = False
            con.print('~' * underline, style='bold ' + color, sep='')
            col = 0
            left -= len(line)


def log_error(what, lines=None, con=errcon):
    if isinstance(what, BridleError):
        location = what.location
        message = what.message_without_location
    else:
        location, message = what
    log_location(con, 'ERROR', 'red', location, message, lines)


def log_warning(what, lines=None):
    location, message = what
    log_location(errcon, 'WARNING', 'yellow', location, message, lines)


def error_exit(reason, exit_status=1):
    errcon.print(escape(program_name), error_marker, " ", escape(reason), sep='')
    sys.exit(exit_status)
