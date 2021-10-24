import sys
from pathlib import Path

import rich.console
import rich.syntax
from rich.markup import escape

errcon = rich.console.Console(stderr=True)
error_marker = " [red]ERROR:[no red]"
program_name = Path(sys.argv[0]).name


def print_location_error(error, line=None, file=sys.stderr):
    errcon.print(escape(str(error.location)), style='bold', end='')
    errcon.print(error_marker, escape(error.message_without_location))
    if line is not None:
        errcon.print(rich.syntax.Syntax(line, "omg-idl", start_line=error.location.line))
        errcon.print(' ' * (error.location.col - 1), '^', '~' * (error.location.length - 1),
            style='bold red', sep='')


def error_exit(reason, exit_status=1):
    errcon.print(escape(program_name), error_marker, " ", escape(reason), sep='')
    sys.exit(exit_status)
