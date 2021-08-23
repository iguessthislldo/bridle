import sys

import rich.console
import rich.syntax
from rich.markup import escape

errcon = rich.console.Console(stderr=True)


def print_location_error(error, line, file=sys.stderr):
    syntax = rich.syntax.Syntax(line, "omg-idl", start_line=error.location.line)
    errcon.print(escape(str(error.location)), style='bold', end='')
    errcon.print(" [red]ERROR:[no red]", escape(error.message_without_location))
    errcon.print(syntax)
    errcon.print(' ' * (error.location.col - 1), '^', '~' * (error.location.length - 1),
        style='bold red', sep='')
