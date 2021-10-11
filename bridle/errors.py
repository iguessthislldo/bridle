from .utils import is_sequence


class BridleError(Exception):
    def __init__(self, fmt, *args):
        self.message = fmt.format(*args)

    def __str__(self):
        return self.message


class InternalError(BridleError):
    def __init__(self, fmt, *args):
        super().__init__('Internal error, please report: ' + fmt, *args)


class ErrorsReported(BridleError):
    pass


class LocationError(BridleError):
    def __init__(self, location, fmt, *args):
        self.location = location
        self.message_without_location = fmt.format(*args)
        super().__init__('{}: ' + fmt, location, *args)


class ParseError(LocationError):
    def __init__(self, location, fmt, *args):
        super().__init__(location, fmt, *args)


class ExpectedError(ParseError):
    def __init__(self, location, expecting, got):
        if is_sequence(expecting):
            expecting = ' or '.join(expecting)
        super().__init__(location, 'Expected {}, but got {}', expecting, got)


class SemanticError(LocationError):
    """\
    Error that occurs after parsing if syntax is correct.
    """
    def __init__(self, location, fmt, *args):
        super().__init__(location, fmt, *args)


class RedefintionError(SemanticError):
    """\
    Happens when a node has the same name as an existing name when it
    shouldn't. This is allowed when the two nodes are modules (assuming the
    module contents don't conflict) and when one or both nodes are forward
    declarations.
    """
    def __init__(self, name, orig_loc, redef_loc):
        super().__init__(redef_loc,
            'Name {} is already being used at {}', repr(name), str(orig_loc))
