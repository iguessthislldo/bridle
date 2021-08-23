"""\
Common framework for examining a series of elements and checking to see if they
fit a series of matching rules. These elements are characters in the case of
IdlTokenizer and tokens in the case of IdlParser.
"""

from .utils import PeekIter, ChainedIter, Location
from .errors import ParseError


class Stream:
    """\
    Stream is a wrapper for the stack of iterators and other objects that
    represent where we are in the series of elements to be processed. It also
    helps decide what failed parsing attempt was likly cause of a failure by
    keeping track of the furthest error.
    """

    def __init__(self, source, name, over_strings):
        self.over_strings = over_strings
        self.iters = [PeekIter(source, return_strings=over_strings)]
        self.locs = [Location(source=name)]
        self.furthest_errors = [None]
        self.furthest_error_locs = [Location(source=name)]
        self.in_annotation = False

    def __iter__(self):
        return self

    def __next__(self):
        rv = self.peek()
        if rv:
            self.advance()
            return rv
        raise StopIteration

    def done(self):
        return self.iters[-1].done()

    def peek(self, count=1, offset=0):
        return self.iters[-1].peek(count, offset=offset)

    def loc(self, value=None):
        if value is not None:
            self.locs[-1] = value
            return value
        return self.locs[-1]

    def advance(self, arg=1, loc=None):
        by = self.iters[-1].advance(arg)
        if loc is None:
            if self.over_strings:
                self.loc().advance(by)
            else:
                for token in by:
                    self.loc().advance(token)
        else:
            self.locs[-1] = Location(loc)

    def push(self):
        self.iters.append(ChainedIter(self.iters[-1]))
        self.locs.append(Location(self.loc()))
        self.furthest_errors.append(self.furthest_errors[-1])
        self.furthest_error_locs.append(Location(self.furthest_error_locs[-1]))

    def pop(self):
        return (
            self.iters.pop(),
            self.locs.pop(),
            self.furthest_errors.pop(),
            self.furthest_error_locs.pop(),
        )

    def accept(self):
        it, loc, error, error_loc = self.pop()
        self.advance(it, loc)
        return it, loc, error, error_loc

    def check_furthest_error_candidate(self, error):
        if error.location > self.furthest_error_locs[-1]:
            self.furthest_errors[-1] = error
            self.furthest_error_locs[-1] = error.location

    def reject(self, this_error):
        it, loc, popped_error, error_loc = self.pop()
        self.check_furthest_error_candidate(this_error)
        if popped_error is not None:
            self.check_furthest_error_candidate(popped_error)
        return it, loc, popped_error, error_loc

    def furthest_error(self, root_error):
        if self.furthest_error_locs[0] > root_error.location and \
                self.furthest_errors[0] is not None:
            return self.furthest_errors[0]
        return None


def nontrivial_rule(method):
    """\
    Decorator for rule methods in Parser subclasses that sets up the rule as a
    place to check if a series of nested rules has failed.
    """
    return lambda self, *args, **kwargs: self.rule_wrapper(
        method, method.__name__[2:], False, *args, **kwargs)


class Parser:
    """\
    Base class for matching rules to a series of elements. These rules must be
    methods of the subclass that begin with "m_". Starting with "start" method,
    rule methods must be called either directly, with '_maybe' tacked on to
    indicate that the rule is optional, or by listing multiple possible rules
    using match().
    """

    def _parse(self, source, name, over_chars, debug=False, parse_error_handler=None):
        self.stream = Stream(source, name, over_chars)
        self.debug_this_parser = debug
        furthest_error = None
        try:
            rv = self.start()
        except ParseError as e:
            furthest_error = self.stream.furthest_error(e)
            if furthest_error is None:
                if parse_error_handler is None:
                    raise
                else:
                    parse_error_handler(self, e)
        if furthest_error is not None:
            if parse_error_handler is None:
                raise furthest_error
            else:
                parse_error_handler(self, furthest_error)
        self.assert_end()
        return rv

    def assert_not_end(self, what):
        if self.stream.done():
            raise ParseError(self.stream.loc(),
                'Expected {}, but reached end of input', ' or '.join(what))

    def assert_end(self):
        if not self.stream.done():
            raise ParseError(self.stream.loc(),
                'Expected end of input, but got {}', repr(str(self.stream.peek())))

    def rule_wrapper(self, func, name, maybe, *args, **kwargs):
        if self.debug_this_parser:
            print('| ' * (len(self.stream.iters) - 1) + name, args, kwargs, self.stream.loc())
        self.stream.push()
        try:
            rv = func(*args, **kwargs) if maybe else func(self, *args, **kwargs)
            it, loc, error, error_loc = self.stream.accept()
            if self.debug_this_parser:
                print('| ' * (len(self.stream.iters) - 1) + 'ACCEPT', loc)
            return rv
        except ParseError as e:
            self.stream.reject(e)
            if self.debug_this_parser:
                print('| ' * (len(self.stream.iters) - 1) + 'REJECT:',
                    e, self.stream.furthest_error_locs[-1])
            if maybe:
                return None
            raise

    def __getattr__(self, name):
        maybe = name.endswith('_maybe')
        if maybe:
            name = name[:-6]
        obj = object.__getattribute__(self, name)
        if maybe:
            return lambda *args, **kwargs: self.rule_wrapper(obj, name, True, *args, **kwargs)
        return obj

    def match(self, rules):
        if isinstance(rules, str):
            rules = {rules: []}
        elif not isinstance(rules, dict):
            rules = {rule: [] for rule in rules}
        loc = Location(self.stream.loc())
        for rule, args in rules.items():
            try:
                return getattr(self, 'm_' + rule)(*args)
            except ParseError:
                pass
        raise ParseError(loc, 'Expected ' + ' or '.join(rules))
