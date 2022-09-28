"""\
Common framework for examining a series of elements and checking to see if they
fit a series of matching rules. These elements are characters in the case of
IdlTokenizer and tokens in the case of IdlParser.
"""

from functools import wraps
import re
import inspect

from .utils import PeekIter, ChainedIter, Location, is_sequence
from .errors import ParseError


class Stream:
    """\
    Stream is a wrapper for the stack of iterators and other objects that
    represent where we are in the series of elements to be processed. It also
    helps decide what failed parsing attempt was the likely cause of a failure
    by keeping track of the furthest error.
    """

    def __init__(self, source, name, source_key, over_strings):
        self.over_strings = over_strings
        self.iters = [PeekIter(source, return_strings=over_strings)]
        self.locs = [Location(source=name, source_key=source_key)]
        self.furthest_errors = [None]
        self.furthest_error_locs = [Location(source=name, source_key=source_key)]
        self.ignored_elements = [[]]

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

    def loc(self):
        return Location(self.locs[-1])

    def override_loc(self, new_loc):
        self.locs[-1] = Location(new_loc)

    def advance(self, arg=1, loc=None):
        by = self.iters[-1].advance(arg)
        if loc is None:
            if self.over_strings:
                self.locs[-1].advance(by)
            else:
                for token in by:
                    self.locs[-1].advance(token)
        else:
            self.locs[-1] = Location(loc)

    def push(self):
        self.iters.append(ChainedIter(self.iters[-1]))
        self.locs.append(self.loc())
        self.furthest_errors.append(self.furthest_errors[-1])
        self.furthest_error_locs.append(Location(self.furthest_error_locs[-1]))
        self.ignored_elements.append([])

    def pop(self):
        return (
            self.iters.pop(),
            self.locs.pop(),
            self.furthest_errors.pop(),
            self.furthest_error_locs.pop(),
            self.ignored_elements.pop(),
        )

    def accept(self):
        it, loc, error, error_loc, ignored_elements = self.pop()
        self.advance(it, loc)
        return it, loc, error, error_loc, ignored_elements

    def check_furthest_error_candidate(self, error):
        if error.location > self.furthest_error_locs[-1]:
            self.furthest_errors[-1] = error
            self.furthest_error_locs[-1] = error.location

    def reject(self, this_error):
        it, loc, popped_error, error_loc, ignored_elements = self.pop()
        self.check_furthest_error_candidate(this_error)
        if popped_error is not None:
            self.check_furthest_error_candidate(popped_error)
        return it, loc, popped_error, error_loc, ignored_elements

    def furthest_error(self, root_error):
        if self.furthest_error_locs[0] > root_error.location and \
                self.furthest_errors[0] is not None:
            return self.furthest_errors[0]
        return None

    def stack_size(self):
        return len(self.iters) - 1

    def push_ignored_element(self, element):
        self.ignored_elements[-1].append(element)

    def get_ignored_elements(self):
        return self.ignored_elements[-1]


class RuleContext:
    def __init__(self, parser, debug_info=None, trivial=None, maybe=False):
        if trivial is None:
            trivial = True
        if trivial:
            if maybe:
                raise ValueError('Maybe requires non-trivial')
        elif debug_info is None:
            raise ValueError('Non-trivial requires debug_info')
        self.parser = parser
        self.debug_info = debug_info
        self.trivial = trivial
        self.maybe = maybe

    def indent(self):
        return '| ' * self.parser.stream.stack_size()

    def __enter__(self):
        if self.trivial:
            return
        if self.parser.debug_this_parser:
            if callable(self.debug_info):
                debug_info = self.debug_info()
            else:
                debug_info = self.debug_info
            print(self.indent() + debug_info, self.parser.stream.loc())
        self.parser.stream.push()

    def __exit__(self, exc_type, exc_value, traceback):
        if self.trivial:
            return
        if exc_type is None:
            it, loc, error, error_loc, ignored_elements = self.parser.stream.accept()
            self.parser.handle_accepted_ignored_elements(ignored_elements)
            if self.parser.debug_this_parser:
                print(self.indent() + 'ACCEPT', loc)
        elif isinstance(exc_value, ParseError):
            self.parser.stream.reject(exc_value)
            if self.parser.debug_this_parser:
                print(self.indent() + 'REJECT:',
                    exc_value, self.parser.stream.furthest_error_locs[-1])
            if self.maybe:
                return True  # Suppress exception


def wrap_rule(rule, name, default_maybe=False):
    if isinstance(rule, Rule):
        @wraps(rule)
        def rule_wrapper(*args, maybe=default_maybe, **kwargs):
            return rule(*args, maybe=maybe, **kwargs)
        return rule_wrapper

    else:
        @wraps(rule)
        def rule_wrapper(*args, maybe=default_maybe, **kwargs):
            if inspect.ismethod(rule):
                parser = rule.__self__
            else:
                parser = args[0]
            with RuleContext(parser, trivial=False, maybe=maybe,
                    debug_info=lambda: '{} {} {}'.format(name, args, kwargs)):
                return rule(*args, **kwargs)
        return rule_wrapper


def rule_name(rule_method):
    return rule_method.__name__[2:]


def nontrivial_rule(rule):
    """\
    Decorator for rule methods in Parser subclasses that sets up the rule as a
    place to check if a series of nested rules has failed.
    """
    return wrap_rule(rule, rule_name(rule))


class Rule:
    def __init__(self, parser_inst, name, trivial=None):
        self.parser_inst = parser_inst
        self.name = name
        if trivial is None:
            raise ValueError('trivial must be specified!')
        self.trivial = trivial
        self.initialized = False
        self.initializing = False
        self.child_rules = None
        self.child_rule_methods = None
        self.child_rule_insts = None

    def init_child_rules(self, child_names):
        self.child_rules = {}
        self.child_rule_methods = {}
        self.child_rule_insts = {}
        for child_name in child_names:
            child = self.parser_inst.all_rules[child_name]
            self.child_rules[child_name] = child
            if isinstance(child, Rule):
                child.init()
                self.child_rule_insts[child_name] = child
            else:
                self.child_rule_methods[child_name] = child

    def init_impl(self):
        """\
        Use this to collect information about other rules.
        """
        raise NotImplementedError

    def init(self):
        if self.initialized:
            return
        if self.initializing:
            raise RuntimeError("Cycle in rule " + self.name)
        self.initializing = True
        self.init_impl()
        self.initializing = False
        self.initialized = True

    def match(self, *args, **kwargs):
        raise NotImplementedError

    def get_debug_info(self, *args, **kwargs):
        return '{} {} {}'.format(self.name, args, kwargs)

    def __call__(self, *args, maybe=False, **kwargs):
        with RuleContext(self.parser_inst, self.get_debug_info, trivial=self.trivial, maybe=maybe):
            return self.match(*args, **kwargs)
        assert False, "match should've returned a result or thrown an exception"


class_based_rule_name_re = re.compile(r'Rule_(\w+)')
method_based_rule_name_re = re.compile(r'm_(\w+)')


def create_method(obj, function):
    method = function.__get__(obj, obj.__class__)
    return method


class Parser:
    """\
    Base class for matching rules to a series of elements. These rules must be
    methods of the subclass that begin with "m_". Starting with "start" method,
    rule methods must be called either directly, with '_maybe' tacked on to
    indicate that the rule is optional, or by listing multiple possible rules
    using match().
    """

    def __init__(self):
        # Collect all the rules defined as m_* methods or Rule subclasses
        self.all_rules = {}
        self.class_based_rules = {}
        self.method_based_rules = {}
        for member_name, member in self.__class__.__dict__.items():
            if inspect.isclass(member) and issubclass(member, Rule):
                m = class_based_rule_name_re.fullmatch(member_name)
                if m:
                    rule_name = m.group(1)
                    inst = member(self, rule_name)
                    self.class_based_rules[rule_name] = inst
                    self.all_rules[rule_name] = inst
                    setattr(self, 'm_' + rule_name, inst)
            elif callable(member):
                m = method_based_rule_name_re.fullmatch(member_name)
                if m:
                    rule_name = m.group(1)
                    # rules have to be bound methods, and these are unbound
                    # functions, so we have to bind them.
                    method = create_method(self, member)
                    self.method_based_rules[rule_name] = method
                    self.all_rules[rule_name] = method

        # Generate *_maybe rules
        for rule_name, rule in self.all_rules.items():
            new_name = rule_name + '_maybe'
            setattr(self, 'm_' + new_name, wrap_rule(rule, new_name, default_maybe=True))

        # Finish initializing the rules created from classes
        for rule_instance in self.class_based_rules.values():
            rule_instance.init()

    def _parse(self, source, name, source_key, over_chars,
            start=None, debug=False, parse_error_handler=None):
        self.stream = Stream(source, name, source_key, over_chars)
        self.debug_this_parser = debug
        furthest_error = None
        try:
            rv = self.start() if start is None else start()
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
            if callable(what):
                what = what()
            if is_sequence(what):
                what = ' or '.join(what)
            raise ParseError(self.stream.loc(), 'Expected {}, but reached end of input', what)

    def assert_end(self):
        if not self.stream.done():
            raise ParseError(self.stream.loc(),
                'Expected end of input, but got {}', repr(str(self.stream.peek())))

    def handle_accepted_ignored_elements(self, ignored_elements):
        pass

    def match_single_rule(self, rule, args):
        try:
            return True, self.all_rules[rule](*args)
        except ParseError:
            return False, None

    def match(self, rules):
        if isinstance(rules, str):
            rules = {rules: []}
        elif not isinstance(rules, dict):
            rules = {rule: [] for rule in rules}
        loc = self.stream.loc()
        for rule, args in rules.items():
            matched, value = self.match_single_rule(rule, args)
            if matched:
                return value
        raise ParseError(loc, 'Expected ' + ' or '.join([str(r) for r in rules]))

    def match_maybe(self, rules):
        with RuleContext(self, trivial=False, maybe=True,
                debug_info=lambda: 'match_maybe ({})'.format(repr(rules))):
            return self.match(rules)
