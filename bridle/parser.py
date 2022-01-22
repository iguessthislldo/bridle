"""\
Common framework for examining a series of elements and checking to see if they
fit a series of matching rules. These elements are characters in the case of
IdlTokenizer and tokens in the case of IdlParser.
"""

from functools import wraps
import re
import inspect

from .utils import PeekIter, ChainedIter, Location
from .errors import ParseError


class Stream:
    """\
    Stream is a wrapper for the stack of iterators and other objects that
    represent where we are in the series of elements to be processed. It also
    helps decide what failed parsing attempt was likly cause of a failure by
    keeping track of the furthest error.
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
        self.ignored_elements .append([])

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


def rule_name(rule_method):
    return rule_method.__name__[2:]


def nontrivial_rule(rule_method):
    """\
    Decorator for rule methods in Parser subclasses that sets up the rule as a
    place to check if a series of nested rules has failed.
    """
    @wraps(rule_method)
    def rule_wrapper_wrapper(self, *args, **kwargs):
        return self.rule_wrapper(
            rule_method, rule_name(rule_method), False, *args, **kwargs)
    return rule_wrapper_wrapper


class RuleContext:
    def __init__(self, parser, maybe=False, get_debug_info=None):
        self.parser = parser
        self.maybe = maybe
        self.get_debug_info = (lambda: '') if get_debug_info is None else get_debug_info

    def indent(self):
        return '| ' * self.parser.stream.stack_size()

    def __enter__(self):
        if self.parser.debug_this_parser:
            print(self.indent() + self.get_debug_info(), self.parser.stream.loc())
        self.parser.stream.push()

    def __exit__(self, exc_type, exc_value, traceback):
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
                return True
        else:
            assert False


class Rule:
    def __init__(self, name, trivial):
        self.name = name
        self.trivial = trivial
        self.initialized = False
        self.initializing = False
        self.child_rules = None
        self.child_rule_methods = None
        self.child_rule_insts = None

    def init_child_rules(self, all_rules, child_names):
        self.child_rules = {}
        self.child_rule_methods = {}
        self.child_rule_insts = {}
        for child_name in child_names:
            child = all_rules[child_name]
            self.child_rules[child_name] = child
            if isinstance(child, Rule):
                child.init(all_rules)
                self.child_rule_insts[child_name] = child
            else:
                self.child_rule_methods[child_name] = child

    def init_impl(self, all_rules):
        """\
        Use this to collect information about other rules.
        """
        raise NotImplementedError

    def init(self, all_rules):
        if self.initialized:
            return
        if self.initializing:
            raise RuntimeError("Cycle in rule " + self.name)
        initializing = True
        self.init_impl(all_rules)
        initializing = False
        self.initialized = True

    def match_impl(self, parser_inst):
        raise NotImplementedError

    def match(self, parser_inst):
        if self.trivial:
            return self.match_impl(parser_inst)
        else:
            with RuleContext(parser_inst, False, lambda: self.name):
                return self.match_impl(parser_inst)
        assert False

    def __call__(self, parser_inst):
        return self.match(parser_inst)


match_rule_re = re.compile(r'(?:m|Rule)_(\w+)')


def get_rule_maybe_wrapper(name, rule):
    @wraps(rule)
    def rule_maybe_wrapper(self, *args, **kwargs):
        return self.rule_wrapper(rule, name, True, *args, **kwargs)
    return rule_maybe_wrapper


class MetaParser(type):
    def __new__(cls, name, bases, dct):
        # Collect rules from members
        new_members = {}
        rules = {}
        new_members['rules'] = rules
        rule_classes = {}
        new_members['rule_classes'] = rule_classes
        for name, member in dct.items():
            m = match_rule_re.fullmatch(name)
            if m:
                rule_name = m.group(1)
                if inspect.isclass(member) and issubclass(member, Rule):
                    rule_classes[rule_name] = member
                else:
                    rules[rule_name] = member

        # Create half finished rules from Rule classes
        rule_instances = {}
        new_members['rule_instances'] = rule_instances
        for rule_name, rule_class in rule_classes.items():
            rule = rule_class(rule_name)
            rule_instances[rule_name] = rule
            rules[rule_name] = rule
            new_members['m_' + rule_name] = lambda parser_inst: rule.match(parser_inst)

        # Generate *_maybe rules
        maybe_rules = {}
        for rule_name, rule in rules.items():
            rule_maybe_name = rule_name + '_maybe'
            rule_maybe = get_rule_maybe_wrapper(rule_maybe_name, rule)
            maybe_rules[rule_maybe_name] = rule_maybe
            new_members['m_' + rule_maybe_name] = rule_maybe
        rules.update(maybe_rules)

        # Finish initializing the rules created from classes
        for rule_instance in rule_instances.values():
            rule_instance.init(rules)

        dct.update(new_members)
        return super().__new__(cls, name, bases, dct)


class Parser(metaclass=MetaParser):
    """\
    Base class for matching rules to a series of elements. These rules must be
    methods of the subclass that begin with "m_". Starting with "start" method,
    rule methods must be called either directly, with '_maybe' tacked on to
    indicate that the rule is optional, or by listing multiple possible rules
    using match().
    """

    def _parse(self, source, name, source_key, over_chars, debug=False, parse_error_handler=None):
        self.stream = Stream(source, name, source_key, over_chars)
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
            if callable(what):
                what = what()
            raise ParseError(self.stream.loc(),
                'Expected {}, but reached end of input', ' or '.join(what))

    def assert_end(self):
        if not self.stream.done():
            raise ParseError(self.stream.loc(),
                'Expected end of input, but got {}', repr(str(self.stream.peek())))

    def handle_accepted_ignored_elements(self, ignored_elements):
        pass

    def rule_wrapper(self, func, name, maybe, *args, pass_self=True, **kwargs):
        with RuleContext(self, maybe, lambda: '{} {} {}'.format(name, args, kwargs)):
            return func(self, *args, **kwargs) if pass_self else func(*args, **kwargs)

    def match_single_rule(self, rule, args):
        try:
            return True, self.rules[rule](self, *args)
        except ParseError:
            return False, None

    def match(self, rules):
        if isinstance(rules, str):
            rules = {rules: []}
        elif not isinstance(rules, dict):
            rules = {rule: [] for rule in rules}
        loc = Location(self.stream.loc())
        for rule, args in rules.items():
            matched, value = self.match_single_rule(rule, args)
            if matched:
                return value
        raise ParseError(loc, 'Expected ' + ' or '.join([str(r) for r in rules]))

    def match_maybe(self, rules):
        return self.rule_wrapper(self.match, 'match_maybe', True, rules, pass_self=False)
