"""\
These iterators allow us to go over a series of elements, which can be a
characters in a string or any arbitrary objects in an iterable.
"""


class BaseIter:
    def __init__(self, peek_iter, pos):
        self.peek_iter = peek_iter
        self.pos = pos

    def __iter__(self):
        return self

    def __next__(self):
        value = self._next()
        return value

    def return_values(self, values):
        return ''.join(values) if self.peek_iter.return_strings else values

    def advance(self, arg=1):
        if isinstance(arg, BaseIter):
            if self.pos > arg.pos:
                raise IndexError(
                    'Can\'t advance to {}, because we\'re at {}'.format(arg.pos, self.pos))
            count = arg.pos - self.pos
        elif isinstance(arg, str):
            count = len(arg)
        else:
            count = arg
        got = []
        for i in range(count):
            got.append(next(self))
        return self.return_values(got)

    def done(self):
        return len(self.peek(start=self.pos)) == 0

    def peek(self, count=1, start=None, offset=0):
        if start is None:
            start = self.pos
        start += offset
        if start < self.pos:
            raise IndexError("index {} is not in range".format(start))
        return self.return_values(self._peek(count, start))


class PeekIter(BaseIter):
    """\
    The PeekIter allows peeking into the iterable without loosing the current
    position as far as the PeekIter is concerned.
    """

    def __init__(self, iterable, return_strings=False):
        super().__init__(peek_iter=self, pos=0)
        self.it = iter(iterable)
        self.return_strings = return_strings
        self.cache = []

    def _next(self):
        if self.cache:
            rv = self.cache[0]
            self.cache = self.cache[1:]
        else:
            rv = next(self.it)
        self.pos += 1
        return rv

    def _peek(self, count, start):
        cache_index = start - self.pos
        need = cache_index + count
        if len(self.cache) < need:
            try:
                for i in range(need - len(self.cache)):
                    self.cache.append(next(self.it))
            except StopIteration:
                pass
        return self.cache[cache_index:need]


class ChainedIter(BaseIter):
    """\
    The ChainedIter takes a PeekIter or another ChainedIter as a parent and can
    either be discarded or passed to the parent's advance() method to update
    the parent to the child's position.
    """

    def __init__(self, parent):
        super().__init__(parent.peek_iter, parent.pos)

    def _next(self):
        maybe = self.peek_iter.peek(start=self.pos)
        if maybe:
            self.pos += 1
            return maybe[0]
        raise StopIteration

    def _peek(self, count, start=None):
        return self.peek_iter.peek(count, start)
