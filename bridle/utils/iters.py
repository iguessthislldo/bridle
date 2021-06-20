class BaseIter:
    def __iter__(self):
        return self

    def __next__(self):
        value = self._next()
        return value

    def advance(self, arg=1):
        if isinstance(arg, BaseIter):
            if self.pos > arg.pos:
                raise IndexError(
                    'Can\'t advance to {}, because we\'re at {}'.format(arg.pos, self.pos))
            count = arg.pos - self.pos
        else:
            count = arg
        got = []
        for i in range(count):
            got.append(next(self))
        return got

    def done(self):
        return len(self.peek(start=self.pos)) == 0

    def peek(self, count=1, start=None):
        if start is None:
            start = self.pos
        if start < self.pos:
            raise IndexError("index {} is no in range".format(start))
        return self._peek(count, start)


class PeekIter(BaseIter):
    def __init__(self, iterable):
        self.it = iter(iterable)
        self.cache = []
        self.pos = 0

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
    def __init__(self, orig):
        self.orig = orig
        self.pos = orig.pos

    def _next(self):
        maybe = self.orig.peek(start=self.pos)
        if maybe:
            self.pos += 1
            return maybe[0]
        raise StopIteration

    def _peek(self, count, start=None):
        return self.orig.peek(count, start)
