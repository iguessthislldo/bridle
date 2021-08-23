from pathlib import Path


class Location:
    def __init__(self, other=None, source=None, source_only=False):
        if other is None:
            self.line = 1
            self.col = 1
            self._length = 1
            self.abspos = 1
            self.source = source
        else:
            self.line = other.line
            self.col = other.col
            self._length = other._length
            self.abspos = other.abspos
            self.source = other.source
        if source_only:
            self.line = None
            self.col = None
            self._length = None
            self.abspos = None

    def advance(self, by):
        by_str = str(by)
        self.abspos += len(by_str)
        for char in by_str:
            if char == '\n':
                self.line += 1
                self.col = 1
            else:
                self.col += 1

    def set_line(self, source, line):
        self.source = source
        self.line = line
        self.col = 1

    @property
    def length(self):
        return self._length

    def set_length(self, what):
        self._length = len(str(what))
        if self._length < 1:
            # Length must be at least one
            self._length = 1

    def __gt__(self, other):
        return self.abspos > other.abspos

    def _str(self, full_path=True, col=True):
        if not full_path and isinstance(self.source, Path):
            name = self.source.name
        else:
            name = str(self.source)
        if self.line is None:
            t = (name,)
        elif col:
            t = (name, str(self.line), str(self.col))
        else:
            t = (name, str(self.line))
        return ':'.join(t)

    def __str__(self):
        return self._str()

    def short_str(self):
        return self._str(full_path=False, col=False)
