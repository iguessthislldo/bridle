from pathlib import Path
import re


class Location:
    def __init__(self, other=None, source=None, source_key=None, source_only=False, line=None):
        if other is None:
            self.line = 1 if line is None else line
            self.col = 1
            self._length = 1
            self.abspos = 1
            self.source = source
            self.source_key = source_key
            if source is not None and source_key is None:
                raise ValueError('Location source_key must be set if source is set')
        else:
            self.line = other.line if line is None else line
            self.col = other.col
            self._length = other._length
            self.abspos = other.abspos
            self.source = other.source
            self.source_key = other.source_key
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

    def set_line(self, source, source_key, line):
        self.source = source
        self.source_key = source_key
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

    direct_count = 0

    @classmethod
    def make_new_source_key(cls, path=None, effective_path=None):
        if path is None:
            if effective_path is None:
                cls.direct_count += 1
                path = '__DIRECT_INPUT_{}__'.format(cls.direct_count)
                source_key = cls.direct_count
            else:
                path = Path(effective_path).resolve()
                source_key = path.resolve()
        else:
            source_key = path = Path(path)
        return path, source_key

    direct_input_re = re.compile(r'^__DIRECT_INPUT_(\d+)__$')

    @classmethod
    def get_source_key(cls, source_key):
        if isinstance(source_key, str):
            m = cls.direct_input_re.match(source_key)
            if m:
                source_key = int(m[1])
            else:
                source_key = Path(source_key).resolve()
        return source_key
