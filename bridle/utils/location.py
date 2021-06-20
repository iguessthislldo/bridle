class Location:
    def __init__(self, other=None, filename=None):
        if other is None:
            self.line = 1
            self.col = 1
            self.abspos = 1
            self.filename = filename
        else:
            self.line = other.line
            self.col = other.col
            self.abspos = other.abspos
            self.filename = other.filename

    def advance(self, by):
        self.abspos += len(by)
        for char in by:
            if char == '\n':
                self.line += 1
                self.col = 1
            else:
                self.col += 1

    def set_line(self, filename, line):
        self.filename = filename
        self.line = line
        self.col = 1

    def __gt__(self, other):
        return self.abspos > other.abspos

    def __str__(self):
        return '{}:{}:{}'.format(self.filename, self.line, self.col)
