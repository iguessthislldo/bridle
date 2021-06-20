class BridleError(Exception):
    def __init__(self, fmt, *args):
        self.message = fmt.format(*args)

    def __str__(self):
        return self.message


class LocationError(BridleError):
    def __init__(self, location, fmt, *args):
        self.location = location
        super().__init__('{}: ' + fmt, location, *args)


class ParseError(LocationError):
    def __init__(self, location, fmt, *args):
        super().__init__(location, fmt, *args)
