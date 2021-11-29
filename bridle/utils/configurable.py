class ConfigOption:
    def __init__(self, name, default_value):
        self.name = name
        self.default_value = default_value


class ConfigOptions:
    def __init__(self, for_cls):
        self._for_cls = for_cls
        self._options = {}
        self._prefixes = {}

    def _add_option(self, key, config_option):
        if key in self._options:
            raise ValueError(f'{repr(key)} already in ConfigOptions for {self.for_cls.__name__}')
        self._options[key] = config_option

    def add_option(self, name, default_value):
        self._add_option(name, ConfigOption(name, default_value))

    def add_options(self, options):
        for name, default_value in options.items():
            self.add_option(name, default_value)

    def add_child_options(self, prefix, child_class):
        if prefix in self._prefixes:
            raise ValueError(
                f'Prefix {repr(prefix)} already in ConfigOptions for {self.for_cls.__name__}')
        self._prefixes[prefix] = child_class
        for name, option in child_class.get_config_options().items():
            self._add_option(prefix + name, option)

    def items(self):
        return self._options.items()

    def __getitem__(self, key):
        return self._options[key]

    def __contains__(self, key):
        return key in self._options


class ConfigValue:
    def __init__(self, name, option):
        self.name = name
        self.option = option
        self._value = option.default_value
        self.is_default = True

    def set(self, value):
        self._value = value
        self.is_default = False

    def get(self):
        return self._value

    def __repr__(self):
        return f'<{self.name}: {repr(self._value)}>'


class ConfigContextManager:
    def __init__(self, config, override, set_defaults=False):
        self.config = config
        self.override = override
        self.set_defaults = set_defaults

    def __enter__(self):
        self.config.push(self.override, self.set_defaults)

    def __exit__(self, exc_type, exc_value, exc_tb):
        self.config.pop()


class Config:
    def __init__(self, options, initial_config):
        self._options = options
        if 'config_parent' in initial_config:
            self._prefix, configurable_parent = initial_config['config_parent']
            self._parent = configurable_parent.config
            del initial_config['config_parent']
        else:
            self._prefix = None
            self._parent = None
        if (self._prefix is None) != (self._parent is None):
            raise ValueError('prefix and parent must both be set to None or not')
        self._stack = []
        self.push(initial_config, set_defaults=self._parent is None)

    def _get_parent_key(self, key):
        if self._prefix is not None:
            key = self._prefix + key
        return key

    def push(self, override={}, set_defaults=False):
        values = {}
        if set_defaults:
            for name, option in self._options.items():
                values[name] = ConfigValue(name, option)
        for key, value in override.items():
            if key in self._options:
                values[key] = ConfigValue(key, self._options[key])
                values[key].set(value)
            else:
                raise KeyError(f'Invalid config key {repr(key)}')
        self._stack.append(values)

    def pop(self):
        self._stack.pop()

    def new_ctx(self, override={}, set_defaults=False):
        return ConfigContextManager(self, override, set_defaults)

    def __getitem__(self, key):
        if key in self._options:
            for d in reversed(self._stack):
                if key in d:
                    return d[key].get()
        if self._parent is not None:
            return self._parent[self._get_parent_key(key)]
        raise KeyError(f'Invalid config key {repr(key)}')

    def __setitem__(self, key, value):
        if key in self._options:
            cv = ConfigValue(key, self._options[key])
            cv.set(value)
            self._stack[-1][key] = cv
        else:
            raise KeyError(f'Invalid config key {repr(key)}')

    def __contains__(self, key):
        return key in self._options


class Configurable:
    _config_options = None

    @classmethod
    def define_config_options(cls, options):
        raise NotImplementedError

    @classmethod
    def get_config_options(cls):
        if cls._config_options is None:
            cls._config_options = ConfigOptions(cls)
            cls.define_config_options(cls._config_options)
        return cls._config_options

    def __init__(self, initial_config={}):
        self.config = Config(self.get_config_options(), initial_config)
