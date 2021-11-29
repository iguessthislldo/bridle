import unittest

from bridle.utils.configurable import Configurable


class TestObject(Configurable):
    @classmethod
    def define_config_options(cls, options):
        options.add_options(dict(a=1, b=3))

    def __init__(self, **config):
        super().__init__(config)


class ParentObject(Configurable):
    @classmethod
    def define_config_options(cls, options):
        options.add_option('a', 11)
        options.add_option('b', 13)
        options.add_child_options('child_', TestObject)

    def __init__(self, **config):
        super().__init__(config)
        self.child = TestObject(config_parent=('child_', self))


class TestConfigurable(unittest.TestCase):
    def test_config(self):
        o = TestObject(b=3)
        self.assertEqual(o.config['a'], 1)
        self.assertEqual(o.config['b'], 3)
        o.config.push(dict(a=2))
        self.assertEqual(o.config['a'], 2)
        self.assertEqual(o.config['b'], 3)
        o.config.pop()
        self.assertEqual(o.config['a'], 1)
        self.assertEqual(o.config['b'], 3)

        with o.config.new_ctx(dict(b=4)):
            self.assertEqual(o.config['a'], 1)
            self.assertEqual(o.config['b'], 4)
            with o.config.new_ctx(dict(a=100)):
                self.assertEqual(o.config['a'], 100)
                self.assertEqual(o.config['b'], 4)
            self.assertEqual(o.config['a'], 1)
            self.assertEqual(o.config['b'], 4)
        self.assertEqual(o.config['a'], 1)
        self.assertEqual(o.config['b'], 3)

    def test_config_set(self):
        o = TestObject()
        self.assertEqual(o.config['a'], 1)
        self.assertEqual(o.config['b'], 3)

        o.config['a'] = 11
        self.assertEqual(o.config['a'], 11)

        with o.config.new_ctx():
            self.assertEqual(o.config['a'], 11)
            self.assertEqual(o.config['b'], 3)

            o.config['a'] = 111
            self.assertEqual(o.config['a'], 111)

            with o.config.new_ctx():
                self.assertEqual(o.config['a'], 111)
                self.assertEqual(o.config['b'], 3)

                o.config['b'] = 33
                self.assertEqual(o.config['b'], 33)

            self.assertEqual(o.config['a'], 111)
            self.assertEqual(o.config['b'], 3)

        self.assertEqual(o.config['a'], 11)
        self.assertEqual(o.config['b'], 3)

    def test_config_no_key(self):
        with self.assertRaises(KeyError):
            TestObject(fake_is_not_real=2)

        o = TestObject(b=2)
        with self.assertRaises(KeyError):
            o.config.push(dict(fake_is_not_real=2))

    def test_parent_child(self):
        o = ParentObject(b=14, child_b=15)
        self.assertEqual(o.config['a'], 11)
        self.assertEqual(o.config['b'], 14)
        self.assertEqual(o.config['child_a'], 1)
        self.assertEqual(o.config['child_b'], 15)
        self.assertEqual(o.child.config['a'], 1)
        self.assertEqual(o.child.config['b'], 15)
