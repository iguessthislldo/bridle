import unittest

from bridle.tree import ScopedName
from bridle.errors import BridleError


class ScopedNameTests(unittest.TestCase):

    def test_invalid_idl_name(self):
        self.assertRaises(BridleError, ScopedName.from_idl, '')
        self.assertRaises(BridleError, ScopedName.from_idl, '%')
        self.assertRaises(BridleError, ScopedName.from_idl, '1id')
        self.assertRaises(BridleError, ScopedName.from_idl, 'a#')
        self.assertRaises(BridleError, ScopedName.from_idl, ':a')
        self.assertRaises(BridleError, ScopedName.from_idl, ':a::b')
        self.assertRaises(BridleError, ScopedName.from_idl, '::a:')
        self.assertRaises(BridleError, ScopedName.from_idl, '::a:b')
        self.assertRaises(BridleError, ScopedName.from_idl, '::a:b::c')
        self.assertRaises(BridleError, ScopedName.from_idl, 'a:b')
        self.assertRaises(BridleError, ScopedName.from_idl, '::a::1b::c')

    def valid_idl(self, name):
        for absolute in (True, False):
            s = ScopedName.from_idl(('::' if absolute else '') + name)
            self.assertEqual(s.absolute, absolute)
            self.assertListEqual(s.parts, name.split('::'))

    def test_valid_idl_name(self):
        self.valid_idl('a')
        self.valid_idl('a::b')
        self.valid_idl('a::b::c')
        self.valid_idl('a1')
        self.valid_idl('a1::b1')
        self.valid_idl('a1::b1::c1')
