import unittest

from bridle.utils.iters import PeekIter, ChainedIter


class TestIters(unittest.TestCase):

    def test_single_peek(self):
        it1 = PeekIter([1, 2, 3])
        self.assertFalse(it1.done())
        self.assertEqual(next(it1), 1)
        self.assertEqual(it1.peek(), [2])
        self.assertEqual(next(it1), 2)
        it1.advance()
        self.assertEqual(it1.peek(), [])
        self.assertTrue(it1.done())

    def test_multiple_peek(self):
        it2 = PeekIter([1, 2, 3, 4, 5])
        self.assertEqual(next(it2), 1)
        self.assertEqual(it2.peek(count=2), [2, 3])
        self.assertEqual(it2.peek(start=2, count=2), [3, 4])
        self.assertEqual(next(it2), 2)
        self.assertEqual(it2.peek(start=2, count=2), [3, 4])
        self.assertEqual(next(it2), 3)
        self.assertEqual(next(it2), 4)
        self.assertEqual(next(it2), 5)
        self.assertEqual(it2.peek(), [])
        self.assertTrue(it2.done())

    def test_chained(self):
        it3 = PeekIter([1, 2, 3, 4])
        self.assertEqual(next(it3), 1)
        it4 = ChainedIter(it3)
        it4.advance(2)
        self.assertEqual(next(it4), 4)
        self.assertTrue(it4.done())
        self.assertEqual(next(it3), 2)
        self.assertEqual(next(it3), 3)
        self.assertEqual(next(it3), 4)
        self.assertTrue(it3.done())
