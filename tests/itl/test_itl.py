import unittest
from pathlib import Path

from bridle.itl import parse_itl_files


test_path = Path(__file__).parent


class ItlFileTests(unittest.TestCase):

    def test_basic_itl(self):
        parse_itl_files([test_path / 'basic.itl'])
