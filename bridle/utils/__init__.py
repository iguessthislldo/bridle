from collections.abc import Sequence

from .iters import PeekIter, ChainedIter
from .location import Location
from .configurable import Configurable


def is_sequence(obj):
    return isinstance(obj, Sequence) and not isinstance(obj, str)


__all__ = [
    'PeekIter', 'ChainedIter',
    'Location',
    'is_sequence',
    'Configurable',
]
