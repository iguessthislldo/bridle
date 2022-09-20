from collections.abc import Sequence

from .iters import PeekIter, ChainedIter
from .location import Location
from .configurable import Configurable


def is_sequence(obj):
    return isinstance(obj, Sequence) and not isinstance(obj, str)


def must_be_sequence(obj):
    if not is_sequence(obj):
        obj = [obj]
    return obj


__all__ = [
    'PeekIter', 'ChainedIter',
    'Location',
    'is_sequence',
    'must_be_sequence',
    'Configurable',
]
