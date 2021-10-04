import enum
import operator as pyop

from .errors import InternalError

# TODO: Check to see what/if the spec says about how this should work. If
# needed use PrimitiveKind to keep track type specific results.


class OpKind(enum.Enum):
    OR = (2, '{} | {}', pyop.or_)
    XOR = (2, '{} ^ {}', pyop.xor)
    AND = (2, '{} & {}', pyop.and_)
    RSHIFT = (2, '{} >> {}', pyop.rshift)
    LSHIFT = (2, '{} << {}', pyop.lshift)
    ADD = (2, '{} + {}', pyop.add)
    SUBTRACT = (2, '{} - {}', pyop.sub)
    MULTIPLY = (2, '{} * {}', pyop.mul)
    DIVIDE = (2, '{} / {}', pyop.truediv)
    MODULO = (2, '{} % {}', pyop.mod)
    POSITIVE = (1, '+{}', pyop.pos)
    NEGATIVE = (1, '-{}', pyop.neg)
    INVERT = (1, '~{}', pyop.invert)
    PRIORITIZE = (1, '({})', lambda a: a)


class Op:
    def __init__(self, kind, *operands):
        expected_count = kind.value[0]
        if len(operands) != expected_count:
            raise InternalError('{} Op expects {} operands, got {}',
                kind.name, expected_count, len(operands))
        self.kind = kind
        self.operands = operands
        self.value = None

    def can_eval(self):
        if self.value is not None:
            return True
        return all([i.can_eval() for i in self.operands])

    def eval(self):
        if self.value is None:
            self.value = self.kind.value[2](*[i.eval() for i in self.operands])
        return self.value

    def __repr__(self):
        return '<{}: {}>'.format(self.__class__.__name__, str(self))

    def __str__(self):
        return self.kind.value[1].format(*[str(i) for i in self.operands])

# TODO: New ConstExpr that wraps the Op tree.
