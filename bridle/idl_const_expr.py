import enum
import operator as pyop
from abc import ABC, abstractmethod
from collections.abc import Callable
from typing import Any, Optional
from functools import cached_property
import inspect
import string
from dataclasses import dataclass

from .errors import ConstExprError, InternalError
from .tree import PrimitiveKind


class ConstAbc(ABC):
    def uncasted_kind(self):
        return None

    @abstractmethod
    def eval(self, to: PrimitiveKind):
        pass

    @abstractmethod
    def __str__(self):
        pass

    def __repr__(self):
        return '<{}: {}>'.format(self.__class__.__name__, str(self))


class ConstValue(ConstAbc):
    def __init__(self, value: Any, kind: PrimitiveKind):
        kind.check_value(value)
        self.value = value
        self.kind = kind

    def uncasted_kind(self):
        return self.kind

    def eval(self, to: PrimitiveKind) -> Any:
        if to != self.kind:
            to.check_value(self.value)
        return self.value

    def __str__(self):
        return str(self.value)


@dataclass(frozen=True)
class OpTraits:
    fmt: str
    impl: Optional[Callable[..., Any]]
    type_impl: Optional[Callable[PrimitiveKind, ..., Any]] = None
    accepts_floats: bool = True

    @cached_property
    def impl_details(self):
        if self.impl is None:
            return (True, self.type_impl)
        return (False, self.impl)

    @cached_property
    def operand_count(self):
        subtract_one, impl = self.impl_details
        impl_count = len(inspect.getfullargspec(impl).args)
        if subtract_one:
            impl_count -= 1
        fmt_count = len(list(string.Formatter().parse(self.fmt)))
        if impl_count != fmt_count:
            InternalError('impl_count ({}) and fmt_count ({}) are different for {}',
                impl_count, fmt_count, repr(self.fmt))
        return impl_count


def divide_impl(to: PrimitiveKind, a, b) -> Any:
    return (pyop.truediv if to.value.is_float else pyop.floordiv)(a, b)


def invert_impl(to: PrimitiveKind, value) -> Any:
    if to.value.is_signed_int:
        return -(value + 1)
    return to.value.max_number_like - value


class Op(enum.Enum):
    OR = OpTraits(fmt='{} | {}', impl=pyop.or_, accepts_floats=False)
    XOR = OpTraits(fmt='{} ^ {}', impl=pyop.xor, accepts_floats=False)
    AND = OpTraits(fmt='{} & {}', impl=pyop.and_, accepts_floats=False)
    RSHIFT = OpTraits(fmt='{} >> {}', impl=pyop.rshift, accepts_floats=False)
    LSHIFT = OpTraits(fmt='{} << {}', impl=pyop.lshift, accepts_floats=False)
    ADD = OpTraits(fmt='{} + {}', impl=pyop.add)
    SUBTRACT = OpTraits(fmt='{} - {}', impl=pyop.sub)
    MULTIPLY = OpTraits(fmt='{} * {}', impl=pyop.mul)
    DIVIDE = OpTraits(fmt='{} / {}', impl=None, type_impl=divide_impl)
    MODULO = OpTraits(fmt='{} % {}', impl=pyop.mod, accepts_floats=False)
    POSITIVE = OpTraits(fmt='+{}', impl=pyop.pos)
    NEGATIVE = OpTraits(fmt='-{}', impl=pyop.neg)
    INVERT = OpTraits(fmt='~{}', impl=None, type_impl=invert_impl, accepts_floats=False)
    PRIORITIZE = OpTraits(fmt='({})', impl=lambda a: a)

    def impl(self, to: PrimitiveKind, operands) -> Callable:
        add_to, impl = self.value.impl_details
        if add_to:
            return impl(to, *operands)
        else:
            return impl(*operands)

    @cached_property
    def operand_count(self):
        return self.value.operand_count

    def check_operand(self, operand: ConstAbc):
        kind = operand.uncasted_kind()
        if kind is not None:
            if kind.value.is_float and not self.value.accepts_floats:
                raise ConstExprError(
                    '{} operation doesn\'t accept floating point values', self.name)
            if not kind.value.can_op:
                raise ConstExprError('Not possible to do operations on {}', kind.name)

    def fmt_operands(self, operands):
        return self.value.fmt.format(*[str(i) for i in operands])


class ConstExpr(ConstAbc):
    def __init__(self, op: Op, *operands):
        expected_count = op.operand_count
        if len(operands) != expected_count:
            raise InternalError('{} expects {} operands, got {}',
                op.name, expected_count, len(operands))
        self.op = op
        self.operands = operands

    def eval(self, to: PrimitiveKind):
        if not to.value.can_op:
            raise ConstExprError('Not possible to do operations to get to {}', to)
        operand_values = []
        for operand in self.operands:
            self.op.check_operand(operand)
            operand_values.append(operand.eval(to))
        try:
            value = self.op.impl(to, operand_values)
            to.check_value(value)
        except Exception as e:
            raise ConstExprError('Eval failed: ' + str(e)) from e
        return value

    def __str__(self):
        return self.op.fmt_operands(self.operands)
