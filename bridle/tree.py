import sys
import enum
from dataclasses import dataclass
import re
import numbers
import typing

from .utils import is_sequence, Location
from .errors import (
    InternalError,
    ErrorsReported,
    RedefinitionError,
)
from .const_expr import ConstAbc

# TODO: Separate IDL processing into separate file?


class ScopedName:
    # Don't use \d because we don't want it to match non-ASCII digits
    # Keep consistent with m_identifier
    idl_re = re.compile(r'(::)?([^0-9\W]\w*(?:::[^0-9\W]\w*)*)')

    def __init__(self, parts=None, absolute=True):
        self.parts = parts
        self.absolute = absolute

    @classmethod
    def from_idl(cls, idl):
        m = cls.idl_re.fullmatch(idl)
        if m:
            return cls(m.group(2).split('::'), m.group(1) is not None)
        raise InternalError('Invalid scoped IDL name: {}', repr(idl))

    def name(self):
        return self.parts[-1] if len(self.parts) else None

    def __repr__(self):
        name = '::'.join(self.parts)
        if self.absolute:
            name = '::' + name
        return name

    def __str__(self):
        return repr(self)


@dataclass(frozen=True)
class PrimitiveTraits:
    element_size: int
    bound_element_size: typing.Optional[int] = None
    is_unsigned_int: bool = False
    is_signed_int: bool = False
    is_float: bool = False
    is_text: bool = False
    is_scalar: bool = True
    is_bool: bool = False
    is_raw: bool = False

    @property
    def can_op(self) -> bool:
        return self.is_scalar

    @property
    def is_int(self) -> bool:
        return self.is_unsigned_int or self.is_signed_int

    @property
    def is_char(self) -> bool:
        return self.is_text and self.is_scalar

    @property
    def is_string(self) -> bool:
        return self.is_text and not self.is_scalar

    @property
    def is_unsigned_int_like(self) -> bool:
        return self.is_raw or self.is_char or self.is_unsigned_int or self.is_bool

    @property
    def is_int_like(self) -> bool:
        return self.is_unsigned_int_like or self.is_signed_int

    @property
    def is_number(self) -> bool:
        return self.is_int or self.is_float

    @property
    def is_number_like(self) -> bool:
        return self.is_number or self.is_int_like

    @property
    def valid_number_like_range(self) -> typing.Optional[typing.Tuple[int, int]]:
        element_size = self.element_size
        if self.bound_element_size is not None:
            element_size = self.bound_element_size

        if self.is_unsigned_int_like:
            return (0, 2 ** element_size - 1)
        elif self.is_signed_int:
            return (-2 ** (element_size - 1), 2 ** (element_size - 1) - 1)
        elif self.is_float:
            # TODO: Floating point range needs to be checked
            return (float("-Inf"), float("Inf"))
        else:
            return None

    @property
    def min_number_like(self):
        return self.valid_number_like_range[0]

    @property
    def max_number_like(self):
        return self.valid_number_like_range[1]

    @property
    def expected_python_type(self) -> type:
        if self.is_bool:
            return bool
        elif self.is_int or self.is_raw:
            return numbers.Integral
        elif self.is_float:
            return numbers.Real
        elif self.is_text:
            return str
        return None

    @property
    def element_size_bytes(self) -> int:
        return self.element_size // 8


@enum.unique
class PrimitiveKind(enum.Enum):
    boolean = PrimitiveTraits(element_size=8, bound_element_size=1, is_bool=True)
    byte = PrimitiveTraits(element_size=8, is_raw=True)
    u8 = PrimitiveTraits(element_size=8, is_unsigned_int=True)
    i8 = PrimitiveTraits(element_size=8, is_signed_int=True)
    u16 = PrimitiveTraits(element_size=16, is_unsigned_int=True)
    i16 = PrimitiveTraits(element_size=16, is_signed_int=True)
    u32 = PrimitiveTraits(element_size=32, is_unsigned_int=True)
    i32 = PrimitiveTraits(element_size=32, is_signed_int=True)
    u64 = PrimitiveTraits(element_size=64, is_unsigned_int=True)
    i64 = PrimitiveTraits(element_size=64, is_signed_int=True)
    u128 = PrimitiveTraits(element_size=128, is_unsigned_int=True)
    i128 = PrimitiveTraits(element_size=128, is_signed_int=True)
    f32 = PrimitiveTraits(element_size=32, is_float=True)
    f64 = PrimitiveTraits(element_size=64, is_float=True)
    f128 = PrimitiveTraits(element_size=128, is_float=True)
    c8 = PrimitiveTraits(element_size=8, is_text=True)
    c16 = PrimitiveTraits(element_size=16, is_text=True)
    s8 = PrimitiveTraits(element_size=8, is_text=True, is_scalar=False)
    s16 = PrimitiveTraits(element_size=16, is_text=True, is_scalar=False)

    def number_like_value(self, value) -> int:
        if self.value.is_char:
            return ord(value)
        elif self.value.is_int_like:
            return int(value)
        elif self.value.is_float:
            return value
        else:
            raise InternalError('{} value {} is not number-like', self.name, repr(value))

    def valid_number_like_range(self, number_value) -> bool:
        r = self.value.valid_number_like_range
        if r is None:
            raise InternalError('Could not get number-like range for {}', self.name)
        return r[0] <= number_value <= r[1]

    def check_value(self, value):
        expected_python_type = self.value.expected_python_type
        if expected_python_type is None:
            raise InternalError('Could not get expected python type for {}', self.name)
        if not isinstance(value, expected_python_type):
            raise TypeError('{} must be like Python type {}, but {} is {}.'.format(
                self.name, expected_python_type.__name__, repr(value), type(value).__name__))

        if self.value.is_char and len(value) != 1:
            raise ValueError('Character str values like {} must have a length of 1: {}'.format(
                self.name, repr(value)))

        if self.value.is_number_like:
            number_value = self.number_like_value(value)
            if not self.valid_number_like_range(number_value):
                raise ValueError((
                    '{v}' + ('' if self.value.is_number else ' (number value {nv})')
                    + ' is outside valid range for {n}.').format(
                        v=repr(value), nv=number_value, n=self.name))

    def __repr__(self):
        return '<PrimitiveKind.{}>'.format(self.name)


class Action(enum.Enum):
    add = enum.auto()
    ignore = enum.auto()
    trim_new = enum.auto()
    trim_old = enum.auto()


class Sourced:

    def __init__(self, loc):
        self.loc = loc
        self.source = None
        self.source_tokens = None
        self.all_source_tokens = None

    def source_as_str(self, all=True):
        from .idl import TokenKind
        tokens = self.all_source_tokens if all else self.source_tokens
        if tokens is None:
            return None
        tokens = [str(t) for t in tokens if t.kind is not TokenKind.preprocessor_statement]
        return ''.join(tokens).strip()

class ScopedNameRef(Sourced, ScopedName):

    def __init__(self, loc, parts=None, absolute=True):
        Sourced.__init__(self, loc)
        ScopedName.__init__(self, parts, absolute)


class Node(Sourced):

    def __init__(self, name=None, parent=None, loc=None):
        Sourced.__init__(self, loc)
        self.name = name
        self._scoped_name = None
        self.parent = parent
        self.tree = None
        self.def_index = None
        self.marked_for_trim = False
        self.annotations = []

    @property
    def name(self):
        return self._name

    @name.setter
    def name(self, new):
        self._name = str(new) if new else None

    @property
    def scoped_name(self):
        return self._scoped_name

    @scoped_name.setter
    def scoped_name(self, new):
        self._name = new.name()
        self._scoped_name = new

    # Only Usable in Syntactic Phase ------------------------------------------

    def set_tree(self, tree, parent_scoped_name):
        self.tree = tree
        self.def_index = tree.get_next_def_index()
        # Note: We can use the parent_name to get scoped_name here, but we
        # can't set parent until we finalize the exact node object
        # relationships in emplace_phase.
        assert parent_scoped_name is not None
        assert self.name is not None
        self.scoped_name = ScopedName(parent_scoped_name.parts + [self.name])

    # Semantic Phases ---------------------------------------------------------

    def handle_possible_redefinition(self, new_node):
        self.tree.new_error(RedefinitionError(self.name, self.loc, new_node.loc))
        return Action.ignore

    def emplace_phase(self):
        '''\
        This phase does basic checks on the children and moves them into
        Node-specific structures. As part of this nodes get their full names,
        modules nodes that are the same namespace are merged and checks for
        redefinition errors happen.
        '''
        raise NotImplementedError(repr(type(self)))

    def trim_phase(self):
        '''\
        Trim nodes that are not needed anymore
        '''
        raise NotImplementedError(repr(type(self)))

    def resolve_type_ref(self, value):
        if isinstance(value, ScopedNameRef):
            got = self.parent.get(value)
            if not isinstance(got, TypeNode):
                raise ValueError('{} is not a type!'.format(value))
            value = got
        return value

    def resolve_const_ref(self, value):
        if isinstance(value, ScopedNameRef):
            got = self.parent.get(value)
            if not isinstance(got, ConstantNode):
                raise ValueError('{} is not a constant value!'.format(value))
            return got
        return None

    def name_resolution_phase(self):
        '''\
        Replace ScopedNameRefs with the actual object if they exist or error if
        they don't.
        See 7.5.2 for "Scoping Rules and Name Resolution"
        '''
        raise NotImplementedError(repr(type(self)))

    def eval_phase(self):
        '''\
        Evaluate any remaining unevaluating expressions.
        '''
        raise NotImplementedError(repr(type(self)))

    # Only Usable After Semantic Phases ---------------------------------------

    def accept(self, visitor):
        raise NotImplementedError

    # repr --------------------------------------------------------------------

    def repr_template(self, fmt='', *args, short=False):
        info = ''
        if self.name:
            info += ' ' + self.name
        if fmt:
            info += ': ' + fmt.format(*args)
        cls = self.__class__.__name__
        if short and cls.endswith('Node'):
            cls = cls[:-4]
        return '<{}{}>'.format(cls, info)

    def _repr(self, short):
        return self.repr_template(short=short)

    def __repr__(self):
        return self._repr(False)

    def short_repr(self):
        return self._repr(True)

    def dump(self, level=0):
        print('  ' * level, self.short_repr(), sep='')


class TypeNode(Node):

    def actual_type(self):
        return self


def typename(obj):
    return type(obj).__name__


class ContainerNode(Node):

    def __init__(self, name=None, parent=None, loc=None):
        Node.__init__(self, name, parent, loc)
        self.children = []
        self.children_dict = None
        self.trimmed = False
        self.called_eval_phase = False

    def child_must_be(self, child, *types):
        if not isinstance(child, types):
            raise TypeError('Child of {} is {}, must be one of {}'.format(
                repr(self), typename(child), ', '.join([t.__name__ for t in types])))

    def check_child(self, child):
        raise NotImplementedError

    def add_child(self, child):
        if is_sequence(child):
            raise TypeError('{} is a sequence!'.format(repr(child)))
        self.check_child(child)
        self.children.append(child)
        if self.tree is not None:
            child.set_tree(self.tree, self.scoped_name)
        if self.children_dict is not None:
            self.emplace_nodes([child])

    def add_children(self, children):
        if not is_sequence(children):
            raise TypeError('{} is NOT a sequence!'.format(repr(children)))
        for child in children:
            self.add_child(child)

    def set_tree(self, tree, parent_scoped_name):
        super().set_tree(tree, parent_scoped_name)
        for child in self.children:
            child.set_tree(tree, self.scoped_name)

    def emplace_nodes(self, nodes):
        if self.children_dict is None:
            self.children_dict = {}
        for node in nodes:
            add = True
            trim_new = False
            trim_old = False
            existing = self.children_dict.get(node.name)
            action = None
            if existing is not None:
                action = existing.handle_possible_redefinition(node)
                trim_old = action == Action.trim_old
                trim_new = action == Action.trim_new
                add = action == Action.add or trim_old
            if add:
                self.children_dict[node.name] = node
                node.parent = self
                if isinstance(node, ContainerNode):
                    node.emplace_phase()
            if trim_new:
                node.marked_for_trim = True
            if trim_old:
                existing.marked_for_trim = True

    def trim_children(self):
        if self.trimmed:
            return
        if self.children_dict is not None:
            for name, child in self.children_dict.items():
                if isinstance(child, ContainerNode):
                    child.trim_children()
                if child.marked_for_trim:
                    del self.children_dict[name]
        new_children = []
        for child in self.children:
            if isinstance(child, ContainerNode):
                child.trim_children()
            if not child.marked_for_trim:
                new_children.append(child)
        self.children = new_children
        self.trimmed = True

    def get(self, scoped_name, look_in_parent=True):
        if isinstance(scoped_name, str):
            scoped_name = ScopedName.from_idl(scoped_name)
        local = scoped_name.parts[0]
        if local not in self.children_dict:
            if look_in_parent and self.parent is not None:
                return self.parent.get(scoped_name)
            raise ValueError('{} not in {}'.format(scoped_name, self))
        node = self.children_dict[local]
        if len(scoped_name.parts) == 1:
            return node
        get_scoped_name = ScopedName(scoped_name.parts[1:], absolute=False)
        if not isinstance(node, ContainerNode):
            raise InternalError('{} can\'t have any children like {}',
                node.scoped_name, get_scoped_name)
        return node.get(get_scoped_name, False)

    def __getitem__(self, scoped_name):
        return self.get(scoped_name)

    def __len__(self):
        return len(self.children_dict)

    def child_names(self):
        return self.children_dict.keys()

    def child_nodes(self):
        return self.children_dict.values()

    def __iter__(self):
        return iter(self.children_dict.items())

    def emplace_phase(self):
        self.emplace_nodes(self.children)

    def trim_phase(self):
        self.trim_children()

    def name_resolution_phase(self):
        for child in self.children_dict.values():
            child.name_resolution_phase()

    def eval_phase(self):
        if not self.called_eval_phase:
            self.called_eval_phase = True
            for child in self.children_dict.values():
                child.eval_phase()

    def accept(self, visitor):
        for child in self.children:
            child.accept(visitor)

    def dump(self, level=0):
        super().dump(level)
        level += 1
        for child in self.children:
            child.dump(level)


class ForwardDclNode(TypeNode):

    def __init__(self, forward_dcl):
        self.forward_dcl = forward_dcl

    def handle_possible_redefinition(self, new_node):
        if isinstance(new_node, type(self)):
            if new_node.forward_dcl:
                return Action.trim_new
            elif self.forward_dcl:
                return Action.trim_old
        return super().handle_possible_redefinition(new_node)


class ModuleNode(ContainerNode):
    def handle_possible_redefinition(self, new_node):
        if isinstance(new_node, ModuleNode):
            self.add_children(new_node.children)
            return Action.trim_new
        else:
            return super.handle_possible_redefinition(new_node)

    def check_child(self, child):
        self.child_must_be(child, TypeNode, ConstantNode, ModuleNode)


class Tree(ModuleNode):

    def __init__(self, loc, raise_on_first_error=False):
        super().__init__(loc=Location(loc, source_only=True))
        self.raise_on_first_error = raise_on_first_error
        self.errors = []
        self.next_def_index = 0
        self.tree = self
        self.scoped_name = ScopedName([])

    def get_next_def_index(self):
        index = self.next_def_index
        self.next_def_index += 1
        return index

    def new_error(self, error):
        if self.raise_on_first_error:
            raise error
        self.errors.append(error)

    def report_errors(self, errors):
        for error in errors:
            print(str(error), file=sys.stderr)

    def finalize(self):
        self.emplace_phase()

        if self.errors:
            self.report_errors(self.errors)
            raise ErrorsReported('Semantic errors were found')

        self.trim_phase()
        self.name_resolution_phase()
        self.eval_phase()

    def _repr(self, short):
        return self.repr_template('{}', self.loc, short=short)


class PrimitiveNode(TypeNode):

    def __init__(self, kind):
        super().__init__()
        self.kind = PrimitiveKind(kind)
        self.element_count_limit = None

    def eval_phase(self):
        pass

    def accept(self, visitor):
        pass

    @property
    def is_int(self):
        return self.kind.value.is_unsigned_int or self.kind.value.is_signed_int

    @property
    def is_string(self):
        return self.kind.value.is_text and not self.kind.value.is_scalar

    @property
    def element_size(self):
        return self.value.element_size

    @property
    def is_unsigned_int(self):
        return self.value.is_unsigned_int

    @property
    def is_signed_int(self):
        return self.value.is_signed_int

    @property
    def is_float(self):
        return self.value.is_float

    @property
    def is_text(self):
        return self.value.is_text

    @property
    def is_scalar(self):
        return self.value.is_scalar

    @property
    def is_bool(self):
        return self.value.is_bool

    @property
    def is_raw(self):
        return self.value.is_raw

    def _repr(self, short):
        contents = self.kind.name
        if self.element_count_limit:
            contents += ' max {}'.format(self.element_count_limit)
        return self.repr_template(contents, short=short)


class FieldNode(Node):

    def __init__(self, name, type_node=None):
        super().__init__(name)
        self.type_node = type_node

    def _repr(self, short):
        return self.repr_template(repr(self.type_node), short=short)

    def name_resolution_phase(self):
        self.type_node = self.resolve_type_ref(self.type_node)

    def eval_phase(self):
        self.type_node.eval_phase()


class StructNode(ContainerNode, ForwardDclNode):

    def __init__(self, name, forward_dcl=False):
        ContainerNode.__init__(self, name)
        ForwardDclNode.__init__(self, forward_dcl)

    def check_child(self, child):
        self.child_must_be(child, FieldNode)  # TODO Check

    def accept(self, visitor):
        visitor.visit_struct(self)


class UnknownAnnotationNode(Node):

    def __init__(self, scoped_name, params):
        super().__init__(scoped_name.name())
        self.scoped_name = scoped_name
        self.params = params

    def accept(self, visitor):
        raise NotImplementedError


class EnumeratorNode(Node):

    def __init__(self, name, value=None, parent=None, loc=None):
        super().__init__(name, parent, loc)
        self.value = None

    def name_resolution_phase(self):
        # TODO
        pass

    def eval_phase(self):
        # TODO
        pass


class EnumNode(ContainerNode, TypeNode):

    def __init__(self, bit_bound=32):
        super().__init__()
        self.bit_count = bit_bound
        self.default_member = None

    def check_child(self, child):
        self.child_must_be(child, EnumeratorNode)  # TODO Check

    def accept(self, visitor):
        visitor.visit_enum(self)

    def _repr(self, short):
        return self.repr_template('{} bits', self.bit_bound, short=short)


class ArrayNode(TypeNode):

    def __init__(self, base_type, dimensions):
        super().__init__()
        self.base_type = base_type
        self.dimensions = dimensions

    def name_resolution_phase(self):
        self.base_type = self.resolve_type_ref(self.base_type)
        # TODO: dimensions

    def eval_phase(self):
        # TODO: dimensions
        pass

    def accept(self, visitor):
        visitor.visit_array(self)

    def _repr(self, short):
        return self.repr_template(
            repr(self.base_type) + ''.join(["[{}]".format(i) for i in self.dimensions]),
            short=short)


class SequenceNode(TypeNode):

    def __init__(self, base_type, max_count):
        super().__init__()
        self.base_type = base_type
        self.max_count = max_count

    def name_resolution_phase(self):
        self.base_type = self.resolve_type_ref(self.base_type)
        # TODO: max_count

    def eval_phase(self):
        # TODO: max_count
        pass

    def accept(self, visitor):
        visitor.visit_sequence(self)

    def _repr(self, short):
        return self.repr_template(repr(self.base_type) + " {}",
            "max " + str(self.max_count) if self.max_count else "no max", short=short)


class ConstantRefNode(ScopedNameRef, ConstAbc):

    def __init__(self, scoped_name_ref):
        ScopedNameRef.__init__(self,
            scoped_name_ref.loc, scoped_name_ref.parts, scoped_name_ref.absolute)
        Node.__init__(self)
        self.constant_node = None

    def uncasted_kind(self):
        if self.constant_node is not None:
            self.constant_node.uncasted_kind()

    def resolve_refs(self, callback):
        if not self.can_eval():
            self.constant_node = callback(self)
            print(self.parts, self.constant_node)

    def can_eval(self):
        if self.constant_node is not None:
            return self.constant_node.can_eval()
        return False

    def eval(self, to: PrimitiveKind):
        if self.constant_node is not None:
            return self.constant_node.eval(to)

    def __str__(self):
        return ScopedNameRef.__str__(self)

    def __repr__(self):
        return ScopedNameRef.__repr__(self)


class ConstantNode(Node, ConstAbc):

    def __init__(self, type_node):
        super().__init__()
        self.type_node = type_node
        self._value = None
        self._raw_value = None

    def uncasted_kind(self):
        if self._value is None and (isinstance(self._raw_value, ScopedName) or
                isinstance(self.type_node, ScopedName)):
            return None
        return self.type_node.actual_type().kind

    @property
    def value(self):
        return self._value

    @value.setter
    def value(self, new_value):
        self._raw_value = new_value
        if self.can_eval():
            self._value = self.eval(self.uncasted_kind())
        else:
            self._value = None

    def can_eval(self):
        if self._value is not None:
            return True
        elif self.uncasted_kind() is None:
            return False
        elif isinstance(self._raw_value, ConstAbc):
            return self._raw_value.can_eval()
        return False

    def eval(self, to: PrimitiveKind):
        if self._value is None:
            self._value = self._raw_value.eval(to)
        return self._value

    def resolve_refs(self, callback):
        return self._raw_value.resolve_refs(callback)

    def name_resolution_phase(self):
        self.type_node = self.resolve_type_ref(self.type_node)
        if self._value is None:
            self.resolve_refs(self.resolve_const_ref)

    def eval_phase(self):
        self._value = self.eval(self.uncasted_kind())

    def accept(self, visitor):
        visitor.visit_constant(self)

    def __str__(self):
        return str(self._value)

    def _repr(self, short):
        return self.repr_template(
            '{} = {}', repr(self.type_node), repr(self.value), short=short)


class UnionBranchNode(FieldNode):

    def __init__(self, name, type_node):
        super().__init__(name, type_node)
        self.cases = []
        self.is_default_branch = False


class UnionNode(ContainerNode, ForwardDclNode):

    def __init__(self, name, forward_dcl=False):
        ContainerNode.__init__(self, name)
        ForwardDclNode.__init__(self, forward_dcl)
        self.disc_type = None

    def check_child(self, child):
        self.child_must_be(child, UnionBranchNode)  # TODO Check

    def accept(self, visitor):
        visitor.visit_union(self)

    def _repr(self, short):
        return self.repr_template('{}', repr(self.disc_type), short=short)


class TypedefNode(TypeNode):

    def __init__(self, name, base_type):
        super().__init__(name)
        self.base_type = base_type

    def actual_type(self):
        return self.base_type.actual_type()

    def name_resolution_phase(self):
        self.base_type = self.resolve_type_ref(self.base_type)

    def eval_phase(self):
        self.base_type.eval_phase()

    def accept(self, visitor):
        visitor.visit_typedef(self)

    def _repr(self, short):
        return self.repr_template('{}', repr(self.base_type), short=short)


class BitsetFieldNode(Node):
    # 7.4.13.4.3.2
    # TODO: Incomplete

    def __init__(self, name, position=None):
        super().__init__(name)
        self.position = position


class BitsetNode(ContainerNode, TypeNode):
    # 7.4.13.4.3.2
    # TODO: Incomplete

    def __init__(self, name):
        super().__init__(name)

    def check_child(self, child):
        self.child_must_be(child, BitsetFieldNode)  # TODO Check


class BitmaskValueNode(Node):
    # 7.4.13.4.3.3
    # TODO: Incomplete

    def __init__(self, name, position=None):
        super().__init__(name)
        self.position = position

    def name_resolution_phase(self):
        # TODO
        pass

    def eval_phase(self):
        # TODO
        pass


class BitmaskNode(ContainerNode, TypeNode):
    # 7.4.13.4.3.3
    # TODO: Incomplete

    def __init__(self, name, bit_bound=None):
        super().__init__(name)
        self.bit_bound = bit_bound

    def check_child(self, child):
        self.child_must_be(child, BitmaskValueNode)  # TODO Check


class ParameterAttr(enum.Enum):
    In = enum.auto()
    Out = enum.auto()
    InOut = enum.auto()


class ParameterNode(Node):

    def __init__(self, name, attr, type):
        super().__init__(name)
        self.attr = attr
        self.type = type
        self.raises = []

    def _repr(self, short):
        return self.repr_template('{} {}', self.attr.name, repr(self.type), short=short)

    def name_resolution_phase(self):
        self.type = self.resolve_type_ref(self.type)
        # TODO: raises

    def eval_phase(self):
        # TODO
        pass


class OpNode(ContainerNode):

    def __init__(self, name, return_type):
        super().__init__(name)
        self.return_type = return_type

    def check_child(self, child):
        self.child_must_be(child, ParameterNode)  # TODO Check

    def name_resolution_phase(self):
        self.return_type = self.resolve_type_ref(self.return_type)
        super().name_resolution_phase()


class InterfaceNode(ContainerNode, ForwardDclNode):

    def __init__(self, name, forward_dcl=False, local=False):
        ContainerNode.__init__(self, name)
        ForwardDclNode.__init__(self, forward_dcl)
        self.local = local

    def accept(self, visitor):
        visitor.visit_interface(self)

    def check_child(self, child):
        self.child_must_be(child, OpNode)  # TODO Check


class NodeVisitor:

    def visit_root_module(self, root_module):
        root_module.accept(self)

    def visit_module(self, module):
        module.accept(self)

    def visit_struct(self, struct_type):
        raise NotImplementedError

    def visit_enum(self, enum_type):
        raise NotImplementedError

    def visit_array(self, array_type):
        raise NotImplementedError

    def visit_sequence(self, sequence_type):
        raise NotImplementedError

    def visit_constant(self, constant_type):
        raise NotImplementedError

    def visit_union(self, union_type):
        raise NotImplementedError

    def visit_typedef(self, typedef_type):
        raise NotImplementedError

    def visit_interface(self, node):
        raise NotImplementedError
