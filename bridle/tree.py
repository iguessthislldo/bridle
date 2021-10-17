import sys
import enum
from dataclasses import dataclass
import re
import numbers
import typing

from .utils import is_sequence, Location
from .errors import InternalError, ErrorsReported, RedefinitionError

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
    boolean = PrimitiveTraits(element_size=8, is_bool=True)
    byte = PrimitiveTraits(element_size=8, bound_element_size=1, is_raw=True)
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


class Action(enum.Enum):
    add = enum.auto()
    ignore = enum.auto()
    trim_new = enum.auto()
    trim_old = enum.auto()


class Node:

    def __init__(self, name=None, parent=None, loc=None):
        self.name = name
        self.scoped_name = None
        self.parent = parent
        self.loc = loc
        self.tree = None
        self.def_index = None
        self.marked_for_trim = False

    @property
    def name(self):
        return self._name

    @name.setter
    def name(self, new):
        self._name = str(new) if new else None

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
        raise NotImplementedError

    def trim_phase(self):
        raise NotImplementedError

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


class ContainerNode(Node):

    def __init__(self, name=None, parent=None, loc=None):
        super().__init__(name, parent, loc)
        self.children = []
        self.children_dict = None
        self.trimmed = False

    def add_child(self, child):
        if is_sequence(child):
            self.add_children(child)
        else:
            self.children.append(child)
            if self.tree is not None:
                child.set_tree(self.tree, self.scoped_name)
            if self.children_dict is not None:
                self.emplace_nodes([child])

    def add_children(self, children):
        self.children.extend(children)
        if self.tree is not None:
            for child in children:
                child.set_tree(self.tree, self.scoped_name)
        if self.children_dict is not None:
            self.emplace_nodes(children)

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

    def get(self, scoped_name):
        if isinstance(scoped_name, str):
            scoped_name = ScopedName.from_idl(scoped_name)
        node = self.children_dict[scoped_name.parts[0]]
        if len(scoped_name.parts) == 1:
            return node
        get_scoped_name = ScopedName(scoped_name.parts[1:], absolute=False)
        if not isinstance(node, ContainerNode):
            raise InternalError('{} can\'t have any children like {}',
                node.scoped_name, get_scoped_name)
        return node.get(get_scoped_name)

    def emplace_phase(self):
        self.emplace_nodes(self.children)

    def trim_phase(self):
        self.trim_children(self.children)

    def accept(self, visitor):
        for child in self.children:
            child.accept(visitor)

    def dump(self, level=0):
        super().dump(level)
        level += 1
        for child in self.children:
            child.dump(level)


class ModuleNode(ContainerNode):
    def handle_possible_redefinition(self, new_node):
        if isinstance(new_node, ModuleNode):
            self.add_children(new_node.children)
            return Action.trim_new
        else:
            return super.handle_possible_redefinition(new_node)


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

    def _repr(self, short):
        return self.repr_template('{}', self.loc, short=short)


class PrimitiveNode(Node):

    def __init__(self, kind):
        super().__init__()
        self.kind = PrimitiveKind(kind)
        self.element_count_limit = None

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


class StructNode(ContainerNode):

    def __init__(self, name):
        super().__init__(name)

    def accept(self, visitor):
        visitor.visit_struct(self)


class EnumeratorNode(Node):
    pass


class EnumNode(ContainerNode):
    # TODO: The members should be nodes themselves like StructNode

    def __init__(self, size=None):
        super().__init__()
        self.size = size
        self.members = {}
        self.default_member = None

    def accept(self, visitor):
        visitor.visit_enum(self)

    def _repr(self, short):
        return self.repr_template('{} bits', self.size, short=short)


class ArrayNode(Node):

    def __init__(self, base_type, dimensions):
        super().__init__()
        self.base_type = base_type
        self.dimensions = dimensions

    def accept(self, visitor):
        visitor.visit_array(self)

    def _repr(self, short):
        return self.repr_template(
            repr(self.base_type) + ''.join(["[{}]".format(i) for i in self.dimensions]),
            short=short)


class SequenceNode(Node):

    def __init__(self, base_type, max_count):
        super().__init__()
        self.base_type = base_type
        self.max_count = max_count

    def accept(self, visitor):
        visitor.visit_sequence(self)

    def _repr(self, short):
        return self.repr_template(repr(self.base_type) + " {}",
            "max " + str(self.max_count) if self.max_count else "no max", short=short)


class ConstantNode(Node):

    def __init__(self, primitive_node):
        super().__init__()
        self.primitive_node = primitive_node
        self.value = None

    def accept(self, visitor):
        visitor.visit_constant(self)

    def _repr(self, short):
        return self.repr_template(
            '{} = {}', repr(self.primitive_node), repr(self.value), short=short)


class UnionBranchNode(FieldNode):

    def __init__(self, name, type_node):
        super().__init__(name, type_node)
        self.cases = []
        self.is_default_branch = False


class UnionNode(ContainerNode):

    def __init__(self, name):
        super().__init__(name)
        self.disc_type = None
        self.forward_dcl = False

    def accept(self, visitor):
        visitor.visit_union(self)

    def _repr(self, short):
        return self.repr_template('{}', repr(self.disc_type), short=short)

    def handle_possible_redefinition(self, new_node):
        if isinstance(new_node, UnionNode):
            if new_node.forward_dcl:
                return Action.trim_new
            elif self.forward_dcl:
                return Action.trim_old
        return super.handle_possible_redefinition(new_node)


class TypedefNode(Node):

    def __init__(self, name, base_type):
        super().__init__(name)
        self.base_type = base_type

    def accept(self, visitor):
        visitor.visit_typedef(self)

    def _repr(self, short):
        return self.repr_template('{}', repr(self.base_type), short=short)


class BitValueNode(Node):

    def __init__(self, name, position=None):
        super().__init__(name)
        self.position = position


class BitMaskNode(ContainerNode):

    def __init__(self, name, bit_bound=None):
        super().__init__(name)
        self.bit_bound = bit_bound


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
