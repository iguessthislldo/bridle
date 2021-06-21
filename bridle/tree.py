from enum import Enum, unique
from dataclasses import dataclass

from .utils import is_sequence


class Node:

    def __init__(self, name=None, parent=None):
        self.name = name
        self.parent = parent
        self.raw_contents = []

    def add_raw(self, content):
        if is_sequence(content):
            self.raw_contents.extend(content)
        else:
            self.raw_contents.append(content)

    def accept(self, visitor):
        raise NotImplementedError

    def repr_name(self):
        if self.name:
            return '::' + self.name.join('::')

    def repr_template(self, fmt='', *args):
        info = ''
        name = self.repr_name()
        if name:
            info += ' ' + name
        if fmt:
            info += ': ' + fmt.format(*args)
        return '<{}{}>'.format(self.__class__.__name__, info)

    def __repr__(self):
        return self.repr_template()


class ModuleNode(Node):

    def __init__(self, name=None, parent=None):
        super().__init__(name, parent)
        self.submodules = {}
        self.types = {}

    def accept(self, visitor):
        for type_node in self.types.values():
            type_node.accept(visitor)

        for submodule in self.submodules.values():
            visitor.visit_module(submodule)

    def repr_name(self):
        return self.name if self.name else ':: (Root Module)'


@dataclass(frozen=True)
class PrimitiveTraits:
    element_size: int = None
    is_unsigned_int: bool = False
    is_signed_int: bool = False
    is_float: bool = False
    is_text: bool = False
    is_scalar: bool = True
    is_bool: bool = False
    is_raw: bool = False


class PrimitiveNode(Node):

    @unique
    class Kind(Enum):
        boolean = PrimitiveTraits(element_size=8, is_bool=True)
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

    def __init__(self, kind):
        super().__init__()
        self.kind = self.Kind(kind)
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

    def __repr__(self):
        contents = self.kind.name
        if self.element_count_limit:
            contents += ' max {}'.format(self.element_count_limit)
        return self.repr_template(contents)


class FieldNode(Node):

    def __init__(self, name, type_node=None, optional=False):
        super().__init__(name)
        self.type_node = type_node
        self.optional = optional

    def __repr__(self):
        return self.repr_template(repr(self.type_node))


class StructNode(Node):

    def __init__(self, name=None):
        super().__init__(name=name)
        self.fields = {}

    def add_field(self, name, type_node, optional):
        self.fields[name] = FieldNode(name, type_node, optional)

    def accept(self, visitor):
        visitor.visit_struct(self)


class EnumNode(Node):

    def __init__(self, size=None):
        super().__init__()
        self.size = size
        self.members = {}
        self.default_member = None

    def add_member(self, name, value):
        self.members[name] = value
        if self.default_member is None:
            self.default_member = name

    def accept(self, visitor):
        visitor.visit_enum(self)

    def __repr__(self):
        return self.repr_template('{} bits', self.size)


class ArrayNode(Node):

    def __init__(self, base_type, dimensions):
        super().__init__()
        self.base_type = base_type
        self.dimensions = dimensions

    def accept(self, visitor):
        visitor.visit_array(self)

    def __repr__(self):
        return self.repr_template(
            repr(self.base_type) + ''.join(["[{}]".format(i) for i in self.dimensions]))


class SequenceNode(Node):

    def __init__(self, base_type, max_count):
        super().__init__()
        self.base_type = base_type
        self.max_count = max_count

    def accept(self, visitor):
        visitor.visit_sequence(self)

    def __repr__(self):
        return self.repr_template(repr(self.base_type) + " {}",
            "max " + str(self.max_count) if self.max_count else "no max")


class ConstantNode(Node):

    def __init__(self, the_type):
        self.the_type = the_type
        self.value = None

    def accept(self, visitor):
        visitor.visit_constant(self)

    def __repr__(self):
        return self.repr_template('{} = {}', repr(self.the_type), repr(self.value))


class UnionNode(Node):

    def __init__(self):
        self.disc_type = None

    def accept(self, visitor):
        visitor.visit_union(self)

    def __repr__(self):
        return self.repr_template('{}', repr(self.disc_type))


class TypedefNode(Node):

    def __init__(self, name, base_type):
        super().__init__(name)
        self.base_type = base_type

    def accept(self, visitor):
        visitor.visit_typedef(self)

    def __repr__(self):
        return self.repr_template('{}', repr(self.base_type))


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
