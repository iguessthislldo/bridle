import sys
from enum import Enum, unique, auto
from dataclasses import dataclass
import re

from .utils import is_sequence, Location
from .errors import BridleError  # , RedefintionError

# TODO: Seperate IDL processing into sepearte file?


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
        raise BridleError('Invalid scoped IDL name: {}', repr(idl))

    def __repr__(self):
        name = '::'.join(self.parts)
        if self.absolute:
            name = '::' + name
        return name

    def __str__(self):
        return repr(self)


class Action(Enum):
    add = auto()
    ignore = auto()
    # TODO: Other actions? merge, replace?


class Node:

    def __init__(self, name=None, parent=None, loc=None):
        self.name = name
        self.scoped_name = None
        self.parent = parent
        self.loc = loc
        self.tree = None
        self.def_index = None

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

    def handle_possible_redefintion(self, new_node):
        # TODO
        # self.tree.new_error(RedefintionError(self.name, self.loc, new_node.loc))
        return Action.ignore

    def emplace_phase(self):
        '''\
        This phase does basic checks on the children and moves them into
        Node-specific structures. As part of this nodes get their full names,
        modules nodes that are the same namespace are merged and checks for
        redefintion errors happen.
        '''
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

    def add_child(self, child):
        if is_sequence(child):
            self.add_children(child)
        else:
            self.children.append(child)
            if self.tree is not None:
                child.set_tree(self.tree, self.scoped_name)

    def add_children(self, children):
        self.children.extend(children)
        if self.tree is not None:
            for child in children:
                child.set_tree(self.tree, self.scoped_name)

    def set_tree(self, tree, parent_scoped_name):
        super().set_tree(tree, parent_scoped_name)
        for child in self.children:
            child.set_tree(tree, self.scoped_name)

    def emplace_nodes(self, nodes):
        if self.children_dict is None:
            self.children_dict = {}
        for node in nodes:
            add = True
            existing = self.children_dict.get(node.name)
            if existing is not None:
                action = existing.handle_possible_redefintion(node)
                add = action == Action.add
            if add:
                self.children_dict[node.name] = node
                node.parent = self
                if isinstance(node, ContainerNode):
                    node.emplace_phase()

    def get(self, scoped_name):
        if isinstance(scoped_name, str):
            scoped_name = ScopedName.from_idl(scoped_name)
        node = self.children_dict[scoped_name.parts[0]]
        if len(scoped_name.parts) == 1:
            return node
        get_scoped_name = ScopedName(scoped_name.parts[1:], absolute=False)
        if not isinstance(node, ContainerNode):
            raise BridleError('{} can\'t have any children like {}',
                node.scoped_name, get_scoped_name)
        return node.get(get_scoped_name)

    def emplace_phase(self):
        self.emplace_nodes(self.children)

    def accept(self, visitor):
        for child in self.children.values():
            child.accept(visitor)

    def dump(self, level=0):
        super().dump(level)
        level += 1
        for child in self.children:
            child.dump(level)


class ModuleNode(ContainerNode):
    # TODO: Handle merging modules
    pass


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
            raise BridleError('Semantic errors were found')

    def _repr(self, short):
        return self.repr_template('{}', self.loc, short=short)


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

    def _repr(self, short):
        contents = self.kind.name
        if self.element_count_limit:
            contents += ' max {}'.format(self.element_count_limit)
        return self.repr_template(contents, short=short)


class FieldNode(Node):

    def __init__(self, name, type_node=None, optional=False):
        super().__init__(name)
        self.type_node = type_node
        self.optional = optional

    def _repr(self, short):
        return self.repr_template(repr(self.type_node), short=short)


class StructNode(ContainerNode):

    def __init__(self, name=None):
        super().__init__(name=name)
        self.fields = {}

    def add_field(self, name, type_node, optional):
        self.fields[name] = FieldNode(name, type_node, optional)

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


class UnionNode(ContainerNode):

    def __init__(self):
        super().__init__()
        self.disc_type = None

    def accept(self, visitor):
        visitor.visit_union(self)

    def _repr(self, short):
        return self.repr_template('{}', repr(self.disc_type), short=short)


class TypedefNode(Node):

    def __init__(self, name, base_type):
        super().__init__(name)
        self.base_type = base_type

    def accept(self, visitor):
        visitor.visit_typedef(self)

    def _repr(self, short):
        return self.repr_template('{}', repr(self.base_type), short=short)


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
