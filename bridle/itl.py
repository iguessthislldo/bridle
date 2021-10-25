# Currently ITL is missing the following functionality:
# - No way to get annotations
# - No constants
# - Would not differentiate between octet and int8_t
# - Does not differentiate between bounded and unbounded strings

import json

from .tree import (
    Tree,
    PrimitiveNode,
    PrimitiveTraits,
    StructNode,
    FieldNode,
    EnumNode,
    ArrayNode,
    SequenceNode,
    ModuleNode,
    EnumeratorNode,
    ScopedName,
)


def itl_name_to_scoped_name(itl_name):
    return ScopedName(parts=itl_name.split(':')[1].split('/'))


def scoped_name_to_itl_name(scoped_name):
    return 'IDL:{}:1.0'.format('/'.join(scoped_name.parts))


def get_detail(details, *keys, default=None):
    try:
        value = details
        for key in keys:
            value = value[key]
        return value
    except KeyError:
        return default


def parse_int(details):
    # Get Traits of the Type
    is_unsigned = bool(get_detail(details, 'unsigned'))
    presentation = get_detail(details, 'note', 'presentation', 'type')
    is_char = presentation == 'char'
    size = get_detail(details, 'bits')
    is_constrained = bool(get_detail(details, 'constrained'))
    values = get_detail(details, 'values')
    is_enum = is_constrained and values
    is_bool = presentation == 'bool'
    is_strict_int = not any([is_enum, is_char, is_bool])

    # Account for ITL weirdness
    if is_char and size is None and get_detail(details, 'note', 'idl', 'type') == 'wchar':
        size = 16
    elif is_bool:
        size = 8

    try:
        if is_enum:
            enum_type = EnumNode(size)
            for k, v in values.items():
                # TODO: value
                enum_type.add_child(EnumeratorNode(k))
            return enum_type
        else:
            return PrimitiveNode(PrimitiveTraits(
                element_size=size,
                is_signed_int=is_strict_int and not is_unsigned,
                is_unsigned_int=is_strict_int and is_unsigned,
                is_text=is_char,
                is_bool=is_bool))
    except Exception:
        raise ValueError('Can\'t decide what this int type is: ' + repr(details))


def parse_float(details):
    size = {
        'binary32': 32,
        'binary64': 64,
        'binary128': 128,
    }.get(get_detail(details, 'model', default=None), None)
    if size is None:
        raise ValueError('Can\'t decide what this float type is: ' + repr(details))
    return PrimitiveNode(PrimitiveTraits(is_float=True, element_size=size))


def parse_fixed(details):
    raise NotImplementedError


def parse_string(details):
    return PrimitiveNode(PrimitiveTraits(
        element_size=16 if get_detail(details, 'note', 'idl', 'type') == 'wstring' else 8,
        is_text=True, is_scalar=False))


def parse_sequence(types, details):
    base_type = parse_type(types, details["type"])
    sequence_max_count = details.get("capacity", None)
    array_dimensions = details.get("size", None)
    if array_dimensions is not None:
        return ArrayNode(base_type, array_dimensions)
    else:
        return SequenceNode(base_type, sequence_max_count)


def parse_record(types, details):
    struct_type = StructNode(None)
    for field_dict in details['fields']:
        struct_type.add_child(FieldNode(
            field_dict['name'], parse_type(types, field_dict['type'])))
    return struct_type


def parse_union(types, details):
    raise NotImplementedError


def parse_alias(types, details):
    the_type = parse_type(types, details['type'])
    the_type.scoped_name = itl_name_to_scoped_name(details['name'])
    # TODO
    # if not the_type.is_topic_type:
    #     the_type.is_topic_type = bool(get_detail(details, 'note', 'is_dcps_data_type'))
    return the_type


def parse_typedef(types, details):
    kind = details['kind']
    if kind == 'int':
        return parse_int(details)
    elif kind == 'float':
        return parse_float(details)
    elif kind == 'fixed':
        return parse_fixed(details)
    elif kind == 'string':
        return parse_string(details)
    elif kind == 'sequence':
        return parse_sequence(types, details)
    elif kind == 'record':
        return parse_record(types, details)
    elif kind == 'union':
        return parse_union(types, details)
    elif kind == 'alias':
        return parse_alias(types, details)
    else:
        raise ValueError(
            'Kind "{}" is not a valid type for parse_typedef()'.format(repr(kind)))


def parse_type(types, details):
    details_type = type(details)
    if details_type is str:
        if details in types:
            return types[details]
        else:
            raise ValueError("Invalid Type: " + details)
    elif details_type is dict:
        return parse_typedef(types, details)
    else:
        raise TypeError(
            'Type "{}" is not a valid type for parse_type()'.format(details_type.__name__))


def parse_itl(types, itl):
    for itl_type in itl['types']:
        parsed_type = parse_type(types, itl_type)
        # opendds_idl produces ITL that includes types from included IDL files, so
        # just use the first definition we found.
        itl_name = scoped_name_to_itl_name(parsed_type.scoped_name)
        if itl_name not in types:
            types[itl_name] = parsed_type


def get_ast(types: dict) -> Tree:
    root_module = Tree(None)
    for type_node in types.values():
        module = root_module
        for module_name in type_node.scoped_name.parts[:-1]:
            child_module = ModuleNode(module_name)
            module.add_child(child_module)
            module = child_module
        module.add_child(type_node)
    root_module.finalize()
    return root_module


def parse_itl_files(itl_files):
    '''Read and parse a list of ITL file paths, collecting the results and
    return an assembled AST.
    '''

    types = {}
    for itl_file in itl_files:
        with itl_file.open() as f:
            parse_itl(types, json.load(f))
    return get_ast(types)
