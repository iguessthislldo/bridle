import sys
import enum

from bridle import tree


class Endian(enum.Enum):
    native = sys.byteorder
    little = 'little'
    big = 'big'


def align(value, by):
    return (value + by - 1) & ~(by - 1)


def pad(value, by):
    return -value & (by - 1)


class DataKind(enum.Enum):
    user_data = enum.auto()
    control_size = enum.auto()
    padding = enum.auto()


class SerializerStatus(enum.Enum):
    ok = enum.auto()
    warning = enum.auto()
    error = enum.auto()

    def __str__(self):
        if self.value != self.ok:
            return self.name.upper() + ': '
        return ''


class SerializerDetails:
    def __init__(self, kind, what, status=SerializerStatus.ok, message=None, raw_data=None):
        self.kind = kind
        self.what = what
        self.status = status
        self.message = message
        self.raw_data = raw_data

    def __str__(self):
        return self.what

    def get_message(self):
        if self.message:
            return '({}{})'.format(str(self.status), self.message)
        return None

    def __repr__(self):
        return '{}: {}'.format(repr(str(self)), repr(self.raw_data))


class Serializer:
    padding_byte = b'\0'

    def __init__(self, buffer, endian=Endian.native, save_details=False):
        self.buffer = buffer
        self.endian = endian
        self.save_details = save_details
        self.reset()

    def reset(self):
        self.pos = 0
        self.details = []

    def take_details(self):
        rv = self.details
        self.details = []
        return rv

    @staticmethod
    def get_string_info(kind):
        return ((True, 'utf-8'), (False, 'utf-16'))[kind.value.element_size_bytes - 1]

    def write(self, data):
        new_pos = self.pos + len(data)
        self.buffer[self.pos:new_pos] = data
        self.pos = new_pos

    def write_align(self, by):
        self.write(self.padding_byte * pad(self.pos, by))

    def write_int(self, value, size, signed):
        self.write_align(size)
        self.write(value.to_bytes(size, self.endian.value, signed=signed))

    def write_string(self, value, include_nul, *encode_args, **encode_kwargs):
        as_bytes = value.encode(*encode_args, **encode_kwargs)
        self.write_u32(len(as_bytes) + (1 if include_nul else 0))
        self.write(as_bytes)
        if include_nul:
            self.write_u8(0)

    def write_primitive_kind(self, value, kind):
        if kind.value.is_int or kind.value.is_bool:
            self.write_int(value, kind.value.element_size_bytes, kind.value.is_signed_int)
        elif kind.value.is_string:
            self.write_string(value, *self.get_string_info(kind))
        else:
            assert False

    def write_boolean(self, value):
        return self.write_primitive_kind(value, tree.PrimitiveKind.boolean)

    def write_u8(self, value):
        return self.write_primitive_kind(value, tree.PrimitiveKind.u8)

    def write_u32(self, value):
        return self.write_primitive_kind(value, tree.PrimitiveKind.u32)

    def write_s8(self, value):
        return self.write_primitive_kind(value, tree.PrimitiveKind.s8)

    def read(self, size):
        new_pos = self.pos + size
        rv = self.buffer[self.pos:new_pos]
        self.pos = new_pos
        return rv

    def read_align(self, by):
        padding_size = pad(self.pos, by)
        if padding_size == 0:
            return
        padding = self.read(padding_size)
        if self.save_details:
            status = SerializerStatus.ok
            message = None
            if padding.count(self.padding_byte) != len(padding):
                status = SerializerStatus.warning
                message = 'not all padding bytes are 0 bytes'
            self.details.append(SerializerDetails(
                DataKind.padding, 'padding', status, message, raw_data=padding))

    def read_int(self, size, signed, details=None):
        self.read_align(size)
        raw_data = self.read(size)
        rv = int.from_bytes(raw_data, self.endian.value, signed=signed)
        if details and self.save_details:
            details.raw_data = raw_data
            self.details.append(details)
        return rv

    def read_int_by_kind(self, kind, details=None):
        if self.save_details and details is None:
            details = SerializerDetails(DataKind.user_data, kind.name + ' value')
        rv = self.read_int(
            kind.value.element_size_bytes, kind.value.is_signed_int, details=details)
        if kind.value.is_bool:
            if self.save_details and details is not None and rv not in (1, 0):
                details.status = SerializerStatus.warning
                details.message = 'boolean is a value other than 1 or 0'
            rv = bool(rv)
        return rv

    def read_boolean(self, details=None):
        return self.read_int_by_kind(tree.PrimitiveKind.boolean, details)

    def read_u8(self, details=None):
        return self.read_int_by_kind(tree.PrimitiveKind.u8, details)

    def read_u16(self, details=None):
        return self.read_int_by_kind(tree.PrimitiveKind.u16, details)

    def read_u32(self, details=None):
        return self.read_int_by_kind(tree.PrimitiveKind.u32, details)

    def read_u64(self, details=None):
        return self.read_int_by_kind(tree.PrimitiveKind.u64, details)

    def read_string(self, include_nul, *decode_args, **decode_kwargs):
        raw_size_details = None
        if self.save_details:
            raw_size_details = SerializerDetails(DataKind.control_size, 'string size')
        raw_size = self.read_u32(raw_size_details)
        raw_data = self.read(raw_size)
        if self.save_details:
            status = SerializerStatus.ok
            message = None
            if raw_size and include_nul and raw_data[-1] != 0:
                status = SerializerStatus.warning
                message = 'string is not terminated with nul'
                include_nul = False
            self.details.append(SerializerDetails(
                DataKind.user_data, 'string data', status, message, raw_data=raw_data))
        return raw_data[:raw_size - int(include_nul)].decode(*decode_args, **decode_kwargs)

    def read_s8(self):
        return self.read_primitive_kind(tree.PrimitiveKind.s8)

    def read_primitive_kind(self, kind):
        if kind.value.is_int or kind.value.is_bool:
            return self.read_int_by_kind(kind)
        elif kind.value.is_string:
            return self.read_string(*self.get_string_info(kind))
        else:
            assert False
