import itertools
from pathlib import Path

from rich.tree import Tree as RichTree
from rich.table import Table as RichTable
from rich.console import Console as RichConsole
from rich.theme import Theme as RichTheme

from bridle.tree import PrimitiveNode
from bridle.type_files import add_type_file_argument_parsing, type_files_to_trees

from .serializer import Serializer


default_theme = RichTheme({
    'user_data': 'magenta',
    'control_size': 'blue',
    'padding': '',
})


class DataDumper:
    def __init__(self, trees, raw_data_width=16):
        self.trees = trees
        self.raw_data_width = raw_data_width
        self.console = RichConsole(theme=default_theme)

    def struct_deserializer(self, struct_type, serializer, contents):
        for child in struct_type.children:
            if isinstance(child.type_node, PrimitiveNode):
                value = serializer.read_primitive_kind(child.type_node.kind)
                details = serializer.take_details()
                value_lines = [
                    "[bold]{}: [user_data]{}[/user_data][/bold]".format(child.name, repr(value))
                ]
                for detail in details:
                    detail_str = '[{0}]{1}[/{0}]'.format(detail.kind.name, detail.what)
                    message = detail.get_message()
                    if message:
                        color = 'red' if detail.status == detail.status.error else 'yellow'
                        detail_str += ' [{0}]{1}[no {0}]'.format(color, message)
                    value_lines.append(detail_str)
                contents.append((details, value_lines))
            else:
                raise TypeError(child.type_node.__class__.__name__ + ' is not supported')

    def raw_data_formatter(self, total_size, pos, details):
        lines = []
        loc_width = len(f'{total_size:x}')
        offset = pos % self.raw_data_width
        if offset:
            line = [' ' * (loc_width + 2)]
        else:
            line = [f'[bold]{pos:0>{loc_width}x}:[/bold] ']
        line += ['  '] * offset
        for detail in details:
            style_unapply = None
            for byte in detail.raw_data:
                if style_unapply is None:
                    line.append('[{}]{:02x}'.format(detail.kind.name, byte))
                    style_unapply = detail.kind.name
                else:
                    line.append('{:02x}'.format(byte))
                pos += 1
                if pos % self.raw_data_width == 0:
                    if style_unapply is not None:
                        line[-1] += '[/{}]'.format(style_unapply)
                        style_unapply = None
                    lines.append(' '.join(line))
                    line = [f'[bold]{pos:0>{loc_width}x}:[/bold] '] \
                        + ['  '] * (pos % self.raw_data_width)
            if style_unapply is not None:
                line[-1] += '[/{}]'.format(style_unapply)
                style_unapply = None
        if line:
            lines.append(' '.join(line))
        return pos, lines

    def dump_i(self, buffer, topic_type_name):
        type_node = None
        for tree in self.trees:
            type_node = tree.get(topic_type_name)
            if type_node is not None:
                break

        contents = []
        self.struct_deserializer(type_node, Serializer(buffer, save_details=True), contents)

        pos = 0
        raw_data_entries = []
        value_entries = []
        for details, value_lines_tmp in contents:
            pos, raw_data_lines_tmp = self.raw_data_formatter(len(buffer), pos, details)
            raw_data_lines = []
            value_lines = []
            for raw_data_line, value_line in itertools.zip_longest(
                    raw_data_lines_tmp, value_lines_tmp, fillvalue=''):
                raw_data_lines.append(raw_data_line)
                value_lines.append(value_line)
            raw_data_entries.append('\n'.join(raw_data_lines))
            value_entries.append('\n'.join(value_lines))

        return value_entries, raw_data_entries

    def dump(self, buffer, topic_type_name):
        value_entries, raw_data_entries = self.dump_i(buffer, topic_type_name)

        table = RichTable(title=topic_type_name)
        table.add_column("Values")
        table.add_column("Raw Data")
        value_tree = RichTree('Root')
        for value_entry in value_entries:
            value_tree.add(value_entry)
        table.add_row(
            value_tree,
            '\n' + '\n'.join(raw_data_entries),
        )
        self.console.print(table)


def dump_data(args):
    trees = type_files_to_trees(args)
    DataDumper(trees).dump(args.data_file.read_bytes(), args.topic_type_name)


def add_dump_data_subcmd(subcmds):
    subcmd = subcmds.add_parser('dump-data', help='Describe a serialized sample of a type')
    subcmd.set_defaults(subcmd=dump_data)
    subcmd.add_argument('topic_type_name',
        metavar='TOPIC_TYPE_NAME',
        help='Name of the topic/base/top-level type')
    subcmd.add_argument('data_file',
        metavar='DATA_FILE', type=Path,
        help='CDR File')
    add_type_file_argument_parsing(subcmd)
