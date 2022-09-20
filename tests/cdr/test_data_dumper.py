import unittest
from pathlib import Path

from bridle import IdlParser
from bridle.cdr.data_dumper import DataDumper

idl_parser = IdlParser()
test_path = Path(__file__).parent


class DataDumperTests(unittest.TestCase):
    def test_simple(self):
        trees = idl_parser.parse([test_path / 'simple.idl'])
        value_entries, raw_data_entries = DataDumper(trees).dump_i(
            (test_path / 'simple.cdr').read_bytes(), '::Simple')

        self.assertListEqual([
            "[bold]name: [user_data]'Hello, World!'[/user_data][/bold]\n[control_size]string size"
                + "[/control_size]\n[user_data]string data[/user_data]",
            '[bold]u32_value: [user_data]305419896[/user_data][/bold]\n[padding]padding'
                + '[/padding]\n[user_data]u32 value[/user_data]',
            '[bold]valid: [user_data]True[/user_data][/bold]\n[user_data]boolean value[/user_data]'
        ], value_entries)
        self.assertListEqual([
            '[bold]00:[/bold]  [control_size]0e 00 00 00[/control_size] [user_data]48 65 6c 6c '
                + '6f 2c 20 57 6f 72 6c 64[/user_data]\n[bold]10:[/bold]  '
                + '[user_data]21 00[/user_data]\n',
            '           [padding]00 00[/padding] [user_data]78 56 34 12[/user_data]\n\n',
            '                             [user_data]01[/user_data]\n'
        ], raw_data_entries)

    def test_simple_with_issues(self):
        trees = idl_parser.parse([test_path / 'simple.idl'])
        value_entries, raw_data_entries = DataDumper(trees).dump_i(
            (test_path / 'simple_with_issues.cdr').read_bytes(), '::Simple')

        self.assertListEqual([
            "[bold]name: [user_data]'Hello, World!'[/user_data][/bold]\n[control_size]string size"
                + "[/control_size]\n[user_data]string data[/user_data] [yellow](WARNING: string is "
                + "not terminated with nul)[no yellow]",
            '[bold]u32_value: [user_data]305419896[/user_data][/bold]\n[padding]padding'
                + '[/padding]\n[user_data]u32 value[/user_data]',
            '[bold]valid: [user_data]True[/user_data][/bold]\n[user_data]boolean value'
                + '[/user_data] [yellow](WARNING: boolean is a value other than 1 or 0)[no yellow]'
        ], value_entries)
        self.assertListEqual([
            '[bold]00:[/bold]  [control_size]0d 00 00 00[/control_size] [user_data]48 65 6c 6c '
                + '6f 2c 20 57 6f 72 6c 64[/user_data]\n[bold]10:[/bold]  '
                + '[user_data]21[/user_data]\n',
            '        [padding]00 00 00[/padding] [user_data]78 56 34 12[/user_data]\n\n',
            '                             [user_data]02[/user_data]\n'
        ], raw_data_entries)
