from io import StringIO

from ..utils import Location, is_sequence, must_be_sequence, Configurable

class IdlFile:

    def __init__(self, path=None, direct_input=None, effective_path=None):
        self.path, self.source_key = Location.make_new_source_key(
            path=path, effective_path=effective_path)
        if direct_input is not None and path is None:
            self.idl_file_contents = direct_input
        elif path is not None and direct_input is None:
            self.idl_file_contents = self.path.read_text()
        else:
            raise ValueError('Either path or direct_input must be set. Not both or neither.')
        self.contents = None

    def name(self):
        return self.path

    def load(self, preprocessor):
        if preprocessor is None:
            self.contents = self.idl_file_contents
        else:
            sio = StringIO()
            preprocessor.parse(self.idl_file_contents, str(self.path))
            preprocessor.write(sio)
            if preprocessor.return_code != 0:
                raise ErrorsReported('Uncaught Preprocessor Error')
            self.contents = sio.getvalue()

    @classmethod
    def get_from(cls, paths=[], direct_inputs=[], effective_path=None):
        return [cls(path=path) for path in paths] + \
            [cls(direct_input=s, effective_path=effective_path) for s in direct_inputs]
