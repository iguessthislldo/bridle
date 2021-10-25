import logging
from pathlib import Path

from pygls.server import LanguageServer
from pygls.lsp.methods import (
    TEXT_DOCUMENT_DID_OPEN,
    TEXT_DOCUMENT_DID_CHANGE,
)
from pygls.lsp.types import (
    Diagnostic,
    DiagnosticSeverity,
    Range,
    Position,
    DidOpenTextDocumentParams,
    DidChangeTextDocumentParams,
)

from bridle import IdlParser, BridleError


class IdlLanguageServer(LanguageServer):
    CONFIGURATION_SECTION = 'idlServer'

    def __ini__(self):
        super().__init__()


idl_language_server = IdlLanguageServer()


def bridle_location_to_range(loc):
    return Range(
        start=Position(line=loc.line - 1, character=loc.col - 1),
        end=Position(line=loc.line - 1, character=loc.col - 1 + loc.length),
    )


def validate(ls, params):
    uri = params.text_document.uri
    doc = ls.workspace.get_document(uri)
    file_scheme = 'file://'
    if not uri.startswith(file_scheme):
        raise ValueError('Expected URI with {} scheme'.format(file_scheme))
    path = Path(uri[len(file_scheme):])
    diagnostics = []

    try:
        IdlParser(raise_parse_errors=True).parse(direct_inputs=[doc.source], effective_path=path)
    except BridleError as e:
        diagnostics.append(Diagnostic(
            message=e.message_without_location,
            source=type(idl_language_server).__name__,
            range=bridle_location_to_range(e.location),
            severity=DiagnosticSeverity.Error,
        ))

    ls.publish_diagnostics(doc.uri, diagnostics)


@idl_language_server.feature(TEXT_DOCUMENT_DID_OPEN)
async def did_open(ls, params: DidOpenTextDocumentParams):
    validate(ls, params)


@idl_language_server.feature(TEXT_DOCUMENT_DID_CHANGE)
def did_change(ls, params: DidChangeTextDocumentParams):
    validate(ls, params)


def run_idl_language_server(args, logname):
    if args.log:
        logging.basicConfig(filename=logname, filemode='w', level=logging.DEBUG)
    if args.tcp:
        idl_language_server.start_tcp(args.host, args.port)
    elif args.ws:
        idl_language_server.start_ws(args.host, args.port)
    else:
        idl_language_server.start_io()
