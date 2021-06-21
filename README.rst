######
Bridle
######

Bridle is parser and AST library for OMG Interface Description Language.

**************************************************
Intentional Divergences with the IDL Specification
**************************************************

* IDL says everything should be ASCII, except for strings and character literals, which can be Latin-1.
  Bridle accepts any encoding that Python supports.
* Zero values ``\0`` are allowed in string and character literals, as there are languages that don't have null-terminated strings.
