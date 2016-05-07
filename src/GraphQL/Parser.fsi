module GraphQL.Parser

open AST

type ParserOptions = {Trace: bool}

/// GraphQL parser that takes a string and converts it to 
/// a <see cref="AST.Document"/> object
val parse: (string -> Document)

/// GraphQL parser that takes a <see cref="ParserOptions"/>
/// object and string and converts to a <see cref="AST.Document"/> 
/// object
val parseWithOptions: ParserOptions -> string -> Document

