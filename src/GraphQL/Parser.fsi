module GraphQL.Parser

type ParserOptions

/// GraphQL parser that takes a string and converts it to 
/// a <see cref="AST.Document"/> object
val parse: (string -> AST.Document)

/// GraphQL parser that takes a <see cref="ParserOptions"/>
/// object and string and converts to a <see cref="AST.Document"/> 
/// object
val parseWithOptions: ParserOptions -> string -> AST.Document

