module GraphQL.Parser

type ParserOptions

val parse: (string -> AST.Document)
val parseWithOptions: ParserOptions -> string -> AST.Document

