namespace GraphQL

[<Sealed>]
type Parser = 
    new : unit -> Parser
    member Parse : string -> Document