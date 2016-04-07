(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Introducing your project
========================

Say more

*)
#r "GraphQL/GraphQL.dll"
open GraphQL

printfn "%A" <| Parser.parse """{  
    uri,
    contact {
      phone,
      name
    },
    height
  }"""  

(**
Some more info
*)
