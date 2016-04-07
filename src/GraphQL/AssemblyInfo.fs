namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("GraphQL")>]
[<assembly: AssemblyProductAttribute("GraphQL.net")>]
[<assembly: AssemblyDescriptionAttribute("An implementation of Facebook's GraphQL for .NET and F#.")>]
[<assembly: AssemblyVersionAttribute("0.2.0")>]
[<assembly: AssemblyFileVersionAttribute("0.2.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.2.0"
    let [<Literal>] InformationalVersion = "0.2.0"
