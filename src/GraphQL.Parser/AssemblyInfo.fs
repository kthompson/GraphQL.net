namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("GraphQL.Parser")>]
[<assembly: AssemblyProductAttribute("GraphQL.net")>]
[<assembly: AssemblyDescriptionAttribute("An implementation of Facebook's GraphQL for .NET and F#.")>]
[<assembly: AssemblyVersionAttribute("0.1.0")>]
[<assembly: AssemblyFileVersionAttribute("0.1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.1.0"
    let [<Literal>] InformationalVersion = "0.1.0"
