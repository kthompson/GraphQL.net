module Document

open GraphQL
open AST

open System

let getOperations document = 
    document.Definitions
    |> Seq.choose (fun x ->
        match x with 
        | Operation o -> Some o
        | _ -> None
    )        

let getOperation document operationName =
    let operationSelector : Operation seq -> Operation option =
        if String.IsNullOrWhiteSpace(operationName) then
            Seq.tryFind (fun _ -> true)
        else
            Seq.tryFind (fun op ->
                match op.Name with 
                | Some (Name name) -> 
                    if name = operationName then true
                    else false
                | _ -> false
            )

    document
    |> getOperations
    |> operationSelector   

let getFragments document = 
    document.Definitions
    |> Seq.choose (fun x ->
        match x with 
        | Fragment o -> Some o
        | _ -> None
    )        