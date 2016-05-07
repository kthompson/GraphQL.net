module Util

open System
open System.Text.RegularExpressions

let invariant (condition : Object) (message: string) =
    let condition = 
       match condition with
       | :? string as t -> String.IsNullOrEmpty(t)
       | :? bool as t -> not t 
       | :? option<Object> as opt ->
            match opt with
            | Some _ -> false
            | None -> true
       | t -> t = null
       
    if condition then
        raise (new InvalidOperationException(message))
    else
        ()


let assertValidName (name : string)  =
    name
    |> sprintf "Names must match /^[_a-zA-Z][_a-zA-Z0-9]*$/ but %s does not."
    |> invariant (Regex.IsMatch(name, "^[_a-zA-Z][_a-zA-Z0-9]*$"))

let memoize f =
    let result = f ()
    fun () ->
        result
