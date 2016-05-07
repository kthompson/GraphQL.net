module GraphQL.Scalars

open Internal

open AST
open Schema

let anySome x = x :> any |> Some

let BoolType = {
    ScalarType.Name = "Boolean";
    Description =
        "The `Boolean` scalar type represents `true` or `false`."
        |> Description
        |> Some;
    Serialize = fun x -> System.Convert.ToBoolean(x) |> anySome
    ParseValue = fun x -> System.Convert.ToBoolean(x) |> anySome
    ParseLiteral = function
    | BooleanValue b -> b |> anySome
    | _ -> None
}

let private coerceInt (x : mixed) =
    try
         System.Convert.ToInt64(x) |> anySome
    with  
    | _ -> None

let IntType = {
    ScalarType.Name = "Int";
    Description =
        "The `Int` scalar type represents non-fractional signed whole numeric " +
        "values. Int can represent values between -(2^31) and 2^31 - 1. "
        |> Description
        |> Some;
    Serialize = coerceInt;
    ParseValue = coerceInt;
    ParseLiteral = function
    | IntValue i -> i |> anySome
    | _ -> None
}



let private coerceFloat (x : mixed) =
    try
         System.Convert.ToDouble(x) :> any |> Some 
    with  
    | _ -> None

let FloatType = {
    ScalarType.Name = "Float";
    Description =
        "The `Float` scalar type represents signed double-precision fractional " +
        "values as specified by " +
        "[IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point). "
        |> Description
        |> Some;
    Serialize = coerceFloat;
    ParseValue = coerceFloat;
    ParseLiteral = function
    | FloatValue f -> f |> anySome
    | _ -> None
}

let private toAny x = x :> any

let StringType = {
    ScalarType.Name = "String";
    Description =
        "The `String` scalar type represents textual data, represented as UTF-8 " +
        "character sequences. The String type is most often used by GraphQL to " +
        "represent free-form human-readable text."
        |> Description
        |> Some;
    Serialize = string >> anySome;
    ParseValue = string >> anySome;
    ParseLiteral = function
    | StringValue s -> s |> anySome
    | _ -> None
}

type Id = 
| IdString of string
| IdInt of int64

let private coerceId (x : mixed) =
    try
         System.Convert.ToInt64(x) |> IdInt |> anySome
    with  
    | _ -> 
        try
            string(x) |> IdString |> anySome
        with
        | _ -> None

let IdType = {
    ScalarType.Name = "String";
    Description =
        "The `String` scalar type represents textual data, represented as UTF-8 " +
        "character sequences. The String type is most often used by GraphQL to " +
        "represent free-form human-readable text."
        |> Description
        |> Some;
    Serialize = coerceId 
    ParseValue = coerceId
    ParseLiteral = function
    | StringValue s -> IdString s |> anySome
    | IntValue i -> IdInt i |> anySome
    | _ -> None
}