module GraphQL.Directives

open Scalars
open Schema

let skipDirective = {
    Directive.Name = "skip";
    Description =
        "Directs the executor to skip this field or fragment when the `if` argument is true."
        |> Description
        |> Some
    Locations = [| Field; FragmentSpread; InlineFragment |];
    Arguments =
        [|
            {   
                ParameterizedArgument.Name = "if";
                Description =
                    "Skipped when true"
                    |> Description
                    |> Some;
                Type = BoolType |> InputType.Scalar |> InputType.NonNull;
                DefaultValue = None
             }
         |]
}


let includeDirective = {
    Directive.Name = "include";
    Description =
        "Directs the executor to include this field or fragment only when the `if` argument is true."
        |> Description
        |> Some
    Locations = [| Field; FragmentSpread; InlineFragment |];
    Arguments =
        [|
            {   
                ParameterizedArgument.Name = "if";
                Description = "Included when true" |> Description |> Some;
                Type = BoolType |> InputType.Scalar |> InputType.NonNull;
                DefaultValue = None
             }
         |]
}
