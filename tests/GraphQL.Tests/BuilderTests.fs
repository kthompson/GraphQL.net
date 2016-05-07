module BuilderTests

open System
open Xunit
open FsUnit.Xunit
    
open GraphQL
open Schema
open Builders
open Scalars

let equals expected actual  = Assert.True ((actual = expected), sprintf """
expected %+A
but got %+A""" expected actual)

let throws<'T when 'T :> exn> f = Assert.Throws<'T>(Action f)

let equalArg (expected : Argument) (actual : Argument) = 
    equals expected.Name actual.Name
    equals expected.Description actual.Description
    equals expected.DefaultValue actual.DefaultValue

    //TODO: we should probably look at the cases here
    equals (expected.Type.GetType()) (actual.Type.GetType())

let equalField (expected : FieldDefinition) (actual : FieldDefinition) = 
    equals expected.Description actual.Description
    equals expected.DeprecationReason actual.DeprecationReason
    
    //TODO: we should probably look at the cases here
    equals (expected.Type.GetType()) (actual.Type.GetType())
    
    let expectedArgs = expected.Args
    let actualArgs = actual.Args
    equals expectedArgs.Length actualArgs.Length

    List.zip expectedArgs actualArgs 
    |> List.iter(fun (expArg, actArg) ->
        equalArg expArg actArg
    )

[<Fact>]
let ``FieldBuilder: supports argument`` () =
    let expected =
        {
            FieldDefinition.Description = None
            DeprecationReason = None
            Type = OutputType.Scalar IntType
            Args =
                {
                    Argument.Name = "id"
                    Description = None
                    Type = (InputType.Scalar IntType) 
                    DefaultValue = None
                } |> List.singleton
            Resolve = fun a b c d -> None
        }

    let f =
        field {
            typ (OutputType.Scalar IntType)
            args [
                argument {
                    name "id"
                    typ (InputType.Scalar IntType)
                }
            ]            
        }
    
    equalField expected f


[<Fact>]
let ``FieldBuilder: supports description`` () =
    let expected =
        {
            FieldDefinition.Description = "Hi" |> Description |> Some
            DeprecationReason = None
            Type = OutputType.Scalar IntType
            Args = []
            Resolve = fun a b c d -> None
        }

    let f =
        field {
            typ (OutputType.Scalar IntType)
            description "Hi"       
        }
    
    equalField expected f

[<Fact>]
let ``FieldBuilder: supports deprecation`` () =
    let expected =
        {
            FieldDefinition.Description = None
            DeprecationReason = "Hi" |> Deprecation |> Some
            Type = OutputType.Scalar IntType
            Args = []
            Resolve = fun a b c d -> None
        }

    let f =
        field {
            typ (OutputType.Scalar IntType)
            deprecation "Hi"       
        }
    
    equalField expected f

[<Fact>]
let ``FieldBuilder: supports type keywords`` () =
    let expField typ : FieldDefinition = 
        {
            FieldDefinition.Description = None
            DeprecationReason = None
            Type = OutputType.Scalar typ
            Args = []
            Resolve = fun a b c d -> None
        }

    equalField (expField IntType) (field { int })
    equalField (expField BoolType) (field { bool })
    equalField (expField FloatType) (field { float })
    equalField (expField StringType) (field { string })
    equalField (expField IdType) (field { id })

[<Fact>]
let ``FieldBuilder: supports arguments`` () =
    
    let expected =
        {
            FieldDefinition.Description = None
            DeprecationReason = None
            Type = OutputType.Scalar IntType
            Args =
                [{
                    Argument.Name = "id"
                    Description = None
                    Type = (InputType.Scalar IntType) 
                    DefaultValue = None
                };
                {
                    Argument.Name = "cat"
                    Description = "meow" |> Description |> Some
                    Type = (InputType.Scalar StringType) 
                    DefaultValue = Some ("Tabby" :> Object)
                }]
            Resolve = fun a b c d -> None
        }

    let f =
        field {        
            typ (IntType |> OutputType.Scalar)     
            args [
                argument {
                    name "id"
                    typ (IntType |> InputType.Scalar)
                }
                argument {
                    name "cat"
                    typ (StringType |> InputType.Scalar)
                    description "meow"
                    defaultValue "Tabby"
                }]            
        }
    
    equalField expected f




[<Fact>]
let ``ArgumentBuilder: supports name`` () =

    let a =
        argument { 
            typ (StringType |> InputType.Scalar)
            name "a"
        }

    let expected = 
        {
            Argument.Name = "a"
            Description = None
            Type = StringType |> InputType.Scalar
            DefaultValue = None            
        }

    equalArg expected a

[<Fact>]
let ``ArgumentBuilder: supports description`` () =
    let a =
        argument { 
            typ (StringType |> InputType.Scalar)
            name "a"
            description "taco tuesday"
        }

    let expected = 
        {
            Argument.Name = "a"
            Description = "taco tuesday" |> Description |> Some 
            Type = StringType |> InputType.Scalar
            DefaultValue = None            
        }

    equalArg expected a

[<Fact>]
let ``ArgumentBuilder: supports defaultValue`` () =
    let a =
        argument { 
            typ (StringType |> InputType.Scalar)
            name "b"
            defaultValue "hi"
        }

    let expected = 
        {
            Argument.Name = "b"
            Description = None
            Type = StringType |> InputType.Scalar
            DefaultValue = Some ("hi" :> Object)
        }

    equalArg expected a

[<Fact>]
let ``ArgumentBuilder: supports type keywords`` () =
    let expArg typ : Argument = 
        {
            Argument.Name = "t"
            Type = typ |> InputType.Scalar
            Description = None
            DefaultValue = None
        }
        
    equalArg (expArg IntType) (argument { 
        int 
        name "t" })
    equalArg (expArg BoolType) (argument { 
        bool 
        name "t" })
    equalArg (expArg FloatType) (argument {
        float 
        name "t" })
    equalArg (expArg StringType) (argument {
        string 
        name "t" })
    equalArg (expArg IdType) (argument {
        id 
        name "t" })
    

[<Fact>]
let ``ArgumentBuilder: arg without name throws`` () =
    let exp = throws<InvalidOperationException> (fun () -> argument { int } |> ignore)

    exp.Message |> should equal "Type must be named." 


[<Fact>]
let ``ArgumentBuilder: arg with invalid name throws`` () =
    let exp = throws<InvalidOperationException> (fun () -> argument { name "-" } |> ignore)

    exp.Message |> should equal "Names must match /^[_a-zA-Z][_a-zA-Z0-9]*$/ but - does not." 


[<Fact>]
let ``ObjectTypeBuilder: object with invalid name throws`` () =
    let exp = throws<InvalidOperationException> (fun () -> objectType { name "-" } |> ignore)

    exp.Message |> should equal "Names must match /^[_a-zA-Z][_a-zA-Z0-9]*$/ but - does not." 

[<Fact>]
let ``ObjectTypeBuilder: object without name throws`` () =
    let exp = throws<InvalidOperationException> (fun () -> objectType { description "" } |> ignore)

    exp.Message |> should equal "Type must be named." 
