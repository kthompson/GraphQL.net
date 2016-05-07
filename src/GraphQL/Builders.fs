module GraphQL.Builders

open Schema
open Util
open Directives

type ArgumentBuilder() = 
    member this.Yield(()) =
        {
            Argument.Name = ""
            Description = None
            Type = InputType.Scalar Scalars.IntType
            DefaultValue = None
        }

    member this.Run(arg : Argument) =
        invariant arg.Name "Type must be named."
        assertValidName arg.Name         
        arg

    [<CustomOperation("typ")>]
    member this.Type(arg : Argument, typ) =
        {arg with Type = typ}

    [<CustomOperation("name")>]
    member this.Name(arg : Argument, name) =
        {arg with Name = name}

    [<CustomOperation("description")>]
    member this.Description(arg : Argument, description) =
        {arg with Description = description |> Description |> Some}

    [<CustomOperation("defaultValue")>]
    member this.DefaultValue(arg : Argument, defaultValue) =
        {arg with DefaultValue = Some defaultValue}

    [<CustomOperation("int")>]
    member this.Int(arg: Argument) =
        {arg with Type = InputType.Scalar Scalars.IntType}

    [<CustomOperation("bool")>]
    member this.Bool(arg: Argument) =
        {arg with Type = InputType.Scalar Scalars.BoolType}

    [<CustomOperation("float")>]
    member this.Float(arg: Argument) =
        {arg with Type = InputType.Scalar Scalars.FloatType}

    [<CustomOperation("string")>]
    member this.String(arg: Argument) =
        {arg with Type = InputType.Scalar Scalars.StringType}

    [<CustomOperation("id")>]
    member this.Id(arg: Argument) =
        {arg with Type = InputType.Scalar Scalars.IdType}

type FieldBuilder() =
    member this.Yield(()) =
        {
            FieldDefinition.Description = None;
            DeprecationReason = None
            Type = OutputType.Scalar Scalars.IntType
            Args = List.empty
            Resolve = fun a b c d -> None
            }
            
    [<CustomOperation("deprecation")>]
    member this.Deprecation(field: FieldDefinition, deprecation) =
        {field with DeprecationReason = deprecation |> Deprecation |> Some}

    [<CustomOperation("description")>]
    member this.Description(field: FieldDefinition, description) =
        {field with Description = description |> Description |> Some}

    [<CustomOperation("typ")>]
    member this.Type(field: FieldDefinition, typ : OutputType) =
        {field with Type = typ}

    [<CustomOperation("args")>]
    member this.Arguments(field: FieldDefinition, args : Argument list) =
        {field with Args = args}

    [<CustomOperation("obj")>]
    member this.Object(field: FieldDefinition, typ : ObjectType) =
        this.Type(field, OutputType.Object typ)

    [<CustomOperation("list")>]
    member this.List(field: FieldDefinition, typ : OutputType) =        
        this.Type(field, OutputType.List typ)

    [<CustomOperation("scalar")>]
    member this.Scalar(field: FieldDefinition, scalar) =        
        this.Type(field, OutputType.Scalar scalar)

    [<CustomOperation("int")>]
    member this.Int(field: FieldDefinition) =
        this.Scalar(field, Scalars.IntType)

    [<CustomOperation("bool")>]
    member this.Bool(field: FieldDefinition) =
        this.Scalar(field, Scalars.BoolType)
        
    [<CustomOperation("float")>]
    member this.Float(field: FieldDefinition) =
        this.Scalar(field, Scalars.FloatType)

    [<CustomOperation("string")>]
    member this.String(field: FieldDefinition) =
        this.Scalar(field, Scalars.StringType)

    [<CustomOperation("id")>]
    member this.Id(field: FieldDefinition) =
        this.Scalar(field, Scalars.IdType)

    [<CustomOperation("resolve")>]
    member this.Resolve(field: FieldDefinition, resolver) =
        {field with Resolve = resolver}

type ObjectTypeBuilder() =
    member this.Yield(()) = {
        ObjectType.Description = None;
        Name = ""
        Fields = Map.empty
        Interfaces = List.empty
        IsTypeOf = None
    }

    member this.Run(typ : ObjectType) =
        invariant typ.Name "Type must be named."
        assertValidName typ.Name

        typ

    [<CustomOperation("description")>]
    member this.Description(typ: ObjectType, description) =
        {typ with Description = description |> Description |> Some}

    [<CustomOperation("name")>]
    member this.Name(typ: ObjectType, name) =
        {typ with Name = name}

    [<CustomOperation("fields")>]
    member this.Fields(typ: ObjectType, fields) =
        {typ with Fields = fields}

    [<CustomOperation("interfaces")>]
    member this.Interfaces(typ: ObjectType, interfaces) =
        {typ with Interfaces = interfaces}

    [<CustomOperation("isTypeOf")>]
    member this.IsTypeOf(typ: ObjectType, isTypeOf) =
        {typ with IsTypeOf = isTypeOf}

type SchemaBuilder() =
    member this.Yield(()) = {
        Schema.QueryType = None
        MutationType = None
        SubscriptionType = None
        TypeMap = Map.empty
        Directives = List.empty
    }

    
    member this.Run(schema : Schema) =
        //invariant typ.Name "Type must be named."
        //assertValidName typ.Name
        //invariant typ.IsTypeOf (sprintf "%s must provide 'isTypeOf' function" typ.Name)

        let initialTypes = [
            schema.QueryType
            schema.SubscriptionType
            schema.MutationType
            // TODO: __Schema introspection support
        ]

        // TODO: implement type reducer to collect all types that are used based 
        // on above types and any supplied types

        if schema.Directives.Length = 0 then
            {schema with Directives = [skipDirective; includeDirective]}
        else
            schema

    [<CustomOperation("query")>]
    member this.Query(typ: Schema, query) =
        {typ with QueryType = query |> Some}

    [<CustomOperation("mutation")>]
    member this.Mutation(typ: Schema, mutation) =
        {typ with MutationType = mutation |> Some}

    [<CustomOperation("subscription")>]
    member this.Subscription(typ: Schema, subscription) =
        {typ with SubscriptionType = subscription |> Some}

    [<CustomOperation("types")>]
    member this.Types(typ: Schema, types) =
        {typ with TypeMap = types}

let schema = new SchemaBuilder()
let objectType = new ObjectTypeBuilder()
let argument = new ArgumentBuilder()
let field = new FieldBuilder()
//
//
//type TypeBuilder() =
//
//     [<CustomOperation("name")>]
//    member x.Name(ObjectType typ, name) =
