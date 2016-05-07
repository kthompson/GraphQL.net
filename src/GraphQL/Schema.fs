module GraphQL.Schema

open Internal
open AST
open Util
open System

type StringMap<'T> = Map<string, 'T>

type Description = Description of string
type Deprecation = Deprecation of string
type DirectiveLocation =
  | Query
  | Mutation
  | Subscription
  | Field
  | FragmentDefinition
  | FragmentSpread
  | InlineFragment

type ScalarType = {
    Name : string;
    Description : Description option;
    Serialize : mixed -> any option;
    ParseValue : mixed -> any option;
    ParseLiteral : Value -> any option;
} with 
  override this.ToString() = this.Name;

type UnionType() = class end
type EnumType() = class end


type ResolveInfo<'TSchema, 'TOutputType, 'TCompositeType> = {
  FieldName: string;
  FieldASTs: Field[];
  ReturnType: 'TOutputType; // OutputType;
  ParentType: 'TCompositeType; //CompositeType;
  Schema: 'TSchema;
  Fragments: StringMap<Fragment>;
  RootValue: mixed;
  Operation: Operation;
  VariableValues: StringMap<mixed>;
}

//
///// (value -> context -> info) -> bool
type IsTypeOfFn<'TSchema, 'TOutputType, 'TCompositeType> = mixed -> mixed -> ResolveInfo<'TSchema, 'TOutputType, 'TCompositeType> -> bool

/// source -> args -> context -> info -> mixed
type FieldResolveFn<'TSchema, 'TOutputType, 'TCompositeType> = mixed * StringMap<mixed> * mixed -> ResolveInfo<'TSchema, 'TOutputType, 'TCompositeType> -> mixed

type ParameterizedArgument<'TInputType> = {
  Name: string
  Description: Description option
  Type: 'TInputType
  DefaultValue: Object option
}

type ParameterizedFieldDefinition<'TInputType, 'TOutputType> = {
    Description : Description option;
    DeprecationReason : Deprecation option;
    Type : 'TOutputType;
    Args : ParameterizedArgument<'TInputType> list
    Resolve: (any -> Map<string, Object> -> any -> any -> any option)
}

/// (value -> context -> info) -> SchemaType
//type TypeResolveFn<'TSchema, 'TOutputType, 'TCompositeType, 'TObjectType> =
//    mixed -> mixed -> ResolveInfo<'TSchema, 'TOutputType, 'TCompositeType> -> 'TObjectType option // ?GraphQLObjectType

type ParameterizedInterfaceType<'TSchema, 'TInputType, 'TOutputType, 'TCompositeType>
    (
        name, 
        description, 
        resolveType,
        fields : StringMap<ParameterizedFieldDefinition<'TInputType, 'TOutputType>>
    ) = 
  do invariant name "Type must be named."
  do assertValidName name

  member this.Name = name
  member this.Description : Description = description
  member this.Fields = fields
  member this.ResolveType (value, context, info) =
    match resolveType with
    | Some fn -> fn value context info
    | None -> None

  override this.ToString() = name;

type ParameterizedObjectType<'TSchema, 'TInputType, 'TOutputType, 'TCompositeType> = {
    Name: string
    Description: Description option
    Fields: Map<String, ParameterizedFieldDefinition<'TInputType, 'TOutputType>>
    Interfaces: ParameterizedInterfaceType<'TSchema, 'TInputType, 'TOutputType, 'TCompositeType> list
    IsTypeOf: IsTypeOfFn<'TSchema, 'TOutputType, 'TCompositeType> option
}

type InputType =
| Scalar of ScalarType
| Enum
| InputObject 
| List of InputType
| NonNull of InputType

type ParameterizedCompositeType<'TSchema, 'TOutputType> = 
| Object of ParameterizedObjectType<'TSchema, InputType, 'TOutputType, ParameterizedCompositeType<'TSchema, 'TOutputType>>
| Interface of ParameterizedInterfaceType<
                'TSchema, 
                InputType, 
                'TOutputType, 
                ParameterizedCompositeType<'TSchema, 'TOutputType>>
| Union

type LeafType = 
| Scalar of ScalarType
| Enum

type ParameterizedOutputType<'TSchema> =
| Scalar of ScalarType
| Object of ParameterizedObjectType<'TSchema, InputType, ParameterizedOutputType<'TSchema>, ParameterizedCompositeType<'TSchema, ParameterizedOutputType<'TSchema>>>
| Interface of ParameterizedInterfaceType<'TSchema, InputType, ParameterizedOutputType<'TSchema>, ParameterizedCompositeType<'TSchema, ParameterizedOutputType<'TSchema>>>
| Union
| Enum
| List of ParameterizedOutputType<'TSchema>
| NonNull of ParameterizedOutputType<'TSchema>

type SchemaType<'TSchema> =
  | Scalar of ScalarType
  | Object of ParameterizedObjectType<'TSchema, InputType, ParameterizedOutputType<'TSchema>, ParameterizedCompositeType<'TSchema, ParameterizedOutputType<'TSchema>>>
  | Interface of ParameterizedInterfaceType<'TSchema, InputType, ParameterizedOutputType<'TSchema>, ParameterizedCompositeType<'TSchema, ParameterizedOutputType<'TSchema>>>
  | Union // of UnionType
  | Enum // of EnumType
  | InputObject
  | List of SchemaType<'TSchema>
  | NonNull of SchemaType<'TSchema>
  with
    override x.ToString() = 
        match x with
        | Scalar typ -> typ.ToString()
        | Object typ -> typ.ToString()
        | Interface typ -> typ.ToString()
        | List typ -> "[" + typ.ToString() + "]"
        | NonNull typ -> typ.ToString() + "!"
        | Union | Enum | InputObject -> "Not implemented"

type Directive = {
    Name : string;
    Description : Description option
    Locations: DirectiveLocation[]
    Arguments : ParameterizedArgument<InputType>[]
}

type Schema = {
    QueryType : ObjectType option
    MutationType : ObjectType option
    SubscriptionType : ObjectType option
    TypeMap : Map<string, SchemaType<Schema>>
    Directives: Directive list
} with
    member x.GetType name = x.TypeMap.TryFind name
    
and OutputType = ParameterizedOutputType<Schema>
and CompositeType = ParameterizedCompositeType<Schema, OutputType>
and ObjectType = ParameterizedObjectType<Schema, InputType, OutputType, CompositeType>
and InterfaceType = ParameterizedInterfaceType<Schema, InputType, OutputType, CompositeType>
and Argument = ParameterizedArgument<InputType>
and FieldDefinition = ParameterizedFieldDefinition<InputType, OutputType>

let rec getNamedType = function
  | List typ -> getNamedType typ
  | NonNull typ -> getNamedType typ
  | typ -> typ

let isInputType typ = 
  match getNamedType typ with
  | Scalar _ | Enum _ | InputObject -> true
  | _ -> false

let isOututType typ = 
  match getNamedType typ with
  | Scalar _ | Object _ | Interface _ | Union _ | Enum _ -> true
  | _ -> false

let isLeafType typ = 
  match getNamedType typ with
  | Scalar _ | Enum _ -> true
  | _ -> false

let isCompositeType = function
  | Object _ | Interface _ | Union _ -> true
  | _ -> false

let isAbstractType = function
  | Interface _ | Union _ -> true
  | _ -> false

let getNullableType = function
  | NonNull typ -> typ
  | typ -> typ
