module GraphQL.AST

open System

type Name =
    | Name of string

type Value = 
    | Variable of string
    | IntValue of int64
    | FloatValue of float
    | StringValue of string
    | BooleanValue of bool
    | EnumValue of string // TODO?
    | ListValue of Value array
    | ObjectValue of Name * Value

type Argument = {Name:Name; Value: Value}
type Directive = {Name:Name; Arguments: Argument array}
type FragmentSpread = {Name:Name; Directives: Directive array}

type NamedType = string
type VariableType = 
    | NamedType of NamedType
    | ListType of VariableType
    | NonNullType of VariableType // NamedType | ListType


type Selection = 
    | Field of Field
    | InlineFragment of InlineFragment
    | FragmentSpread of FragmentSpread
 and SelectionSet = {selections: Selection array}
 and Field = {
    Alias: Name option; 
    Name: Name; 
    Arguments: Argument array; 
    Directives: Directive array; 
    Selections: SelectionSet option}
 and Fragment = {
    Name: string; 
    TypeCondition: NamedType; 
    Directives: Directive array; 
    Selections: SelectionSet}
 and InlineFragment = {
    TypeCondition:NamedType; 
    Directives: Directive array; 
    Selections: SelectionSet}


type Variable = {Name:string; Type: VariableType; DefaultValue: Value option}

type OperationType = Query | Mutation | Subscription

type Operation = 
    {
        OperationType: OperationType; 
        Name: Name option; 
        Variables: Variable array;
        Directives: Directive array;
        Selections: SelectionSet
    }

type Definition = 
    | Operation of Operation
    | Fragment of Fragment

type Document = {Definitions: Definition array}