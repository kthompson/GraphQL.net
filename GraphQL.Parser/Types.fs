namespace GraphQL

open System

type OperationType = 
    | Query
    | Mutation

type Argument = {Name:string; Value: Object}
type Directive = {Name:string; Arguments: Argument array}
type FragmentSpread = {Name:string; Directives: Directive array}

type VariableType = {Name: string; IsList: bool; AllowsNull: bool}
type Variable = {Name:string; Type: VariableType; DefaultValue: Object; Value: Object}

type Selection = 
    | Field of Field
    | FragmentSpread of FragmentSpread
    | InlineFragment of InlineFragment
 and Field = {Name: string; Alias: string; Directives: Directive array; Arguments: Argument array; Selections: Selection array}
 and FragmentDefinition = {Name:string; Type: string; Directives: Directive array; Selections: Selection array}
 and InlineFragment = {Type:string; Directives: Directive array; Selections: Selection array}
 

type Operation = 
    {
        OperationType: OperationType; 
        Name: string; 
        Directives: Directive array;
        Variables: Variable array;
        Selections: Selection array
    }

type Definition = 
    | Operation of Operation
    | Fragment of FragmentDefinition

type Document = {Definitions: Definition array}