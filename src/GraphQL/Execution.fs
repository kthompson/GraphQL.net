module GraphQL.Execution

open AST
open Internal
open Schema
open System
open Rop

type ExecutionError =
| ExecutionError of string

type ExecutionContext = {
    Schema: Schema;
    Fragments: Map<string, Fragment>;
    RootValue: Object;
    Operation: Operation;
    VariableValues: Map<string, Variable * Value>;
    Errors: ExecutionError list;
}

type GraphData = 
| GString of string
| GNumber of float
| GBool   of bool
| GNull
| GList   of GraphData list
| GObject of Map<string, GraphData>

type ExecutionResult = {
    Data: GraphData;
    Errors: ExecutionError list
}

let executeFields context rootType rootValue fields = TODO

let getOperationRootType schema operation : ObjectType = TODO

let collectFields context rootType selections : Map<string, Field list> * string list = TODO

let executeOperation context : ExecutionResult = 
    let rootType = getOperationRootType context.Schema context.Operation
    let (fields, _) = collectFields context rootType context.Operation.Selections

    executeFields context rootType context.RootValue fields

let getVariableValue (schema : Schema) (var : Variable) value = 
  TODO

let getVariableValues schema (variables : Variable array) (inputs : Map<string, Value>) = 
  variables 
  |> Seq.ofArray
  |> Seq.map (fun var ->
    inputs.TryFind var.Name
    |> function
    | Some value -> getVariableValue schema var value
    | None -> getVariableValue schema var var.DefaultValue
  )
  |> Map.ofSeq

let buildExecutionContext schema root operationName (document : Document) inputs =
    match Document.getOperation document operationName with
    | None -> 
        let msg = 
            if String.IsNullOrWhiteSpace(operationName) then "No operations defined"
            else sprintf "Operation not found %s" operationName

        Failure [ ExecutionError msg ]
    | Some operation ->
        {
            ExecutionContext.Schema = schema
            Fragments = 
                document
                |> Document.getFragments
                |> Seq.map (fun frag -> (frag.Name, frag))
                |> Map.ofSeq 
            RootValue = root
            Operation = operation
            VariableValues = getVariableValues schema operation.Variables inputs
            Errors =  []
        } |> Success

let execute (schema : Schema) root operationName (document : Document) inputs : ExecutionResult = 
    // TODO: validate document
    buildExecutionContext schema root operationName document inputs
    |> function
    | Success c -> executeOperation c
    | Failure errors -> {ExecutionResult.Data = GNull; Errors = errors}   
    
    
