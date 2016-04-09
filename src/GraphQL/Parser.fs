[<RequireQualifiedAccess>]
module GraphQL.Parser

open FParsec
open AST
open System
open System.Globalization

type ParserOptions = {Trace: bool}

let parseWithOptions options query = 
    let someOrEmpty s =
        match s with
        | Some str -> str
        | None -> ""
    
    let BP (p: Parser<_,_>) stream =
        p stream // set a breakpoint here

    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
        if options.Trace = false then
            p
        else
            fun stream ->
                printfn "%A: Entering %s" stream.Position label
                let reply = p stream
                printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
                reply

    let pSelection, pSelectionRef = createParserForwardedToRef<_, unit>()
    let pField, pFieldRef = createParserForwardedToRef<_, unit>()
    let pSelectionSet, pSelectionSetRef = createParserForwardedToRef<SelectionSet, unit>()
    let pType, pTypeRef = createParserForwardedToRef<VariableType, unit>()
    let pValue, pValueRef = createParserForwardedToRef<_, unit>()

    let ``???`` = pzero

    let oneOf str =
        str
        |> Seq.map (fun x -> pstring (x.ToString()))
        |> choice        

    let arrayOrEmptyArray optArray =
        match optArray with 
        | None -> Array.empty
        | Some (list : list<_>) -> list |> Array.ofList

    // Section 2.1 Source text
    let SourceCharacter = anyChar

    // 2.1.1 White Space
    let WhiteSpace =
        regex "[\u0009\u000B\u000C\u0020\u00A0]"
        <?> "WhiteSpace"

    // 2.1.2 Line Terminators
    let lineTerminatorChars = "\u000A\u000D\u2028\u2029"
    let LineTerminator =    
        regex "[\u000A\u000D\u2028\u2029]"
        <?> "NewLine"

    // 2.1.3 Comments
    let pCommentChar =
        //SourceCharacter but not LineTerminator        
        noneOf lineTerminatorChars

    let pComment =
        pchar '#' 
        >>. many pCommentChar 
        |>> fun x -> x |> string

    // 2.1.4 Insignificant Commas
    let pComma = pstring ","

    
    // 2.1.6 Ignored Tokens
    let pIgnored =
        many <| choice [
            WhiteSpace;
            LineTerminator;
            pComment;
            pComma;
        ] 
        <?> "Ignored"
        <!> "Ignored"

    let pIgnored1 =
        many1 <| choice [
            WhiteSpace;
            LineTerminator;
            pComment;
            pComma;
        ] 
        <?> "Ignored1"
        <!> "Ignored1"

    let sepByIgnored p = 
        many (p .>> pIgnored) .>> pIgnored

    let ws p = p .>> pIgnored
    let wsstr s = pstring s .>> pIgnored
    //    // 2.1.7 Punctuators
    //    let pPunctuator =
    //        choice [
    //            pstring "!"; pstring "$"; pstring "("; pstring ")";
    //            pstring "..."; pstring ":"; pstring "="; pstring "@";
    //            pstring "["; pstring "]"; pstring "{"; pstring "}";
    //        ] 
        

    // 2.1.8 Names
    let pName = 
        regex "[_A-Za-z][_0-9A-Za-z]*" 
        <?> "Name"
        <!> "Name"

    // 2.2.7.1 Int Value
    let pDigit = regex "[0-9]"
    let pDigits = regex "[0-9]*"
    let pIntPart =
        let negSign = pstring "-"
        let nonZeroDigit = anyOf "123456789" |>> fun x -> x.ToString()        

        let zero = opt negSign >>. pstring "0"
        let nonZero = 
            opt negSign .>>. nonZeroDigit .>>. pDigits
            |>> fun ((oneg, d1), drest) ->
                someOrEmpty oneg + d1 + drest

        zero <|> nonZero

    let pIntValue = 
        pIntPart 
        |>> fun x -> Int32.Parse(x)

    // 2.2.7.2 Float Value
    let pFloatValue = 
        let FractionalPart = 
            pstring "." >>. many pDigit |>> fun x -> "." + string(x)
        
        let ExponentPart = 
            let sign = anyOf "-+"
            let ExponentIndicator = anyOf "eE"

            ExponentIndicator >>. opt sign .>>. many pDigit 
            |>> fun (osign , digits) -> 
                    let s = match osign with None -> "" | Some some -> some.ToString()
                    s + string(digits)

        pIntPart .>>. opt FractionalPart .>>. opt ExponentPart
        |>> fun ((a, b), c) -> float(a + (someOrEmpty b) + (someOrEmpty c))
    
    // 2.2.7.3 Bool Value
    let pBooleanValue = 
        (wsstr "true" >>% true) 
        <|>
        (wsstr "false" >>% false)

    // 2.2.7.4 String Value
    let pStringValue = 
        let escapeChar =
            let pEscapedUnicode = 
                regex "[0-9A-Fa-f]{4}"
                |>> fun x -> char(Int32.Parse(x, NumberStyles.HexNumber))

            let pEscapedCharacter = 
                anyOf "\"\\/bfnrt"
                |>> fun x -> x
            
            let encoded = 
                choice [
                    pstring "u" >>. pEscapedUnicode
                    pEscapedCharacter
                ]
            pstring "\\" >>. encoded

        let sourceChar = satisfy <| fun c -> 
                match c with 
                | '\"' | '\\' | '\u000A' | '\u000D' | '\u2028' | '\u2029' -> false
                | _ -> true

        let pStringCharacter = sourceChar <|> escapeChar        
        let pQuote = pchar '"'

        between pQuote pQuote (manyChars pStringCharacter)
        <?> "String"

    // 2.2.8 Variables
    let pVariable =
        pstring "$" >>. ws pName 
        |>> fun (name) -> {Variable.Name = name}
        <?> "Variable"
        <!> "Variable"

    let pEnumValue = pName
    let pListValue =
        wsstr "[" >>. (sepByIgnored pValue) .>> wsstr "]"
        |>> Array.ofList

    let pObjectValue = ``???``

    // 2.2.7 Input Values
    let objcast n = n :> Object
    do pValueRef := 
        choice [
            pVariable       |>> Variable
            pIntValue       |>> IntValue
            pFloatValue     |>> FloatValue
            pStringValue    |>> StringValue
            pBooleanValue   |>> BooleanValue
            pEnumValue      |>> EnumValue
            pListValue      |>> ListValue
            pObjectValue    |>> ObjectValue
        ] <?> "Value"

    //    // 2.1.5 Lexical Tokens
    //    let Token =
    //        choice [
    //            pPunctuator;
    //            pName;
    //            pIntValue;
    //            pFloatValue;
    //            pStringValue;
    //        ]
    //        
        
    // 2.2.5 Field Alias
    let pAlias =
        ws pName .>> wsstr ":"
        |>> Name
        <?> "Alias"
        <!> "Alias"


    // 2.2.4 Arguments
    let pArgument = 
        ws pName .>> wsstr ":" .>>. pValue
        |>> fun (name, value) -> {Argument.Name = Name name; Value = value}
        <?> "Argument"
        <!> "Argument"

    let pArguments = wsstr "(" >>.  sepByIgnored pArgument .>> wsstr ")"


    // 2.2.10 Directives
    let pDirective =
        pstring "@" >>. pName .>>. opt pArguments
        |>> fun (name, args) -> 
            {
                Directive.Name = Name name;
                Arguments = arrayOrEmptyArray args
            }
                

    let pDirectives = 
        many pDirective
        |>> Array.ofList


    // 2.2.6 Fragments
    let pFragmentName = 
        pName
    
    
    let pFragmentSpreadTail = 
        ws pFragmentName .>>. pDirectives
        <?> "FragmentSpread"
        <!> "FragmentSpread"
        |>> fun (name, directives) ->
                {
                    FragmentSpread.Name = Name name
                    Directives = directives
                }
    //
    //    let pFragmentSpread = 
    //        pstring "..." >>. pFragmentSpreadTail



    // 2.2.9 Input Types    

    let pNamedType = 
        pName 
        |>> fun (name) -> NamedType name

    let pListType = 
        between (pstring "[")  (pstring "]") pType
        |>> ListType

    let pNonNullType = 
        choice [
            pNamedType .>> pstring "!"
            pListType .>> pstring "!"
        ]
        |>> NonNullType

    do pTypeRef := 
        choice [
            pNamedType
            pListType
            pNonNullType
        ]


    // 2.2.6.1 Type Conditions
    let pTypeCondition = 
        //pNamedType 
        pName //dont convert to variabletype right now

    // 2.2.6.2 Inline Fragments
    let pInlineFragmentTail =
        let func typeCond directives selSet =
            {
                InlineFragment.TypeCondition = typeCond
                Directives = directives
                Selections = selSet
            }

        pipe3 (wsstr "on" >>. ws pTypeCondition) pDirectives pSelectionSet func
        <?> "InlineFragment"
        <!> "InlineFragment"

    //    let pInlineFragment = 
    //        pstring "..." >>. pInlineFragmentTail

    let pSelectionFragment =
        let fragmentTail = 
            (pInlineFragmentTail |>> Selection.InlineFragment)            
            <|>
            (pFragmentSpreadTail |>> Selection.FragmentSpread)

        wsstr "..." >>. fragmentTail

    // 2.2.2 Selection Sets    
    do pSelectionRef := 
        choice [|
            (pField |>> Field) <!> "Field"
            pSelectionFragment
        |]
        <?> "Selection"
        <!> "Selection"

    do pSelectionSetRef :=
        between (wsstr "{") (wsstr "}") (sepByIgnored pSelection)
        |>> fun (selections) -> {selections = selections |> Array.ofList}
        <?> "SelectionSet"
        <!> "SelectionSet"


    // 2.2.3 Fields
    do pFieldRef := 
        let func oalias name oargs directives oselection = 
            {
                Field.Alias = oalias
                Name = Name name
                Arguments = arrayOrEmptyArray oargs
                Directives = directives 
                Selections = oselection
            }
        pipe5 (opt (attempt pAlias)) (ws pName) (opt pArguments) pDirectives (opt pSelectionSet) func
        <?> "Field"
        <!> "Field"



    // 2.2.8 Variables
    let pDefaultValue = wsstr "=" >>. pValue

    let pVariableDefinition = 
        let func varName varType oDefVal =
            {
                VariableDefinition.Variable = varName
                Type = varType
                DefaultValue = oDefVal
            }
        pipe3 (pVariable .>> wsstr ":") pType (opt pDefaultValue) func
        <?> "Variable Definition"
        

    let pVariableDefinitions =
        between (wsstr "(") (wsstr ")") (many pVariableDefinition)
        |>> Array.ofList


    // 2.2.1 Operations
    let pOperationType =
        (wsstr "query" >>% Query)
        <|> (wsstr "mutation" >>% Mutation)

    let pOperationDefinition1 =
        pipe5 pOperationType (ws pName) (opt pVariableDefinitions) pDirectives pSelectionSet
        <| fun otype name ovars directives selection ->
            {
                OperationDefinition.OperationType = otype
                Name = Some (Name name)
                VariableDefinitions = match ovars with None -> Array.empty | Some vars -> vars
                Directives = directives
                Selections = selection
            }

    let pOperationDefinition2 =
        pSelectionSet
        |>> fun x ->
            {
                OperationDefinition.OperationType = Query
                Name = None
                VariableDefinitions = Array.empty
                Directives = Array.empty
                Selections = x
            }
        

    let pOperationDefinition = 
        pOperationDefinition1
        <|>
        pOperationDefinition2
        <?> "OperationDefinition"
        <!> "OperationDefinition"
            

    // 2.2.6 Fragments
    let pFragmentDefinition = 
        pipe4 (wsstr "fragment" >>. ws pFragmentName .>> wsstr "on") (ws pTypeCondition) pDirectives pSelectionSet
        <| fun name typeCond directives selSet ->
                {
                    FragmentDefinition.Name = Name name
                    TypeCondition = typeCond
                    Directives = directives
                    Selections = selSet
                }
        <?> "FragmentDefinition"
        <!> "FragmentDefinition"
        

    // 2.2 Query Document
    let pDefinition =
        (pOperationDefinition |>> Operation)
        <|> (pFragmentDefinition |>> Fragment)

    let pDocument =
        many pDefinition
        |>> fun x -> 
            {
                Document.Definitions = (x |> Array.ofList)
            }    

    let gql = ws pDocument .>> eof
    match run gql query with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> raise (System.FormatException(errorMsg))

let parse (x) = parseWithOptions {Trace = false} x