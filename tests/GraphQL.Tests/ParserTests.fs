module ParserTests

open Xunit
open GraphQL
open GraphQL.AST
open FParsec

let equals expected actual  = Assert.True ((actual = expected), sprintf """
expected %+A
but got %+A""" expected actual)

let test ast str =    
  let result = Parser.parse str 
  equals ast result

let docN definitions = { Document.Definitions = definitions }    
let doc1 definition = docN [| definition |]

let namedQueryVS name variables selections = 
    {
        OperationDefinition.OperationType = Query; 
        Name = name
        Directives = Array.empty;
        VariableDefinitions = variables;
        Selections = {selections = selections};
    } |> Operation

let namedQueryV1S1 name variable selection  = 
    namedQueryVS (Some name) [| variable |] [| selection |]

let namedQueryS name = 
    namedQueryVS name Array.empty 

let namedQueryS1 name selection = 
    namedQueryS name [| selection |]

let queryS = namedQueryS None
let query1 = namedQueryS1 None

let arg name value =
    {Argument.Name = Name name; Value = value}

let fieldNA name arguments selections = 
    {
        Field.Name = Name name;
        Alias = None;
        Directives = Array.empty;
        Arguments = arguments;
        Selections = selections;
    } |> Field

let selectionSet selections =
    {selections = selections }

let someSelectionSet selections =
    {selections = selections }
    |> Some

let fieldNA1 name arguments selection =
    fieldNA name arguments ([| selection |] |> someSelectionSet)

let fieldA name arguments =
    fieldNA name arguments None

let fieldN name = 
    fieldNA name Array.empty

let field1 name selection =
    fieldN name (Some {selections = [| selection |]})

let field name =
    fieldN name None

let withAlias alias x =
    match x with
    | Field f -> Field {f with Alias = Some (Name alias)}
    | _ -> x

let frag name ``type`` selections = 
    {
        FragmentDefinition.Name = Name name
        TypeCondition = ``type``
        Directives = Array.empty
        Selections = {selections = selections}
    }
    |> Fragment

let frag1 name ``type`` selection = 
    frag name ``type`` [| selection |]

let ifrag ``type`` selections = 
    {
        InlineFragment.TypeCondition = ``type``
        Directives = Array.empty
        Selections = {selections = selections}
    }
    |> Selection.InlineFragment

let ifrag1 ``type`` selection = 
    ifrag ``type`` [| selection |]


let spreadN name directives =
    {FragmentSpread.Name = name; Directives = directives}
    |> Selection.FragmentSpread

let spread1 name directive =
    spreadN (Name name) [| directive |]

let spread name =
    spreadN (Name name) Array.empty

let var vtype defaultValue name = 
    {Variable=name; Type= vtype; DefaultValue= defaultValue}

let directive name arguments =
    {Directive.Name = name; Arguments = arguments}

let directive1 name argument =
    directive (Name name) [| argument |]

let someName = Name >> Some

[<Fact>]
let ``parser should parse empty query`` () =
  let expected = docN Array.empty
  test expected "" 
  
[<Fact>]
let ``parser should parse empty query with whitespace`` () =
  let expected = docN Array.empty
  test expected """     
  """ 

[<Fact>]
let ``parser should parse simple query with single field`` () =
  let expected = 
    field "uri"    
    |> query1 
    |> doc1     

  test expected """{uri}""" 

  
[<Fact>]
let ``parser should parse simple query with single field with whitespace`` () =
  let expected = 
    field "uri"    
    |> query1 
    |> doc1
      
  test expected """{  
    uri
  }""" 
  
[<Fact>]
let ``parser should parse simple fields`` () =
  let expected = 
    [|"uri"; "width"; "height"|]
    |> Array.map field
    |> queryS
    |> doc1  
  
  test expected """{uri,width,height}"""

[<Fact>]
let ``parser should parse simple fields with whitespace`` () =
  let expected = 
    [|"uri"; "width"; "height"|]
    |> Array.map field
    |> queryS
    |> doc1  
  
  test expected """{  
    uri,
    width,
    height
  }"""

[<Fact>]
let ``parser should parse simple fields without commas whitespace`` () =
  let expected = 
    [|"uri"; "width"; "height"|]
    |> Array.map field
    |> queryS
    |> doc1  
  
  test expected """{  
    uri
    width
    height
  }"""

[<Fact>]
let ``parser should parse nested field`` () =
  let expected = 
    [|"phone"; "name"|]
    |> Array.map field
    |> someSelectionSet
    |> fieldN "contact"
    |> query1
    |> doc1     
      
  test expected """{
    contact {
      phone,
      name
    }
  }"""   

[<Fact>]
let ``parser should parse nested field no commas`` () =
  let expected = 
    [|"phone"; "name"|]
    |> Array.map field
    |> someSelectionSet
    |> fieldN "contact"
    |> query1
    |> doc1     
      
  test expected """{
    contact {
      phone
      name
    }
  }"""   
  
[<Fact>]
let ``parser should parse nested fields`` () =
  let contact = 
    [|"phone"; "name"|]
    |> Array.map field
    |> someSelectionSet
    |> fieldN "contact"

  let expected =
    [|field "uri"; contact; field "height" |]
    |> queryS
    |> doc1    

  test expected """{  
    uri,
    contact {
      phone,
      name
    },
    height
  }"""  

[<Fact>]
let ``parser should parse nested fields no commas`` () =
  let contact = 
    [|"phone"; "name"|]
    |> Array.map field
    |> someSelectionSet
    |> fieldN "contact"

  let expected =
    [|field "uri"; contact; field "height" |]
    |> queryS
    |> doc1    

  test expected """{  
    uri
    contact {
      phone
      name
    }
    height
  }"""  

  
[<Fact>]
let ``parser should parse multi level nested fields`` () =
  let profile =
    field "uri"
    |> field1 "profile_picture"
   
  let author =
    [|field "name"; profile|]
    |> someSelectionSet
    |> fieldN "author"

  let expected =        
    [|author; field "text"|]
    |> namedQueryS ("Story" |> someName)
    |> doc1

  test expected """query Story {
        author {
          name,
          profile_picture {
            uri
          }
        },
        text
      }""" 

[<Fact>]
let ``parser should parse GraphQL with values`` () =
  let profile =
    [| field "uri"; field "width"; field "height"; |]
    |> someSelectionSet
    |> fieldNA "profilePicture" [| arg "size" (IntValue 50) |]
  
  let user =
    [| field "id"; field "name"; field "isViewerFriend"; profile |]
    |> someSelectionSet
    |> fieldNA "user" [| arg "id" (IntValue 3500401) |]

  let expected =
    user
    |> query1
    |> doc1

  test expected  """{
    user(id: 3500401) {
      id,
      name,
      isViewerFriend,
      profilePicture(size: 50)  {
        uri,
        width,
        height
      }
    }
  }""" 

[<Fact>]
let ``parser should parse query with arguments`` () = 
  let expected =
    [| field "name"; |]
    |> someSelectionSet
    |> fieldNA "user" [| arg "id" (IntValue 4) |]
    |> query1
    |> doc1

  test expected  """{
      user(id: 4) {
        name
      }
  }"""


  
[<Fact>]
let ``parser should parse query with arguments 1`` () = 
  let expected =
    [| field "id"; field "name"; fieldA "profilePic" [| arg "size" (IntValue 100) |] |]
    |> someSelectionSet
    |> fieldNA "user" [| arg "id" (IntValue 4) |]
    |> query1
    |> doc1

  test expected  """{
      user(id: 4) {
        id
        name
        profilePic(size: 100)
      }
    }"""

[<Fact>]
let ``parser should parse query with multiple arguments`` () = 
  let profilePic =
    [| arg "width" (IntValue 100); arg "height" (IntValue 50) |]
    |> fieldA "profilePic"

  let expected =
    [| field "id"; field "name"; profilePic |]
    |> someSelectionSet
    |> fieldNA "user" [| arg "id" (IntValue 4) |]
    |> query1
    |> doc1

  test expected  """{
      user(id: 4) {
        id
        name
        profilePic(width: 100, height: 50)
      }
    }"""

[<Fact>]
let ``parser should parse query with field alias`` () = 
  let profilePic alias size  =
    [| arg "size" size  |]
    |> fieldA "profilePic" 
    |> withAlias alias

  let expected =
    [| field "id"; field "name"; profilePic "smallPic" (IntValue 64); profilePic "bigPic" (IntValue 1024) |]
    |> someSelectionSet
    |> fieldNA "user" [| arg "id" (IntValue 4) |]
    |> query1
    |> doc1

  test expected  """{
      user(id: 4) {
        id
        name
        smallPic: profilePic(size: 64)
        bigPic: profilePic(size: 1024)
      }
    }"""


[<Fact>]
let ``parser should parse query with top level field alias`` () = 
  let expected =
    [| field "id"; field "name"; |]
    |> someSelectionSet
    |> fieldNA "user" [| arg "id" (IntValue 4) |]
    |> withAlias "zuck"
    |> query1
    |> doc1

  test expected  """{
      zuck: user(id: 4) {
        id
        name
      }
    }"""

[<Fact>]
let ``parser should parse query without fragments`` () = 
  let friends name =
    [| field "id"; field "name"; fieldA "profilePic" [| arg "size" (IntValue 50) |] |]
    |> someSelectionSet
    |> fieldNA name [| arg "first" (IntValue 10) |]
    
  let expected =
    [| friends "friends"; friends "mutualFriends"; |]
    |> someSelectionSet
    |> fieldNA "user" [| arg "id" (IntValue 4) |]
    |> namedQueryS1 ("noFragments" |> someName)
    |> doc1

  test expected  """query noFragments {
      user(id: 4) {
        friends(first: 10) {
          id
          name
          profilePic(size: 50)
        }
        mutualFriends(first: 10) {
          id
          name
          profilePic(size: 50)
        }
      }
    }"""

[<Fact>]
let ``parser should parse query with fragments `` () = 
  let friends name =
    spread "friendFields"
    |> fieldNA1 name [| arg "first" (IntValue 10) |]
    
  let withFragments =
    [| friends "friends"; friends "mutualFriends"; |]
    |> someSelectionSet
    |> fieldNA "user" [| arg "id" (IntValue 4) |]
    |> namedQueryS1 ("withFragments" |> someName)
    

  let friendFields =
    [| field "id"; field "name"; fieldA "profilePic" [| arg "size" (IntValue 50) |] |]
    |> frag "friendFields" "User"

  let expected = 
    [| withFragments ; friendFields |]
    |> docN

  test expected  """query withFragments {
      user(id: 4) {
        friends(first: 10) {
          ...friendFields
        }
        mutualFriends(first: 10) {
          ...friendFields
        }
      }
    }

    fragment friendFields on User {
      id
      name
      profilePic(size: 50)
    }"""

    
[<Fact>]
let ``parser should parse query with nested fragments `` () = 
  let friends name =
    spread "friendFields"
    |> fieldNA1 name [| arg "first" (IntValue 10) |]
    
  let withFragments =
    [| friends "friends"; friends "mutualFriends"; |]
    |> someSelectionSet
    |> fieldNA "user" [| arg "id" (IntValue 4) |]
    |> namedQueryS1 ("withNestedFragments" |> someName)
  
  let standardProfilePic =
    fieldA "profilePic" [| arg "size" (IntValue 50) |]
    |> frag1 "standardProfilePic" "User"

  let friendFields =
    [| field "id"; field "name"; spread "standardProfilePic" |]
    |> frag "friendFields" "User"

  let expected = 
    [| withFragments ; friendFields; standardProfilePic |]
    |> docN

  test expected """query withNestedFragments {
      user(id: 4) {
        friends(first: 10) {
          ...friendFields
        }
        mutualFriends(first: 10) {
          ...friendFields
        }
      }
    }

    fragment friendFields on User {
      id
      name
      ...standardProfilePic
    }

    fragment standardProfilePic on User {
      profilePic(size: 50)
    }"""


    
[<Fact>]
let ``parser should parse query with type conditions`` () = 
  let nestedFrag name typ fieldName =
    field "count"
    |> field1 fieldName
    |> frag1 name typ

  let userFragment =
    nestedFrag "userFragment" "User" "friends"

  let pageFragment =
    nestedFrag "pageFragment" "Page" "likers"

  let fragmentTyping =
    [| field "handle"; spread "userFragment"; spread "pageFragment"; |]
    |> someSelectionSet
    |> fieldNA "profiles" [| arg "handles" (ListValue [| StringValue "zuck"; StringValue "cocacola"|]) |]
    |> namedQueryS1 ("FragmentTyping" |> someName)

  let expected =
    [| fragmentTyping; userFragment; pageFragment |]
    |> docN

  test expected """query FragmentTyping {
      profiles(handles: ["zuck", "cocacola"]) {
        handle
        ...userFragment
        ...pageFragment
      }
    }

    fragment userFragment on User {
      friends {
        count
      }
    }

    fragment pageFragment on Page {
      likers {
        count
      }
    }"""
    
[<Fact>]
let ``parser should parse query with inline fragments`` () = 
  let nestedFrag typ fieldName =
    field "count"
    |> field1 fieldName
    |> ifrag1 typ

  let userFragment =
    nestedFrag "User" "friends"

  let pageFragment =
    nestedFrag "Page" "likers"

  let inlineFragmentTyping =
    [| field "handle"; userFragment; pageFragment; |]
    |> someSelectionSet
    |> fieldNA "profiles" [| arg "handles" (ListValue [| StringValue "zuck"; StringValue "cocacola"|]) |]
    |> namedQueryS1 ("inlineFragmentTyping" |> someName)

  let expected =
    inlineFragmentTyping
    |> doc1

  test expected """query inlineFragmentTyping {
      profiles(handles: ["zuck", "cocacola"]) {
        handle
        ... on User {
          friends {
            count
          }
        }
        ... on Page {
          likers {
            count
          }
        }
      }
    }"""
    
[<Fact>]
let ``parser should parse query with fragment directives`` () =     
  let maybeFragment =
    field "name"
    |> field1 "me"
    |> frag1 "maybeFragment" "Query"

  let v = 
    var (VariableType.NamedType "Boolean") None {Variable.Name = "condition"}

  let hasConditionalFragment =
    {Variable.Name = "condition"}
    |> Variable
    |> arg "if"
    |> directive1 "include"
    |> spread1 "maybeFragment"
    |> namedQueryV1S1 (Name "hasConditionalFragment") v
    
  let expected =
    [| hasConditionalFragment; maybeFragment |]
    |> docN

  test expected """query hasConditionalFragment($condition: Boolean) {
      ...maybeFragment @include(if: $condition)
    }

    fragment maybeFragment on Query {
      me {
        name
      }
    }
    """