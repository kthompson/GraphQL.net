module ParserTests

open Xunit
open GraphQL
open FParsec

let equals expected actual  = Assert.True ((actual = expected), sprintf """
expected %+A
but got %+A""" expected actual)

let test ast str =    
  let parser = Parser ()  
  let result = parser.Parse str 
  equals ast result

let docN definitions = { Document.Definitions = definitions }    
let doc1 definition = docN [| definition |]

let namedQueryVS name variables selections = 
    {
        Operation.OperationType = Query; 
        Name = name 
        Directives = Array.empty;
        Variables = variables;
        Selections = selections;
    } |> Operation

let namedQueryV1S1 name variable selection  = 
    namedQueryVS name [| variable |] [| selection |]

let namedQueryS name = 
    namedQueryVS name Array.empty 

let namedQueryS1 name selection = 
    namedQueryS name [| selection |]

let queryS = namedQueryS null
let query1 = namedQueryS1 null

let arg name value =
    {Argument.Name = name; Value = value}

let fieldNA name arguments selections = 
    {
        Field.Name = name;
        Alias = null;
        Directives = Array.empty;
        Arguments = arguments;
        Selections = selections;
    } |> Field

let fieldNA1 name arguments selection =
    fieldNA name arguments [| selection |]
let fieldA name arguments =
    fieldNA name arguments Array.empty

let fieldN name = 
    fieldNA name Array.empty

let field1 name selection =
    fieldN name [| selection |]

let field name =
    fieldN name Array.empty

let withAlias alias x =
    match x with
    | Field f -> Field {f with Alias = alias}
    | _ -> x

let frag name ``type`` selections = 
    {
        FragmentDefinition.Name = name
        Type = ``type``
        Directives = Array.empty
        Selections = selections
    }
    |> Fragment

let frag1 name ``type`` selection = 
    frag name ``type`` [| selection |]

let ifrag ``type`` selections = 
    {
        InlineFragment.Type = ``type``
        Directives = Array.empty
        Selections = selections
    }
    |> Selection.InlineFragment

let ifrag1 ``type`` selection = 
    ifrag ``type`` [| selection |]


let spreadN name directives =
    {FragmentSpread.Name = name; Directives = directives}
    |> Selection.FragmentSpread

let spread1 name directive =
    spreadN name [| directive |]

let spread name =
    spreadN name Array.empty

let varType list nullable name =
    {Name = name; IsList = list; AllowsNull = nullable}

let valueType = varType false false
let refType = varType false true
let listType = varType true false

let var vtype defaultValue value name = 
    {Name=name; Type= vtype; DefaultValue= defaultValue; Value = value}

let directive name arguments =
    {Directive.Name = name; Arguments = arguments}

let directive1 name argument =
    directive name [| argument |]

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
    |> fieldN "author"

  let expected =        
    [|author; field "text"|]
    |> namedQueryS "Story"
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
let ``parser should parse GraphQL`` () =
  let profile =
    [| field "uri"; field "width"; field "height"; |]
    |> fieldNA "profilePicture" [| arg "size" 50 |]
  
  let user =
    [| field "id"; field "name"; field "isViewerFriend"; profile |]
    |> fieldNA "user" [| arg "id" 3500401 |]

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
    |> fieldNA "user" [| arg "id" 4 |]
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
    [| field "id"; field "name"; fieldA "profilePic" [| arg "size" 100 |] |]
    |> fieldNA "user" [| arg "id" 4 |]
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
    [| arg "width" 100; arg "height" 50 |]
    |> fieldA "profilePic"

  let expected =
    [| field "id"; field "name"; profilePic |]
    |> fieldNA "user" [| arg "id" 4 |]
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
    [| field "id"; field "name"; profilePic "smallPic" 64; profilePic "bigPic" 1024 |]
    |> fieldNA "user" [| arg "id" 4 |]
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
    |> fieldNA "user" [| arg "id" 4 |]
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
    [| field "id"; field "name"; fieldA "profilePic" [| arg "size" 50 |] |]
    |> fieldNA name [| arg "first" 10 |]
    
  let expected =
    [| friends "friends"; friends "mutualFriends"; |]
    |> fieldNA "user" [| arg "id" 4 |]
    |> namedQueryS1 "noFragments"
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
    |> fieldNA1 name [| arg "first" 10 |]
    
  let withFragments =
    [| friends "friends"; friends "mutualFriends"; |]
    |> fieldNA "user" [| arg "id" 4 |]
    |> namedQueryS1 "withFragments"
    

  let friendFields =
    [| field "id"; field "name"; fieldA "profilePic" [| arg "size" 50 |] |]
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
    |> fieldNA1 name [| arg "first" 10 |]
    
  let withFragments =
    [| friends "friends"; friends "mutualFriends"; |]
    |> fieldNA "user" [| arg "id" 4 |]
    |> namedQueryS1 "withNestedFragments"
  
  let standardProfilePic =
    fieldA "profilePic" [| arg "size" 50 |]
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
    |> fieldNA "profiles" [| arg "handles" [|"zuck"; "cocacola"|] |]
    |> namedQueryS1 "FragmentTyping"    

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
    |> fieldNA "profiles" [| arg "handles" [|"zuck"; "cocacola"|] |]
    |> namedQueryS1 "inlineFragmentTyping"    

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

  let v =  var (refType "Boolean") null null "condition"

  let hasConditionalFragment =
    arg "if" "condition"
    |> directive1 "include"
    |> spread1 "maybeFragment"
    |> namedQueryV1S1 "hasConditionalFragment" v
    
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