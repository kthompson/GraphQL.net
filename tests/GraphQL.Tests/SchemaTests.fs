module SchemaTests

open System
open Xunit
open FsUnit.Xunit

open GraphQL

open Schema
open Scalars
open Execution
open Builders
open Parser

type Image = {url: string; width:int; height:int}
type Article<'TAuthor> = {
    id : string;
    isPublished : bool;
    title : string;
    body : string;
    hidden : string;
    keywords : string list;
    author: 'TAuthor option
}
type Author = {id: string; name:string; recentArticle: Article<Author> option; pic: int -> int -> Image}

let getPic id width height = {url= sprintf "cdn://%d" id; width=width; height=height}
let articleWithUser author id =
    {
        id = id
        isPublished = true
        title = "My Article " + string(id)
        body =  "This is a post"
        hidden = "This data is not exposed in the schema"
        keywords = [ "foo"; "bar"; null ]
        author = author
      }

let johnSmith =
    {
      id = "123"
      name = "John Smith"
      pic = getPic 123
      recentArticle = articleWithUser None "1" |> Some
    };

let article = articleWithUser (Some johnSmith)

let articles =
    seq { 1..10 }
    |> Seq.map string
    |> Seq.map article
    |> List.ofSeq

let BlogImage = objectType {
    name "Image"
    fields (
        Map.empty
        |> Map.add "url" (field { string })
        |> Map.add "width" (field { int })
        |> Map.add "height" (field { int })
    )
}

let blogArticle authorType = objectType {
    name "Article"
    fields (
        Map.empty
        |> Map.add "id" (field { typ (StringType |> OutputType.Scalar |> OutputType.NonNull) })
        |> Map.add "isPublished" (field { typ (Scalars.BoolType |> OutputType.Scalar |> OutputType.NonNull) })
        |> Map.add "author" (field { obj authorType })
        |> Map.add "title" (field { string })
        |> Map.add "body" (field { string })
        |> Map.add "keywords" (field { list (StringType |> OutputType.Scalar) })
    )
}

let BlogAuthor = objectType {
    name "Author"
    fields (
        Map.empty
        |> Map.add "id" (field { string })
        |> Map.add "name" (field { string })
        //TODO figure out how to fix this... |> Map.add "recentArticle" (field { obj (blogArticle BlogAuthor) })
        |> Map.add "pic" (
            field {
                obj BlogImage
                args (
                    [
                        argument {
                            name "width" 
                            int
                        }
                        argument {
                            name "height" 
                            int
                        }
                    ]
                )
                resolve (fun obj args c d ->
                    let w = args.["width"] :?> int
                    let h = args.["height"] :?> int
                    match obj with
                    | :? Author as author -> author.pic w h :> Object |> Some
                    | _ -> None
                )
            })
    )    
}

let BlogArticle = blogArticle BlogAuthor

let BlogQuery = objectType {
    name "Query"   
    fields (
        Map.empty
        |> Map.add "article" (field { 
            obj BlogArticle
            args [
                argument { name "id"; id }
            ]
            resolve (fun obj args c d -> 
                let id = args.["id"] :?> string
                article id :> Object |> Some
            )
            })
        |> Map.add "feed" (field {
            list (BlogArticle |> OutputType.Object)
            resolve (fun obj args c d ->
                [
                    article "1"
                    article "2"
                    article "3"
                    article "4"
                    article "5"
                    article "6"
                    article "7"
                    article "8"
                    article "9"
                    article "10"
                ] :> Object |> Some
            )
        })
    )
}

let BlogSchema = schema {
    query BlogQuery
}

//let check testType testData expected = 
//    async {
//        let data = Map.empty.Add("test", testData)
//
//        let nestType =
//            let fields = 
//                Map.empty.Add("test", new FieldDefinition(testType))
//            new ObjectType("NestedType", fields = fields)
//
//        let dataType =
//            let fields =            
//                Map.empty
//                   .Add("next", new FieldDefinition(nestType, resolve = fun a b c d -> testData))
//
//            new ObjectType("DataType", fields = fields)
//        
//        let addType (t : ObjectType)  =
//            SchemaType.Object t
//            |> Map.add t.Name 
//
//        let typeMap = 
//            Map.empty
//            |> addType nestType
//            |> addType dataType
//
//        let schema = new Schema(typeMap, query = dataType)
//
//        let ast = Parser.parse "{ nest { test } }"
//
//        let! response = execute schema ast data;
//
//        Assert.Equal(expected, response)
//    }

    
[<Fact>]
let ``Execute: Handles execution with a complex schema`` () =

    let request = @"
      {
        feed {
          id,
          title
        },
        article(id: ""1"") {
          ...articleFields,
          author {
            id,
            name,
            pic(width: 640, height: 480) {
              url,
              width,
              height
            },
            recentArticle {
              ...articleFields,
              keywords
            }
          }
        }
      }

      fragment articleFields on Article {
        id,
        isPublished,
        title,
        body,
        hidden,
        notdefined
      }"
    
    let result = 
        let document = parse request
        
        Execution.execute BlogSchema None null document Map.empty
        //|> Async.RunSynchronously

    let expected = 
        let data = 
            let feed = 
                let garticle id = 
                    Map.empty
                        .Add("id", GNumber id)
                        .Add("title", sprintf "My Article %f" id |> GString)
                    |> GObject

                [
                    garticle 1.0
                    garticle 2.0
                    garticle 3.0
                    garticle 4.0
                    garticle 5.0
                    garticle 6.0
                    garticle 7.0
                    garticle 8.0
                    garticle 9.0
                    garticle 10.0
                ]
                |> GList
            let article = 
                let author = 
                    let pic = 
                        Map.empty
                           .Add("url", GString "cdn://123")
                           .Add("width", GNumber 640.0)
                           .Add("height", GNumber 480.0)
                        |> GObject

                    let recentArticle = 
                        Map.empty
                           .Add("id", GString "1")
                           .Add("isPublished", GBool true)
                           .Add("title", GString "My Article 1")
                           .Add("body", GString "This is a post")
                           .Add("keywords", GList [ GString "foo"; GString "bar"; GString "1"; GString "true"; GNull ])
                        |> GObject

                    Map.empty
                       .Add("id", GNumber 123.0)
                       .Add("name", GString "John Smith")
                       .Add("pic", pic)
                       .Add("recentArticle", recentArticle)               
                    |> GObject
                Map.empty
                   .Add("id", GNumber 1.0)
                   .Add("isPublished", GBool true)
                   .Add("title", GString "My Article 1")
                   .Add("body", GString "This is a post")
                   .Add("author", author)               
                |> GObject
            Map.empty
               .Add("feed", feed)
               .Add("article", article)
            |> GObject        

        Map.empty
        |> Map.add "data" data
        |> GObject
//        article: {
//          author: {
//            id: '123',
//            name: 'John Smith',
//            pic: {
//              url: 'cdn://123',
//              width: 640,
//              height: 480
//            },
//            recentArticle: {
//              id: '1',
//              isPublished: true,
//              title: 'My Article 1',
//              body: 'This is a post',
//              keywords: [ 'foo', 'bar', '1', 'true', null ]
//            }
//          }
//        }

    result.Data |> should equal expected
    //let expected = docN Array.empty
    //test expected "" 
    ()
  