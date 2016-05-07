module Rop


type SuccessFailure<'a,'b> = 
| Success of 'a
| Failure of 'b

let bind f = function
| Success x -> f x
| Failure e -> Failure e 

let bindAsync f = function
| Success x -> f x
| Failure e -> async { return Failure e }

let bindF f = function
| Success x -> Success x
| Failure e -> f e

let map f = function
| Success x -> Success (f x)
| Failure e -> Failure e 

let mapF f = function
| Success x -> Success x
| Failure e -> Failure (f e)

let orElse errValue = function
| Success x -> x
| Failure _ -> errValue

let unwrap = function
| Success x -> x
| Failure x -> x
