module String

let nullOrEmpty = System.String.IsNullOrEmpty

let notNullOrEmpty = not << System.String.IsNullOrEmpty