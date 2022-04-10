module internal OpilioCraft.Lisp.StandardLib.UnaryOperators

open OpilioCraft.Lisp
open SpecialSymbols

// unary operators
let unaryQuote _ arg : Expression = arg

let unaryNot _ = function
    | LispBoolean boolValue -> not boolValue |> LispBoolean
    | List [] -> T
    | _ -> NIL

// datetime functions
let unaryYear _ =
    function
    | LispDate dateValue -> dateValue.Year |> LispNumeral
    | LispDateTime datetimeValue -> datetimeValue.Year |> LispNumeral
    | _ -> raise <| InvalidArgsException

let unaryMonth _ =
    function
    | LispDate dateValue -> dateValue.Month |> LispNumeral
    | LispDateTime datetimeValue -> datetimeValue.Month |> LispNumeral
    | _ -> raise <| InvalidArgsException

let unaryDay _ =
    function
    | LispDate dateValue -> dateValue.Day |> LispNumeral
    | LispDateTime datetimeValue -> datetimeValue.Day |> LispNumeral
    | _ -> raise <| InvalidArgsException
