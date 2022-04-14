module internal OpilioCraft.Lisp.StandardLib.UnaryOperators

open OpilioCraft.Lisp
open SpecialSymbols

// unary operators
let unaryQuote arg : Expression = arg

let unaryNot = function
    | LispBoolean boolValue -> not boolValue |> LispBoolean
    | List [] -> T
    | _ -> NIL

// datetime functions
let unaryYear =
    function
    | LispDate dateValue -> dateValue.Year |> LispNumeral
    | LispDateTime datetimeValue -> datetimeValue.Year |> LispNumeral
    | _ -> raise <| InvalidLispExpressionException $"Function year expects exactly one argument of type Date or DateTime"

let unaryMonth =
    function
    | LispDate dateValue -> dateValue.Month |> LispNumeral
    | LispDateTime datetimeValue -> datetimeValue.Month |> LispNumeral
    | _ -> raise <| InvalidLispExpressionException $"Function month expects exactly one argument of type Date or DateTime"

let unaryDay =
    function
    | LispDate dateValue -> dateValue.Day |> LispNumeral
    | LispDateTime datetimeValue -> datetimeValue.Day |> LispNumeral
    | _ -> raise <| InvalidLispExpressionException $"Function day expects exactly one argument of type Date or DateTime"
