module internal OpilioCraft.Lisp.StandardLib.UnaryFunctions

open OpilioCraft.Lisp
open SpecialSymbols

// Note: unary functions will later be augmented to evaluate incoming expression first; no need to do it here

// unary operators
let unaryNot _ expr =
    match expr with
    | LispBoolean boolValue -> not boolValue |> LispBoolean
    | List [] -> T
    | _ -> NIL

// datetime functions
let unaryYear _ expr =
    match expr with
    | LispDate dateValue -> dateValue.Year |> LispNumeral
    | LispDateTime datetimeValue -> datetimeValue.Year |> LispNumeral
    | _ -> raise <| InvalidLispExpressionException $"Function year expects exactly one argument of type Date or DateTime"

let unaryMonth _ expr =
    match expr with
    | LispDate dateValue -> dateValue.Month |> LispNumeral
    | LispDateTime datetimeValue -> datetimeValue.Month |> LispNumeral
    | _ -> raise <| InvalidLispExpressionException $"Function month expects exactly one argument of type Date or DateTime"

let unaryDay _ expr =
    match expr with
    | LispDate dateValue -> dateValue.Day |> LispNumeral
    | LispDateTime datetimeValue -> datetimeValue.Day |> LispNumeral
    | _ -> raise <| InvalidLispExpressionException $"Function day expects exactly one argument of type Date or DateTime"
