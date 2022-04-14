module internal OpilioCraft.Lisp.StandardLib.BinaryOperators

open OpilioCraft.FSharp.Prelude
open OpilioCraft.Lisp

let rec private compareExpressions condition (expr1, expr2) : bool =
    match (expr1, expr2) with
    // atoms of equal type and of same value are treated as equal, otherwise not
    | Atom a, Atom b ->
        a.TryCompareTo b |> (function | Some result -> condition result | _ -> false)

    // special case: empty list is treated as false
    | Atom (FlexibleValue.Boolean a), List [] -> a.CompareTo(false) |> condition
    | List [], Atom (FlexibleValue.Boolean b) -> false.CompareTo(b) |> condition

    // two lists are equal is all elements are equal
    | List a, List b when a.Length = b.Length ->
        List.zip a b |> List.fold (fun state pair -> state = state && compareExpressions condition pair) true

    // default is false
    | _ -> false

let stdEqual (exprA, exprB)           = (exprA, exprB) |> compareExpressions (fun x -> x = 0) |> LispBoolean
let stdLower (exprA, exprB)           = (exprA, exprB) |> compareExpressions (fun x -> x < 0) |> LispBoolean
let stdLowerEqual (exprA, exprB)      = (exprA, exprB) |> compareExpressions (fun x -> x <= 0) |> LispBoolean
let stdGreater (exprA, exprB)         = (exprA, exprB) |> compareExpressions (fun x -> x > 0) |> LispBoolean
let stdGreaterEqual (exprA, exprB)    = (exprA, exprB) |> compareExpressions (fun x -> x >= 0) |> LispBoolean
