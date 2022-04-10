module internal OpilioCraft.Lisp.StandardLib.BinaryOperators

open System

open OpilioCraft.FSharp.Prelude
open OpilioCraft.Lisp

let private compareFlexibleValues (fv1 : FlexibleValue) (fv2 : FlexibleValue) : int option =
    match (fv1, fv2) with
        | FlexibleValue.Boolean a, FlexibleValue.Boolean b -> a.CompareTo(b) |> Some
        | FlexibleValue.Numeral a, FlexibleValue.Numeral b -> a.CompareTo(b) |> Some
        | FlexibleValue.Decimal a, FlexibleValue.Decimal b -> a.CompareTo(b) |> Some
        | FlexibleValue.String a, FlexibleValue.String b -> String.Compare(a, b) |> Some
        | FlexibleValue.Date a, FlexibleValue.Date b -> a.CompareTo(b) |> Some
        | FlexibleValue.Time a, FlexibleValue.Time b -> a.CompareTo(b) |> Some
        | FlexibleValue.DateTime a, FlexibleValue.DateTime b -> a.CompareTo(b) |> Some
        | _ -> None
   
let rec private compareExpressions condition (expr1, expr2) : bool =
    match (expr1, expr2) with
    // atoms of equal type and of same value are treated as equal, otherwise not
    | Atom a, Atom b ->
        compareFlexibleValues a b |> (function | Some result -> condition result | _ -> false)

    // special case: empty list is treated as false
    | Atom (FlexibleValue.Boolean a), List [] -> a.CompareTo(false) |> condition
    | List [], Atom (FlexibleValue.Boolean b) -> false.CompareTo(b) |> condition

    // two lists are equal is all elements are equal
    | List a, List b when a.Length = b.Length ->
        List.zip a b |> List.fold (fun state pair -> state = state && compareExpressions condition pair) true

    // default is false
    | _ -> false

let stdEqual _ (exprA, exprB)           = (exprA, exprB) |> compareExpressions (fun x -> x = 0) |> LispBoolean
let stdLower _ (exprA, exprB)           = (exprA, exprB) |> compareExpressions (fun x -> x < 0) |> LispBoolean
let stdLowerEqual _ (exprA, exprB)      = (exprA, exprB) |> compareExpressions (fun x -> x <= 0) |> LispBoolean
let stdGreater _ (exprA, exprB)         = (exprA, exprB) |> compareExpressions (fun x -> x > 0) |> LispBoolean
let stdGreaterEqual _ (exprA, exprB)    = (exprA, exprB) |> compareExpressions (fun x -> x >= 0) |> LispBoolean
