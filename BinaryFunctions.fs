module internal OpilioCraft.Lisp.StandardLib.BinaryFunctions

open System.Text.RegularExpressions

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

let binaryEqual        = (compareExpressions (fun x -> x = 0))  >> LispBoolean
let binaryLower        = (compareExpressions (fun x -> x < 0))  >> LispBoolean
let binaryLowerEqual   = (compareExpressions (fun x -> x <= 0)) >> LispBoolean
let binaryGreater      = (compareExpressions (fun x -> x > 0))  >> LispBoolean
let binaryGreaterEqual = (compareExpressions (fun x -> x >= 0)) >> LispBoolean


let private matchFunction funcName funcImpl (exprA, exprB) : bool =
    match (exprA, exprB) with
    | Atom (FlexibleValue.String value) , Atom (FlexibleValue.String pattern) -> funcImpl value pattern
    | _ , Atom (FlexibleValue.String pattern) -> false // other expressions than string atoms lead to false
    | _ -> raise <| InvalidLispExpressionException $"{funcName} expects a string pattern as second argument"

let binaryContains     = (matchFunction "contains" (fun value pattern -> value.Contains(pattern))) >> LispBoolean
let binaryMatchesRegex = (matchFunction "matches" (fun value pattern -> Regex.IsMatch(value, pattern))) >> LispBoolean


let binaryAdd (exprA, exprB) : Expression =
    match (exprA, exprB) with
    | Atom (FlexibleValue.Numeral a), Atom (FlexibleValue.Numeral b) -> (a + b) |> LispNumeral
    | Atom (FlexibleValue.Decimal a), Atom (FlexibleValue.Decimal b) -> (a + b) |> LispDecimal
    | Atom (FlexibleValue.Numeral a), Atom (FlexibleValue.Decimal b) -> ((decimal a) + b) |> LispDecimal
    | Atom (FlexibleValue.Decimal a), Atom (FlexibleValue.Numeral b) -> (a + (decimal b)) |> LispDecimal
    | _ -> raise <| InvalidLispExpressionException $"'+' supports only numeral or decimal values"

let binarySubtract (exprA, exprB) : Expression =
    match (exprA, exprB) with
    | Atom (FlexibleValue.Numeral a), Atom (FlexibleValue.Numeral b) -> (a - b) |> LispNumeral
    | Atom (FlexibleValue.Decimal a), Atom (FlexibleValue.Decimal b) -> (a - b) |> LispDecimal
    | Atom (FlexibleValue.Numeral a), Atom (FlexibleValue.Decimal b) -> ((decimal a) - b) |> LispDecimal
    | Atom (FlexibleValue.Decimal a), Atom (FlexibleValue.Numeral b) -> (a - (decimal b)) |> LispDecimal
    | _ -> raise <| InvalidLispExpressionException $"'-' supports only numeral or decimal values"

let binaryMultiply (exprA, exprB) : Expression =
    match (exprA, exprB) with
    | Atom (FlexibleValue.Numeral a), Atom (FlexibleValue.Numeral b) -> (a * b) |> LispNumeral
    | Atom (FlexibleValue.Decimal a), Atom (FlexibleValue.Decimal b) -> (a * b) |> LispDecimal
    | Atom (FlexibleValue.Numeral a), Atom (FlexibleValue.Decimal b) -> ((decimal a) * b) |> LispDecimal
    | Atom (FlexibleValue.Decimal a), Atom (FlexibleValue.Numeral b) -> (a * (decimal b)) |> LispDecimal
    | _ -> raise <| InvalidLispExpressionException $"'*' supports only numeral or decimal values"

let binaryDivide (exprA, exprB) : Expression =
    match (exprA, exprB) with
    | _, Atom (FlexibleValue.Numeral b) when b = 0 ->
        raise <| InvalidLispExpressionException $"division by zero is not defined"
    | _, Atom (FlexibleValue.Decimal b) when b = 0m ->
        raise <| InvalidLispExpressionException $"division by zero is not defined"

    | Atom (FlexibleValue.Numeral a), Atom (FlexibleValue.Numeral b) -> (a / b) |> LispNumeral
    | Atom (FlexibleValue.Decimal a), Atom (FlexibleValue.Decimal b) -> (a / b) |> LispDecimal
    | Atom (FlexibleValue.Numeral a), Atom (FlexibleValue.Decimal b) -> ((decimal a) / b) |> LispDecimal
    | Atom (FlexibleValue.Decimal a), Atom (FlexibleValue.Numeral b) -> (a / (decimal b)) |> LispDecimal

    | _ -> raise <| InvalidLispExpressionException $"'/' supports only numeral or decimal values"
