module internal OpilioCraft.Lisp.StandardLib.BinaryFunctions

open System.Text.RegularExpressions

open OpilioCraft.FSharp.Prelude
open OpilioCraft.Lisp

open EvalHelper

// ------------------------------------------------------------------------------------------------
// canonical comparisons

let rec private compareExpressions condition : (Expression * Expression -> bool) = function // result has to be bool to facilitate recursion
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

let binaryEqual env        = evalExpressionTupel env >> (compareExpressions (fun x -> x = 0))  >> LispBoolean
let binaryLower env        = evalExpressionTupel env >> (compareExpressions (fun x -> x < 0))  >> LispBoolean
let binaryLowerEqual env   = evalExpressionTupel env >> (compareExpressions (fun x -> x <= 0)) >> LispBoolean
let binaryGreater env      = evalExpressionTupel env >> (compareExpressions (fun x -> x > 0))  >> LispBoolean
let binaryGreaterEqual env = evalExpressionTupel env >> (compareExpressions (fun x -> x >= 0)) >> LispBoolean

// ------------------------------------------------------------------------------------------------
// match-based comparisons

let private matchFunction funcName funcImpl : (Expression * Expression -> bool) = function
    | Atom (FlexibleValue.String value) , Atom (FlexibleValue.String pattern) -> funcImpl value pattern
    | _ , Atom (FlexibleValue.String pattern) -> false // other expressions than string atoms lead to false
    | _ -> raise <| InvalidLispExpressionException $"{funcName} expects a string pattern as second argument"

let binaryContains env     = evalExpressionTupel env >> (matchFunction "contains" (fun value pattern -> value.Contains(pattern))) >> LispBoolean
let binaryMatchesRegex env = evalExpressionTupel env >> (matchFunction "matches" (fun value pattern -> Regex.IsMatch(value, pattern))) >> LispBoolean

// ------------------------------------------------------------------------------------------------
// arithmetic functions

let binaryAdd env =
    evalExpressionTupel env
    >> function
        | Atom (FlexibleValue.Numeral a), Atom (FlexibleValue.Numeral b) -> (a + b) |> LispNumeral
        | Atom (FlexibleValue.Decimal a), Atom (FlexibleValue.Decimal b) -> (a + b) |> LispDecimal
        | Atom (FlexibleValue.Numeral a), Atom (FlexibleValue.Decimal b) -> ((decimal a) + b) |> LispDecimal
        | Atom (FlexibleValue.Decimal a), Atom (FlexibleValue.Numeral b) -> (a + (decimal b)) |> LispDecimal
        | _ -> raise <| InvalidLispExpressionException $"'+' supports only numeral or decimal values"

let binarySubtract env =
    evalExpressionTupel env
    >> function
        | Atom (FlexibleValue.Numeral a), Atom (FlexibleValue.Numeral b) -> (a - b) |> LispNumeral
        | Atom (FlexibleValue.Decimal a), Atom (FlexibleValue.Decimal b) -> (a - b) |> LispDecimal
        | Atom (FlexibleValue.Numeral a), Atom (FlexibleValue.Decimal b) -> ((decimal a) - b) |> LispDecimal
        | Atom (FlexibleValue.Decimal a), Atom (FlexibleValue.Numeral b) -> (a - (decimal b)) |> LispDecimal
        | _ -> raise <| InvalidLispExpressionException $"'-' supports only numeral or decimal values"

let binaryMultiply env =
    evalExpressionTupel env
    >> function
        | Atom (FlexibleValue.Numeral a), Atom (FlexibleValue.Numeral b) -> (a * b) |> LispNumeral
        | Atom (FlexibleValue.Decimal a), Atom (FlexibleValue.Decimal b) -> (a * b) |> LispDecimal
        | Atom (FlexibleValue.Numeral a), Atom (FlexibleValue.Decimal b) -> ((decimal a) * b) |> LispDecimal
        | Atom (FlexibleValue.Decimal a), Atom (FlexibleValue.Numeral b) -> (a * (decimal b)) |> LispDecimal
        | _ -> raise <| InvalidLispExpressionException $"'*' supports only numeral or decimal values"

let binaryDivide env =
    evalExpressionTupel env 
    >> function
        | _, Atom (FlexibleValue.Numeral b) when b = 0 ->
            raise <| InvalidLispExpressionException $"division by zero is not defined"
        | _, Atom (FlexibleValue.Decimal b) when b = 0m ->
            raise <| InvalidLispExpressionException $"division by zero is not defined"

        | Atom (FlexibleValue.Numeral a), Atom (FlexibleValue.Numeral b) -> (a / b) |> LispNumeral
        | Atom (FlexibleValue.Decimal a), Atom (FlexibleValue.Decimal b) -> (a / b) |> LispDecimal
        | Atom (FlexibleValue.Numeral a), Atom (FlexibleValue.Decimal b) -> ((decimal a) / b) |> LispDecimal
        | Atom (FlexibleValue.Decimal a), Atom (FlexibleValue.Numeral b) -> (a / (decimal b)) |> LispDecimal

        | _ -> raise <| InvalidLispExpressionException $"'/' supports only numeral or decimal values"
