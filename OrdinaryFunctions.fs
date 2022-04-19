module internal OpilioCraft.Lisp.StandardLib.Functions

open OpilioCraft.Lisp
open SpecialSymbols

// (and expr1 ... exprN) := expr1 [ && ... && exprN ]
let funcAnd exprList =
    exprList |> List.fold (fun (result : bool) -> function | LispBoolean boolValue -> result && boolValue | _ -> false) true
    |> LispBoolean

// (or expr1 ... exprN) := expr1 [ || ... || exprN ]
let funcOr exprList =
    exprList |> List.fold (fun (result : bool) -> function | LispBoolean boolValue -> result || boolValue | _ -> false) true
    |> LispBoolean

// (cond (test1 action1) [ ... (testN actionN) ] )
let rec funcCond exprList : Expression =
    match exprList with
    | [ ] -> NIL
    | List [ test; action ] :: tail ->
        match test with
        | LispBoolean true -> action
        | LispBoolean false -> funcCond tail
        | _ -> invalidArg "cond" "cond( (test action) ...): test has to result into a boolean value"
    | _ -> invalidArg "cond" "cond expects pairs of test and action as arguments"
