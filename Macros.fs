module internal OpilioCraft.Lisp.StandardLib.Macros

open OpilioCraft.Lisp

// NOTE: macros rewrite the provided LISP expression list, they do not evaluate anything

let macroNotEqual _ (exprList : Expression list) : Expression =
    List [ Symbol "not" ; List ( Symbol "eq" :: exprList ) ]

let macroBetween _ (exprList : Expression list) : Expression =
    match exprList with
    | [ lower; upper; value ] ->
        let checkLowerBorder = List [ Symbol "ge"; value ; lower ]
        let checkUpperBorder = List [ Symbol "le"; value; upper ]
        List [ Symbol "and" ; checkLowerBorder ; checkUpperBorder ]
    | _ -> raise <| InvalidLispExpressionException $"between expects exactly two argument"

let macroInside _ (exprList : Expression list) : Expression =
    match exprList with
    | [ lower; upper; value ] ->
        let checkLowerBorder = List [ Symbol "gt"; value ; lower ]
        let checkUpperBorder = List [ Symbol "lt"; value; upper ]
        List [ Symbol "and" ; checkLowerBorder ; checkUpperBorder ]
    | _ -> raise <| InvalidLispExpressionException $"inside expects exactly two argument"
