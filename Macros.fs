module internal OpilioCraft.Lisp.StandardLib.Macros

open OpilioCraft.Lisp

let macroNotEqual (exprList : Expression list) : Expression =
    List [ Symbol "not" ; List ( Symbol "eq" :: exprList ) ]

let macroBetween (exprList : Expression list) : Expression =
    match exprList with
    | [ lower; upper; value ] ->
        let checkLowerBorder = List [ Symbol "ge"; value ; lower ]
        let checkUpperBorder = List [ Symbol "le"; value; upper ]
        List [ Symbol "and" ; checkLowerBorder ; checkUpperBorder ]
    | _ -> raise <| InvalidArgsException

let macroInside (exprList : Expression list) : Expression =
    match exprList with
    | [ lower; upper; value ] ->
        let checkLowerBorder = List [ Symbol "gt"; value ; lower ]
        let checkUpperBorder = List [ Symbol "lt"; value; upper ]
        List [ Symbol "and" ; checkLowerBorder ; checkUpperBorder ]
    | _ -> raise <| InvalidArgsException
