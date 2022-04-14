[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OpilioCraft.Lisp.StandardLib

open StandardLib.TypeConstructors
open StandardLib.UnaryOperators
open StandardLib.BinaryOperators
open StandardLib.Functions
open StandardLib.Macros

// function catalogue
let unaryFunctions : Map<string, Expression -> Expression> = 
    Map.ofArray
        [|
            // quote operator
            "quote", unaryQuote

            // boolean functions
            "not", unaryNot

            // datetime functions
            "year", unaryYear
            "month", unaryYear
            "day", unaryYear
        |]

let binaryFunctions : Map<string, Expression * Expression -> Expression> =
    Map.ofArray
        [|
            // comparisons
            "eq", stdEqual
            "lt", stdLower
            "le", stdLowerEqual
            "gt", stdGreater
            "ge", stdGreaterEqual
        |]

let ordinaryFunctions : Map<string, Function> =
    Map.ofArray
        [|
            // type constructors
            "#date", stdDate
            "#time", stdTime
            "#datetime", stdDateTime

            // boolean functions
            "and", stdAnd
            "or", stdOr
            "cond", stdCond

            // macros
            "neq", macroNotEqual
            "between", macroBetween
            "inside", macroInside
        |]
