[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OpilioCraft.Lisp.StandardLib

open StandardLib.TypeConstructors
open StandardLib.UnaryOperators
open StandardLib.BinaryOperators
open StandardLib.Functions
open StandardLib.Macros

// helpers
let liftExpression (expr : Expression) = [ expr ]

let liftUnaryOp (opName : string) (op : UnaryOperator) (runtime : IRuntime) (args : Expression list) =
    match args with
    | [ arg ] -> op runtime arg
    | _ -> raise <| WrongNumberOfArgsException (opName, 1)

let liftBinaryOp (opName : string) (op : BinaryOperator) (runtime : IRuntime) (args : Expression list) =
    match args with
    | [ a; b ] -> op runtime (a, b)
    | _ -> raise <| WrongNumberOfArgsException (opName, 2)

// initialize library
let init : Map<string, Function> =
    seq {
        // quote operator
        "quote", liftUnaryOp "quote" unaryQuote

        // type constructors
        "#date", stdDate
        "#time", stdTime
        "#datetime", stdDateTime

        // datetime functions
        "year", liftUnaryOp "year" unaryYear
        "month", liftUnaryOp "month" unaryYear
        "day", liftUnaryOp "day" unaryYear

        // boolean functions
        "not", liftUnaryOp "not" unaryNot
        "and", stdAnd
        "or", stdOr

        // comparisons
        "eq", liftBinaryOp "eq" stdEqual
        "lt", liftBinaryOp "lt" stdLower
        "le", liftBinaryOp "le" stdLowerEqual
        "gt", liftBinaryOp "gt" stdGreater
        "ge", liftBinaryOp "ge" stdGreaterEqual
        "cond", stdCond
    }
    |> Map.ofSeq

let registerMacros (runtime : IRuntime) : IRuntime =
    runtime.RegisterMacro "neq" macroNotEqual
    runtime.RegisterMacro "between" macroBetween
    runtime.RegisterMacro "inside" macroInside
    runtime
