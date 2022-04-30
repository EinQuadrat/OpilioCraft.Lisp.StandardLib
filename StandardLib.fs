[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OpilioCraft.Lisp.StandardLib

open StandardLib.TypeConstructors
open StandardLib.UnaryFunctions
open StandardLib.BinaryFunctions
open StandardLib.Functions
open StandardLib.Macros

open StandardLib.EvalHelper

// function catalogue
let unaryFunctions : Map<string, Environment -> Expression -> Expression> = 
    Map.ofArray
        [|
            // boolean functions
            "not", unaryNot

            // datetime functions
            "year", unaryYear
            "month", unaryYear
            "day", unaryYear
        |]
    |> Map.map (fun _ body -> (fun env -> Evaluator.evalExpression env >> body env)) // unary function always have to eval their argument

let binaryFunctions : Map<string, Environment -> Expression * Expression -> Expression> =
    Map.ofArray
        [|
            // arithmetic
            "+", binaryAdd
            "-", binarySubtract
            "*", binaryMultiply
            "/", binaryDivide

            // comparison
            "eq", binaryEqual
            "lt", binaryLower
            "le", binaryLowerEqual
            "gt", binaryGreater
            "ge", binaryGreaterEqual

            // matching
            "contains", binaryContains
            "matches",  binaryMatchesRegex
        |]

let ordinaryFunctions : Map<string, Function> =
    Map.ofArray
        [|
            // type constructors
            "#date", fun env -> evalExpressionList env >> ctrDate env
            "#time", fun env -> evalExpressionList env >> ctrTime env
            "#datetime", fun env -> evalExpressionList env >> ctrDateTime env

            // boolean functions
            "and", funcAnd
            "or", funcOr

            // other functions
            "cond", funcCond

            // macros
            "neq", macroNotEqual
            "between", macroBetween
            "inside", macroInside
        |]
