module internal OpilioCraft.Lisp.StandardLib.EvalHelper

open OpilioCraft.Lisp

let evalExpressionTupel env (exprA, exprB) : Expression * Expression =
    (Evaluator.evalExpression env exprA), (Evaluator.evalExpression env exprB)

let evalExpressionList env : Expression list -> Expression list =
    List.map (Evaluator.evalExpression env)
