module Binding

open Syntax

type Binding = 
    | IntBinding     of int
    | DefunBinding   of string   * Binding list
    | InvokeBinding  of Function * Binding list
    | ErrorBinding   of string

let rec fromExpression (expression : Expression) : Binding = 
    match expression with
    | IntExpr n               -> IntBinding n
    | DefunExpr (name, exprs) -> DefunBinding (name, exprs |> List.map fromExpression)
    | InvokeExpr (fn, exprs)  -> InvokeBinding (fn, exprs |> List.map fromExpression)
    | ErrorExpr error         -> ErrorBinding error

let fromExpressions = List.map fromExpression