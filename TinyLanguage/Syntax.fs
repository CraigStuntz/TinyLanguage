module Syntax

type Expression = 
    | IntExpr     of int
    | StringExpr  of string
    | DefunExpr   of name: string * arguments: string list * body: Expression 
    | InvokeExpr  of name: string * arguments: Expression list
    | ErrorExpr   of string

let rec private findErrors (expression : Expression): string list = 
    match expression with
    | ErrorExpr error           -> [ error ]
    | IntExpr _                 -> []
    | StringExpr _              -> []
    | DefunExpr (_, _, body)    -> body |> findErrors
    | InvokeExpr (_, arguments) -> arguments |> List.collect findErrors


let findAllErrors (expressions : Expression list): string list = expressions |> List.collect findErrors
