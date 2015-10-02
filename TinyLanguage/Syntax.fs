module Syntax

type Expression = 
    | IntExpr        of int
    | StringExpr     of string
    | DefunExpr      of name: string * argument: string * body: Expression
    | InvokeExpr     of name: string * argument: Expression 
    | IdentifierExpr of string
    | ErrorExpr      of string

let rec private findErrors (expression : Expression): string list = 
    match expression with
    | ErrorExpr error           -> [ error ]
    | IdentifierExpr _          -> [] 
    | IntExpr _                 -> []
    | StringExpr _              -> []
    | DefunExpr (_, _, body)    -> body     |> findErrors
    | InvokeExpr (_, argument)  -> argument |> findErrors


let findAllErrors (expressions : Expression list): string list = expressions |> List.collect findErrors
