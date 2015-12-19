module Syntax

type ArgumentExpression = {
    TypeName:     string
    ArgumentName: string 
}

type Expression = 
    | IntExpr        of int
    | StringExpr     of string
    | DefunExpr      of name: string * argument: ArgumentExpression option * body: Expression
    | InvokeExpr     of name: string * argument: Expression option
    | IdentifierExpr of string
    | ErrorExpr      of string
    | EmptyListExpr

let rec private findErrors (expression : Expression): string list = 
    match expression with
    | ErrorExpr error           -> [ error ]
    | EmptyListExpr
    | IdentifierExpr _
    | IntExpr _
    | StringExpr _              -> []
    | DefunExpr (_, _, body)    -> body |> findErrors
    | InvokeExpr (_, argument)  -> 
        match argument with
        | Some arg -> arg |> findErrors
        | None -> []

