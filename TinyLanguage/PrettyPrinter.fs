module PrettyPrinter

open Syntax

let rec prettyPrint = function
| IdentifierExpr name -> name
| IntExpr number      -> number.ToString(System.Globalization.CultureInfo.InvariantCulture)
| StringExpr str      -> sprintf "\"%s\"" str
| DefunExpr (name, argument, body) ->
    let bodyExpression = prettyPrint body
    sprintf "(defun %s %s %s)" name argument bodyExpression
| InvokeExpr (name, argument) ->
    let argument = argument |> prettyPrint
    sprintf "(%s %s)" name argument
| ErrorExpr message   -> message