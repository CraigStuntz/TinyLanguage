module PrettyPrinter

open Syntax

let prettyPrintArgument = function
| Some argument -> sprintf "(%s %s)" argument.TypeName argument.ArgumentName
| None          -> "()"

let rec prettyPrint = function
| IdentifierExpr name -> name
| EmptyListExpr       -> "()"
| IntExpr number      -> number.ToString(System.Globalization.CultureInfo.InvariantCulture)
| StringExpr str      -> sprintf "\"%s\"" str
| DefunExpr (name, argument, body) ->
    let bodyExpression = prettyPrint body
    sprintf "(defun %s %s %s)" name (prettyPrintArgument argument) bodyExpression
| InvokeExpr (name, argument) ->
    match argument with
    | Some arg -> sprintf "(%s %s)" name (prettyPrint arg)
    | None     -> sprintf "(%s)" name
| ErrorExpr message   -> message