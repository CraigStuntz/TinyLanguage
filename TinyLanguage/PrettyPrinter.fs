module PrettyPrinter

open Syntax

let rec prettyPrint = function
| IntExpr number -> number.ToString(System.Globalization.CultureInfo.InvariantCulture)
| StringExpr str -> sprintf "\"%s\"" str
| DefunExpr (name, arguments, body) ->
    let argumentExpressions = arguments |> String.concat " "
    let bodyExpression = prettyPrint body
    sprintf "(defun %s (%s) %s)" name argumentExpressions bodyExpression
| InvokeExpr (name, arguments) ->
    let argumentExpressions = arguments |> List.map prettyPrint |> String.concat " "
    sprintf "(%s %s)" name argumentExpressions
| ErrorExpr message -> message