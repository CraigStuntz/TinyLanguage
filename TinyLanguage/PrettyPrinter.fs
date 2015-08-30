module PrettyPrinter

open Syntax

let private prettyPrintOperation = function
| Plus -> "+"
| Minus -> "-"
| Times -> "*"

let private prettyPrintFunction = function
| Builtin operation -> prettyPrintOperation operation

let rec prettyPrint = function
| IntExpr number -> number.ToString(System.Globalization.CultureInfo.InvariantCulture)
| DefunExpr (name, body) ->
    let bodyExpressions = body |> List.map prettyPrint |> String.concat " "
    sprintf "(defun %s %s)" name bodyExpressions
| InvokeExpr (f, arguments) ->
    let name = prettyPrintFunction f
    let argumentExpressions = arguments |> List.map prettyPrint |> String.concat " "
    sprintf "(%s %s)" name argumentExpressions
| ErrorExpr message -> message