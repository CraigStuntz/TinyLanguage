module TestHelpers

open Syntax

let rec isTreeWithErrorMessageContaining (messageFragment: string) = function
| DefunExpr  (_, _, expression) -> expression |> (isTreeWithErrorMessageContaining messageFragment)
| IdentifierExpr _              -> false
| InvokeExpr (_, argument)      ->
    match argument with
    | Some expression  -> expression |> (isTreeWithErrorMessageContaining messageFragment)
    | None -> false
| NilExpr
| IntExpr     _
| StringExpr  _                 -> false
| ErrorExpr  message            -> message.Contains(messageFragment)