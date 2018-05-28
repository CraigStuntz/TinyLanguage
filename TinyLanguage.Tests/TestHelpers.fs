module TestHelpers

open Syntax

let rec isTreeWithErrorMessageContaining (messageFragment: string) = function
| DefunExpr  (_, _, expression) -> expression |> (isTreeWithErrorMessageContaining messageFragment)
| IdentifierExpr _              -> false
| InvokeExpr (_, argument)      -> argument |> (isTreeWithErrorMessageContaining messageFragment)
| NilExpr
| IntExpr     _
| StringExpr  _                 -> false
| ErrorExpr  message            -> message.Contains(messageFragment)