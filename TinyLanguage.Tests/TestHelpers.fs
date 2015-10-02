module TestHelpers

open Syntax

let rec isTreeWithErrorMessageContaining (messageFragment: string) = function
| DefunExpr  (_, _, expression) -> expression |> (isTreeWithErrorMessageContaining messageFragment)
| IdentifierExpr _              -> false
| InvokeExpr (_, expression)    -> expression |> (isTreeWithErrorMessageContaining messageFragment)
| IntExpr     _                 -> false
| StringExpr  _                 -> false
| ErrorExpr  message            -> message.Contains(messageFragment)