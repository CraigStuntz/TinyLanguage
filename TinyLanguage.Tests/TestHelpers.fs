module TestHelpers

open Parser

let rec isTreeWithErrorMessageContaining (messageFragment: string) = function
| Defun       (_, expressions) -> expressions |> List.exists (isTreeWithErrorMessageContaining messageFragment)
| Invoke      (_, expressions) -> expressions |> List.exists (isTreeWithErrorMessageContaining messageFragment)
| ConstantInt _ -> false
| Error message -> message.Contains(messageFragment)