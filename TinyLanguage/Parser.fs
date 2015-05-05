module Parser

open Lexer

type Operation = | Plus | Minus | Times
let private parseOperation = function
| "+" -> Some Plus
| "-" -> Some Minus
| "*" -> Some Times
| _   -> None 

type Function =
    | Builtin of Operation

type Expression = 
    | ConstantInt of int
    | Defun       of string   * Expression list
    | Invoke      of Function * Expression list
    | Error       of string

let rec findAllErrors = function
| Defun       (_, expressions) -> expressions |> List.collect findAllErrors
| Invoke      (_, expressions) -> expressions |> List.collect findAllErrors
| ConstantInt _ -> []
| Error message -> [ message ]

type private ParseState = {
    Expressions: Expression list
    Remaining:   Lexeme list 
}

let private error (state : ParseState, message: string): ParseState =
    { state with Expressions = state.Expressions @ [ Error message ] }

let parse (lexemes: Lexeme list): Expression list=
    []