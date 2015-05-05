module Lexer

type Lexeme =
    | LeftParenthesis
    | RightParenthesis
    | Identifier   of string
    | LiteralInt   of int
    | Unrecognized of char

let lex (source: string): Lexeme list =
    []