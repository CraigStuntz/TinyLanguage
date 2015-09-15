module Parser

open Lexer
open Syntax

type private ParseState = {
    Expressions: Expression list
    Remaining:   Lexeme list 
}

let private error (state : ParseState, message: string): ParseState =
    { state with Expressions = state.Expressions @ [ ErrorExpr message ] }

let rec private parseExpression (state : ParseState): ParseState =
    match state.Remaining with
    | LeftParenthesis     :: Identifier name    :: argumentsAndBody -> 
        let invoke = parseInvoke (name, { state with Remaining = argumentsAndBody })
        match invoke.Remaining with
        | RightParenthesis :: remaining -> { invoke with Remaining = remaining }
        | []                            -> error (invoke, "Expected ')'.") 
        | wrong :: _                    -> error (state, sprintf "Expected ')'; found %A." wrong) 
    | LeftParenthesis     :: wrong -> error (state, sprintf "%A cannot follow '('." wrong) 
    | RightParenthesis    :: _     -> error (state, "Unmatched )")
    | Identifier   name   :: _     -> error (state, sprintf "Unrecognized identifier '%s'." name) 
    | LiteralInt   number :: rest  ->  
        { Expressions = state.Expressions @ [ IntExpr number ]; Remaining = rest }
    | LiteralString   str :: rest  ->  
        { Expressions = state.Expressions @ [ StringExpr str ]; Remaining = rest }    | Unrecognized char   :: _    -> error (state, sprintf "Unexpected character %A" char )
    | [] -> state
and private parseInvoke (identifier: string, state : ParseState) =
    let arguments = parseArguments { state with Expressions = [] }
    { Expressions = state.Expressions @ [ InvokeExpr(identifier, arguments.Expressions) ]; Remaining = arguments.Remaining }
and private parseArguments (state : ParseState) : ParseState =
    match state.Remaining with 
    | [] -> state // will be converted to error by parseExpression
    | RightParenthesis :: rest -> state
    | _ -> parseArguments (parseExpression state)

let rec private parseExpressions (state : ParseState): ParseState =
    let parsed = parseExpression state
    match parsed.Remaining with
    | [] -> parsed
    | _  -> parseExpressions parsed

let rec private containsMain expressions = 
    match expressions with 
    | DefunExpr ("main", _, _) :: _-> true
    | [] -> false
    | _ :: rest -> containsMain rest

let parse (lexemes: Lexeme list): Expression list=
    let parsed = parseExpressions { Expressions = []; Remaining = lexemes }
    parsed.Expressions