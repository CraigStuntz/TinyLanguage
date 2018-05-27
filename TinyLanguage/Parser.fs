module Parser

open Lexer
open Syntax

type private ParseState = {
    Expressions: Expression list
    Remaining:   Lexeme list 
}

let private error (message: string): ParseState =
    { Expressions = [ ErrorExpr message ]; Remaining = [] }

let rec private parseExpression (state : ParseState): ParseState =
    match state.Remaining with
    | LeftParenthesis     :: Identifier "defun"    :: Identifier name :: rest -> 
        let defun = parseDefun (name, { state with Remaining = rest })
        match defun.Expressions, defun.Remaining with
        | [ ErrorExpr _ ], _            -> defun
        | _, RightParenthesis :: remaining -> { defun with Remaining = remaining }
        | _, []                            -> error ("Expected ')'.") 
        | _, wrong :: _                    -> error (sprintf "Expected ')'; found %A." wrong) 
    | LeftParenthesis     :: Identifier name    :: argumentsAndBody -> 
        let invoke = parseInvoke (name, { state with Remaining = argumentsAndBody })
        match invoke.Remaining with
        | RightParenthesis :: remaining -> { invoke with Remaining = remaining }
        | []                            -> error ("Expected ')'.") 
        | wrong :: _                    -> error (sprintf "Expected ')'; found %A." wrong) 
    | LeftParenthesis     :: RightParenthesis :: rest ->
        { Expressions = state.Expressions @ [ NilExpr ]; Remaining = rest }
    | LeftParenthesis     :: wrong -> error (sprintf "%A cannot follow '('." wrong) 
    | RightParenthesis    :: _     -> error ("Unmatched )")
    | Identifier   name   :: rest  -> 
         { Expressions = state.Expressions @ [ IdentifierExpr name ]; Remaining = rest }
    | LiteralInt   number :: rest  ->  
        { Expressions = state.Expressions @ [ IntExpr number ]; Remaining = rest }
    | LiteralString   str :: rest  ->  
        { Expressions = state.Expressions @ [ StringExpr str ]; Remaining = rest }    | Unrecognized char   :: _    -> error (sprintf "Unexpected character %A" char )
    | [] -> state
and private parseDefunBody (name: string, argument: ArgumentExpression option, state : ParseState) =
    let body = parseExpression { state with Expressions = [] }
    match body.Expressions with 
    | [] -> error(sprintf "Implementation expected for function '%s'." name)
    | [ bodyExpression ] ->
        { Expressions = state.Expressions @ [ DefunExpr(name, argument, bodyExpression ) ]; Remaining = body.Remaining }
    | multiple -> error(sprintf "Expected only one expression in implementation of function '%s'." name)
and private parseDefun (name: string, state : ParseState) =
    let argumentExpr = parseExpression { state with Expressions = [] }
    match argumentExpr.Expressions with
    | [ InvokeExpr(typeName, argumentOption) ] ->
        match argumentOption with
        | Some (IdentifierExpr argument) when List.contains typeName [ "bool"; "int" ] ->
            let argument = Some <| { TypeName = typeName; ArgumentName = argument }
            parseDefunBody(name, argument, { state with Remaining = argumentExpr.Remaining } )
        | Some wrong -> { state with Expressions = [ ErrorExpr "Arguments must have a type and an identifier name." ] }
        | None ->
            parseDefunBody(name, None, argumentExpr)
    | [ NilExpr ] -> 
        parseDefunBody(name, None, { state with Remaining = argumentExpr.Remaining })
    | ErrorExpr message :: _ -> error message
    | _ -> error "Exactly one argument expected."
and private parseInvoke (identifier: string, state : ParseState) =
    match parseExpression { state with Expressions = [] } with
    | { Expressions = [ argument ]; Remaining = rest } ->
        { Expressions = state.Expressions @ [ InvokeExpr(identifier, Some argument) ]; Remaining = rest }
    | { Expressions = []; Remaining = rest } ->
        { Expressions = state.Expressions @ [ InvokeExpr(identifier, None) ]; Remaining = rest }
    | _ -> error "Exactly one argument expected."

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
