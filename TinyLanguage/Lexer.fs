module Lexer


type Lexeme =
    | LeftParenthesis
    | RightParenthesis
    | Identifier    of string
    | LiteralInt    of int
    | LiteralString of string
    | Unrecognized  of char


let private prettyPrintLexeme = function
| LeftParenthesis          -> "("           
| RightParenthesis         -> ")"           
| Identifier    identifier -> identifier    
| LiteralInt    num        -> num.ToString()
| LiteralString str        -> str           
| Unrecognized  ch         -> ch.ToString() 

let prettyPrint = 
    List.map prettyPrintLexeme
    >> String.concat ""


let private isIdentifierStart (c: char) =
    System.Char.IsLetter c || System.Char.IsPunctuation c || System.Char.IsSymbol c


let private isIdentifierBody (c: char) =
    (c <> ')') 
    && System.Char.IsLetter c || System.Char.IsPunctuation c || System.Char.IsSymbol c || System.Char.IsDigit c


let rec private lexChars (source: char list) : Lexeme list =
    match source with 
    | '(' :: rest -> 
        LeftParenthesis  :: lexChars rest
    | ')' :: rest -> 
        RightParenthesis :: lexChars rest
    | '"' :: rest -> 
        lexString(rest, "")
    | c   :: rest when isIdentifierStart c -> 
        lexName (source, "")
    | d   :: rest when System.Char.IsDigit d -> 
        lexNumber(source, "")
    | [] -> 
        []
    | w   :: rest when System.Char.IsWhiteSpace w -> 
        lexChars rest
    | c   :: rest -> 
        Unrecognized c :: lexChars rest
and lexName(source: char list, name: string) = 
    match source with 
    | ')' :: rest -> 
        Identifier(name) :: lexChars source
    | c :: rest when isIdentifierStart c && name = "" -> 
        lexName(rest, name + c.ToString())
    | c :: rest when isIdentifierBody c -> 
        lexName(rest, name + c.ToString())
    | _ -> 
        Identifier(name) :: lexChars source
and lexNumber (source: char list, number: string) = 
    match source with
    | d :: rest when System.Char.IsDigit(d) ->
        lexNumber (rest, number + d.ToString())
    | rest -> 
        LiteralInt(System.Int32.Parse(number)) :: lexChars rest
and lexString (source: char list, str: string) =
    match source with
    | [] -> 
        [ LiteralString str ]
    | '"' :: rest -> 
        LiteralString str :: lexChars rest
    | c :: rest -> 
        lexString (rest, str + c.ToString())

let lex (source: string): Lexeme list =
    lexChars (List.ofSeq source)