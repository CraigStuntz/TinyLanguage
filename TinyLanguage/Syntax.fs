﻿module Syntax

type ArgumentExpression = {
    TypeName:     string
    ArgumentName: string 
}

type Expression =
    /// A literal integer
    | IntExpr        of int
    /// A literal string in double quotes
    | StringExpr     of string
    /// A function definition
    | DefunExpr      of name: string * argumentExpression: ArgumentExpression * body: Expression
    /// Invoking (calling) a function
    | InvokeExpr     of name: string * argument: Expression
    /// An unquoted string starting with a letter. Usually a function or variable name.
    | IdentifierExpr of string
    /// Bad syntax found by the scanner. Not a valid part of a program
    | ErrorExpr      of string
    /// <summary>Nothing. Used only as the argument to the entry point, as required by ECMA-335
    /// (ECMA-335 also allows lists as an entry point argument, but we have no lists!)
    /// Nil is written as <c>()</c> and has type <c>unit</c></summary>
    | NilExpr

let rec private findErrors (expression : Expression): string list = 
    match expression with
    | ErrorExpr error           -> [ error ]
    | NilExpr
    | IdentifierExpr _
    | IntExpr _
    | StringExpr _              -> []
    | DefunExpr (_, _, body)    -> body |> findErrors
    | InvokeExpr (_, argument)  -> argument |> findErrors
