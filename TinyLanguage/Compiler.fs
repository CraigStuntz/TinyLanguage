module Compiler

open Parser
open Syntax
open System.Reflection

let rec private allErrors = function
| DefunExpr       (_, expressions) -> expressions |> List.collect allErrors 
| InvokeExpr      (_, expressions) -> expressions |> List.collect allErrors
| IntExpr _                        -> [ None ]
| ErrorExpr message                -> [ Some message ]

let compile =
    Lexer.lex
        >> Parser.parse
        >> Binding.fromExpressions
        >> IlGenerator.codegenDefuns
        >> Railway.map Il.toAssemblyBuilder