module Compiler

open Parser
open System.Reflection

let rec private allErrors = function
| Defun       (_, expressions) -> expressions |> List.collect allErrors 
| Invoke      (_, expressions) -> expressions |> List.collect allErrors
| ConstantInt _ -> [ None ]
| Error message -> [ Some message ]

let compile =
    Lexer.lex
        >> Parser.parse
        >> IlGenerator.codegenDefuns
        >> Railway.map Il.toAssemblyBuilder