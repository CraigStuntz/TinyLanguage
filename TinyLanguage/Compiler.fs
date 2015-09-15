module Compiler

open Parser
open Syntax
open System.Reflection

let compile =
    Lexer.lex
        >> Parser.parse
        >> Binding.fromExpressions
        >> IlGenerator.codegenStatements
        >> Railway.map Il.toAssemblyBuilder