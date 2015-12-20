module Compiler

open Parser
open Syntax
open System.Reflection
open Il

let compile =
    Lexer.lex
        >> Parser.parse
        >> Binder.fromExpressions
        >> IlGenerator.codegen
        >> Railway.map Il.toAssemblyBuilder