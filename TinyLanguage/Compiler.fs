module Compiler

open Parser
open Syntax
open System.Reflection
open Il

let compile =
    Lexer.lex
    >> Parser.parse
    >> Binder.bind
    >> IlGenerator.codegen
    >> Railway.map OptimizeIl.optimize
    >> Railway.map Il.toAssemblyBuilder