module Compiler

let compile =
    Lexer.lex
    >> Parser.parse
    >> Binder.bind
    >> IlGenerator.codegen
    >> Railway.map OptimizeIl.optimize
    >> Railway.map Il.toAssemblyBuilder