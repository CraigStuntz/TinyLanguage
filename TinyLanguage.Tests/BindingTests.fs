module BindingTests

open NUnit.Framework
open FsUnit
open Binding
open Lexer
open Parser

type BindingTests() =
    let bind (source: string) : Statement list =
        source |> Lexer.lex |> Parser.parse |> Binding.fromExpressions

    [<Test>]
    member this.``should generate correct binding for plain expression``() = 
        let source = "(inc 2)"
        let expected = [ 
            Ignore (InvokeBinding { Name = "inc"; Argument = IntBinding 2; ResultType = IntType })
        ]
        let actual = bind source

        actual |> should equal expected


    [<Test>]
    member this.``should return error for unbound invocation``() = 
        let source = "(bad-method 2)"
        let expected = [ 
            Ignore (ErrorBinding "Undefined function 'bad-method'.")
        ]
        let actual = bind source

        actual |> should equal expected

    [<Test>]
    member this.``should return error for incorrect argument type``() = 
        let source = "(inc \"Hi!\")"
        let expected = [ 
            Ignore (ErrorBinding "Expected integer; found \"Hi!\".")
        ]
        let actual = bind source

        actual |> should equal expected
        
    [<Test>]
    member this.``should return error for duplicate definition``() = 
        let source = """
        (defun add-1 a (inc a))
        (defun add-1 a (inc a))"""
        let expected = Ignore (ErrorBinding "Function 'add-1' is already defined.")

        let actual = bind source

        actual.[1] |> should equal expected