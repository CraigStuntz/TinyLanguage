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
        let source = "(+ 1 2)"
        let expected = [ 
            Ignore (InvokeBinding { Name = "+"; Arguments = [ IntBinding 1; IntBinding 2]; ResultType = IntType })
        ]
        let actual = bind source

        actual |> should equal expected


    [<Test>]
    member this.``should return error for unbound invocation``() = 
        let source = "(bad-method 1 2)"
        let expected = [ 
            Ignore (ErrorBinding "Undefined function 'bad-method'.")
        ]
        let actual = bind source

        actual |> should equal expected

    [<Test>]
    member this.``should return error for incorrect argument type``() = 
        let source = "(+ \"Hi!\" 2)"
        let expected = [ 
            Ignore (ErrorBinding "Expected integer; found \"Hi!\".")
        ]
        let actual = bind source

        actual |> should equal expected

    [<Test>]
    member this.``should return error for incorrect argument count``() = 
        let source = "(+ 2)"
        let expected = [ 
            Ignore (ErrorBinding "Expected 2 arguments, found 1.")
        ]
        let actual = bind source

        actual |> should equal expected