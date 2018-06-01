module BinderTests

open NUnit.Framework
open FsUnit
open Binder

type BinderTests() =
    let bind (source: string) : Binding =
        source |> Lexer.lex |> Parser.parse |> Binder.bind

    [<Test>]
    member this.``should generate correct binding for plain expression``() = 
        let source =   "(inc 2)"
        let expected = InvokeBinding { FunctionName = "inc"; Argument = IntBinding 2; Function = Inc }
        let actual =   bind source

        actual |> should equal expected


    [<Test>]
    member this.``should return error for unbound invocation``() = 
        let source = "(bad-method 2)"
        let expected = ErrorBinding "Undefined function 'bad-method'."
        let actual = bind source

        actual |> should equal expected

    [<Test>]
    member this.``should return error for incorrect argument type``() = 
        let source = "(inc \"Hi!\")"
        let expectedError : Railway.Result<Binding, string> = Railway.fail "Expected int argument, but found Hi!."

        let actual = bind source
        let actualErrors = Binder.failIfAnyErrors actual

        actualErrors |> should equal expectedError
        
    [<Test>]
    member this.``should return error for duplicate definition``() = 
        let source = """
        (defun add-1 (int a) (inc a))
        (defun add-1 (int a) (inc a))"""
        let expectedError : Railway.Result<Binding, string> = Railway.fail "Function 'add-1' is already defined."

        let actual = bind source
        let actualErrors = Binder.failIfAnyErrors actual

        actualErrors |> should equal expectedError