namespace TinyLangage.Tests.Lexer

open NUnit.Framework
open FsUnit
open Lexer

type LexerTests () =
    [<Test>] 
    member this.``should lex nothing``() = 
        lex "" |> should equal []

    [<Test>] 
    member this.``should lex (+ 1 2)``() = 
        lex "(+ 1 2)"  |> should equal [ LeftParenthesis; Identifier "+"; LiteralInt 1; LiteralInt 2; RightParenthesis]

    [<Test>] 
    member this.``should lex (defun add (a b) (+ a b))``() = 
        let actual = lex "(defun add (a b) (+ a b))"  
        actual |> should equal [ 
            LeftParenthesis
            Identifier "defun"
            Identifier "add"
            LeftParenthesis
            Identifier "a"
            Identifier "b"
            RightParenthesis
            LeftParenthesis
            Identifier "+"
            Identifier "a"
            Identifier "b"
            RightParenthesis
            RightParenthesis
        ]