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