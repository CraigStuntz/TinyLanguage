namespace TinyLangage.Tests.Lexer

open NUnit.Framework
open FsUnit
open Lexer
open Parser
open TestHelpers

type ParserTests () =
    [<Test>] 
    member this.``should parse nothing``() = 
        parse [] |> should equal []

    [<Test>] 
    member this.``should parse (+ 1 2)``() = 
        parse [ LeftParenthesis; Identifier "+"; LiteralInt 1; LiteralInt 2; RightParenthesis] 
            |> should equal 
            [ Defun("main", [ Invoke (Builtin Plus, [ConstantInt 1; ConstantInt 2]) ] ) ]

    [<Test>] 
    member this.``missing right parenthesis should error``() = 
        NUnit.Framework.Assert.That(
            parse [ LeftParenthesis; Identifier "+"; LiteralInt 1; LiteralInt 2] 
            |> List.exists (isTreeWithErrorMessageContaining "Expected ')'"))