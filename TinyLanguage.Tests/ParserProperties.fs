module ParserProperties

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Parser
open PrettyPrinter
open Syntax

let fctn = Gen.map Builtin Arb.generate<Operation>

let expr =
    let rec expr' s = 
        match s with
        | 0 -> Gen.map IntExpr Arb.generate<int>
        | n when n > 0 -> 
            Gen.oneof [ Gen.map IntExpr Arb.generate<int>
                        Gen.map3 (fun f x y -> InvokeExpr (f, [ x; y ])) fctn (expr' (n/2)) (expr' (n/2))
                      ]
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    Gen.sized expr'

type ParserGenerators =
    static member Expression() =
        { new Arbitrary<Expression>() with
            override x.Generator = expr
            override x.Shrinker t = Seq.empty }

//[<TestFixtureSetUp>]
let setup() = 
    Arb.register<ParserGenerators>() |> ignore

//[<Property>]
let ``parse of prettyPrint is original`` () = 
    let property expression = 
        let parsed = 
            PrettyPrinter.prettyPrint expression
            |> Lexer.lex
            |> Parser.parse
        parsed = Parser.ensureHasMainFunction [ expression ]
    Check.QuickThrowOnFailure property