namespace TinyLangage.Tests.IlGenerator

open NUnit.Framework
open Binding
open FsUnit
open Il
open IlGenerator
open Parser
open Railway
open Syntax
open TestHelpers

type IlGeneratorTests () =
    [<Test>] 
    member this.``should generate IL code for (inc 2) method``() = 
        let expected: Result<Method list, string> = 
            Success([ { Name = "main"; Instructions = [ Ldc_I4 2; Ldc_I4_1; Add]; ArgumentType = IntType; ReturnType = typeof<int> } ] )
        let actual = codegen [ Defun { Name = "main"; Argument = { Name = "i"; ArgumentType = IntType }; Body = InvokeBinding { Name = "inc"; Argument = IntBinding 2; ResultType = IntType }; ResultType = IntType } ]
        actual |> should equal expected