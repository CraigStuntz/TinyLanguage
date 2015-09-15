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
    member this.``should generate IL code for (+ 1 2) method``() = 
        let expected: Result<Map<string, Instruction list>, string> = 
            Success(Map([("main", [ Ldc_I4 1; Ldc_I4 2; Add])]))
        codegenStatements [ Defun { Name = "main"; Arguments = []; Body = InvokeBinding { Name = "+"; Arguments = [IntBinding 1; IntBinding 2]; ResultType = IntType }; ResultType = VoidType } ]
            |> should equal expected