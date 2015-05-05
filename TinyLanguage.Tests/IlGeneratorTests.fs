namespace TinyLangage.Tests.IlGenerator

open NUnit.Framework
open FsUnit
open Il
open IlGenerator
open Parser
open Railway
open TestHelpers

type IlGeneratorTests () =
    [<Test>] 
    member this.``should generate IL code for (+ 1 2) method``() = 
        let expected: Result<Map<string, Instruction list>, string> = 
            Success(Map([("main", [ Ldc_I4 1; Ldc_I4 2; Add])]))
        codegenDefuns [ Defun("main", [ Invoke (Builtin Plus, [ConstantInt 1; ConstantInt 2])])]
            |> should equal expected