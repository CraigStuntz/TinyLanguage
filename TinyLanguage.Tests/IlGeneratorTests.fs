namespace TinyLangage.Tests.IlGenerator

open NUnit.Framework
open BindingTree
open FsUnit
open Il
open IlGenerator
open Railway

type IlGeneratorTests () =
    [<Test>] 
    member this.``should generate IL code for (defun "main" () (inc 2)) method``() = 
        let expected: Result<Method list, string> = 
            Success([ { Name = "main"; Instructions = [ Ldc_I4_2; Ldc_I4_1; Add]; ArgumentType = None; ReturnType = typeof<int> } ] )
        let actual = 
            codegen (
                DefBinding {
                    VariableName = "main"
                    VariableBinding = 
                        FunctionBinding (
                            UserFunction(
                                Argument = None,
                                Body = 
                                    InvokeBinding { 
                                        FunctionName = "inc"
                                        Argument = Some ( IntBinding 2 )
                                        Function =  Inc 
                                    },
                                ResultType = IntType ) )
                    Body = EmptyBinding
                })
        actual |> should equal expected