﻿namespace TinyLangage.Tests.IlGenerator

open NUnit.Framework
open Binder
open Types
open FsUnit
open Il
open IlGenerator
open Railway

type IlGeneratorTests () =
    [<Test>] 
    member this.``should generate IL code for (defun "main" () (inc 2)) method``() = 
        let expected: Result<Method list, string> = 
            Success([ { Name = "main"; Instructions = [ Ldc_I4 2; Ldc_I4_1; Add]; ArgumentType = None; ReturnType = typeof<int> } ] )
        let actual = 
            codegen (
                DefBinding {
                    VariableName = "main"
                    VariableBinding = 
                        FunctionBinding (
                            UserFunction(
                                Argument = { 
                                    ArgumentName = "()"
                                    ArgumentType = UnitType 
                                },
                                Body = 
                                    InvokeBinding { 
                                        FunctionName = "inc"
                                        Argument = IntBinding 2
                                        Function =  Inc 
                                    },
                                ResultType = IntType ) )
                    Body = IntBinding 1
                })
        actual |> should equal expected