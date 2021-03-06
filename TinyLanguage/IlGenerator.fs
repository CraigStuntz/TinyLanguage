﻿module IlGenerator

open Types
open Il
open Railway
open Binder

type Instrinsic = 
    | IncInt
    | WriteLine
        
let private writeLineMethod = 
    typeof<System.Console>.GetMethod("WriteLine", [| typeof<System.Int32> |])

let private codegenIntrinsic = function
    | IncInt    -> 
        [   Instruction.Ldc_I4_1
            Instruction.Add ]
    | WriteLine -> 
        [   Instruction.Call writeLineMethod ]

let private nameToOper = function
| "inc"   -> IncInt
| "print" -> WriteLine
| unknown -> failwithf "Unknown function %s" unknown

let rec private codegenBinding (binding : Binding) = 
    match binding with
    | VariableBinding(_, _) -> [LdArg_0]
    | BoolBinding   b  -> 
        match b with
        | true         -> [Ldc_I4_1]
        | false        -> [Ldc_I4_0]
    | IntBinding    n  -> [Ldc_I4 n]
    | StringBinding s  -> [Ldstr s]
    | InvokeBinding { FunctionName = name; Argument = argument } -> 
        let invoke = (name |> nameToOper |> codegenIntrinsic)
        match argument |> inferType with 
        | UnitType -> invoke
        | _ -> 
            let arguments = argument |> codegenBinding
            arguments @ invoke
    | wrong -> failwithf "Sorry, you can't pass %A here!" wrong

let private codegenStatements (statement : Binding): Method list = 
    match statement with
    | DefBinding { VariableName = name; VariableBinding = FunctionBinding ( UserFunction (Argument = argument; Body = body; ResultType = resultType ) ) } ->
        let argumentType = 
            match argument.ArgumentType with
            | UnitType -> None
            | _ -> Some argument.ArgumentType
        let instructions = codegenBinding body
        [{ 
            Name = name
            Instructions = instructions
            ArgumentType = argumentType 
            ReturnType   = resultType |> Il.typeOf
        }]
    | wrong -> failwithf "Expected Defun, found %A." wrong

let rec private hasMain (statements : Binding) : bool = 
    // "The entry point method shall either accept no arguments or a vector of strings."
    // -ECMA-335, II.15.4.1.2 The .entrypoint directive
    let isMain = function 
        |  DefBinding { VariableName = "main"; VariableBinding = FunctionBinding _ } -> true
        | _ -> false
    Binder.bindingExists isMain statements


let private ensureHasMain (statements : Binding) : Result<Binding, string> =
    match hasMain statements with
    | true  -> succeed statements
    | false -> fail "Could not find a main method or a statement to execute."


let codegen (statements : Binding): Result<Method list, string> = 
    statements 
        |> Binder.failIfAnyErrors
        |> Railway.bind ensureHasMain
        |> Railway.map  codegenStatements