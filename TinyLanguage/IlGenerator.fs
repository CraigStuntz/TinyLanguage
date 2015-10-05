module IlGenerator

open Binding
open Il
open Parser
open Railway
open Syntax

type Builtin = 
    | Inc
    | WriteLine
        
let writeLineMethod = typeof<System.Console>.GetMethod("WriteLine", [| typeof<System.Int32> |])

let private codegenOper = function
    | Inc       -> [ Instruction.Ldc_I4_1; Instruction.Add ]
    | WriteLine -> [ Instruction.Call writeLineMethod ]

let private nameToOper = function
| "inc" -> Inc
| "WriteLine" -> WriteLine
| unknown -> failwithf "Unknown function %s" unknown

let rec private codegenBinding (binding : Binding) = 
    match binding with
    | BoolBinding   b -> 
        match b with
        | true        -> [Ldc_I4_1]
        | false       -> [Ldc_I4_0]
    | IntBinding    n -> [Ldc_I4 n]
    | StringBinding s -> [Ldstr s]
    | InvokeBinding { Name = name; Argument = argument; ResultType = resultType } -> 
        let arguments = argument |> codegenBinding
        let invoke = (name |> nameToOper |> codegenOper)
        arguments @ invoke
    | wrong -> failwithf "Sorry, you can't pass %A here!" wrong

let private codegenStatement (statement : Statement): (string * Instruction list) = 
    match statement with
    | Defun { Name = name; Argument = argument; Body = body; ResultType = resultType } -> 
        name, body |> codegenBinding
    | wrong -> failwithf "Expected Defun, found %A." wrong

let private codegenStatements = List.map codegenStatement

let codegen (statements : Statement list): Result<Map<string, Instruction list>, string> = 
    statements 
        |> Binding.findAllErrors
        |> Railway.map (codegenStatements >> Map.ofList)