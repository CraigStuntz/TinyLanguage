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
    | ArgBinding(_, _) -> [LdArg_0]
    | BoolBinding   b  -> 
        match b with
        | true         -> [Ldc_I4_1]
        | false        -> [Ldc_I4_0]
    | IntBinding    n  -> [Ldc_I4 n]
    | StringBinding s  -> [Ldstr s]
    | InvokeBinding { Name = name; Argument = argument; ResultType = resultType } -> 
        let arguments = argument |> codegenBinding
        let invoke = (name |> nameToOper |> codegenOper)
        arguments @ invoke
    | wrong -> failwithf "Sorry, you can't pass %A here!" wrong

let private codegenStatement (statement : Statement): Method = 
    match statement with
    | Defun { Name = name; Argument = argument; Body = body; ResultType = resultType } ->
        let instructions = codegenBinding body
        { 
            Name = name
            Instructions = instructions
            ArgumentType = argument.ArgumentType 
            ReturnType   = resultType |> Il.typeOf
        }
    | wrong -> failwithf "Expected Defun, found %A." wrong

let private codegenStatements = List.map codegenStatement

let rec private hasMain (statements : Statement list) : bool = 
    match statements with 
    | [] -> false
    | Defun { Name = "main" } :: rest -> true
    | _ :: rest -> hasMain rest

let private isDefun = function
    | Defun _  -> true
    | Ignore _ -> false

let private lastIgnoreBinding (statements : Statement list) : Binding option =
    let rec lastIgnoreBinding' (statements : Statement list) =
        match statements with
        | [] -> None
        | Ignore binding :: rest -> Some binding
        | _ :: rest -> lastIgnoreBinding' rest
    statements |> List.rev |> lastIgnoreBinding'

let private ensureHasMain (statements : Statement list) : Result<Statement list, string> =
    let defuns, ignores = statements |> List.partition isDefun
    match hasMain statements with
    | true  -> succeed defuns
    | false -> 
        match lastIgnoreBinding statements with 
        | None -> fail "Could not find a main method or a statement to execute."
        | Some body -> 
            succeed [ Defun { 
                Name = "main"
                Argument = { Name = "argv"; ArgumentType = StringType}
                Body = body
                ResultType = body |> Binding.inferType} ]

let codegen (statements : Statement list): Result<Method list, string> = 
    statements 
        |> Binding.failIfAnyErrors
        |> Railway.bind ensureHasMain
        |> Railway.map  codegenStatements