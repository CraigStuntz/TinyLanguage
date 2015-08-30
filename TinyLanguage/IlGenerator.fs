module IlGenerator

open Binding
open Il
open Parser
open Railway
open Syntax

let private codegenOper = function
    | Plus     -> Instruction.Add
    | Minus    -> Instruction.Sub
    | Times    -> Instruction.Mul

let rec private codegenBinding (binding : Binding) = 
    match binding with
    | IntBinding n -> [Ldc_I4 n]
    | InvokeBinding (Builtin operation, bindings) -> 
        let argumentCount = List.length bindings
        let arguments = bindings |> List.collect codegenBinding
        let invokes = List.replicate (argumentCount - 1) (codegenOper operation)
        arguments @ invokes
    | wrong -> failwithf "Sorry, you can't pass %A here!" wrong

let private findErrors (expressions : Expression list): Result<Expression list, string> = 
    match expressions |> List.collect findAllErrors with
    | []     -> succeed expressions
    | errors -> fail (System.String.Join(System.Environment.NewLine, errors))

let private codegenDefun (binding : Binding): (string * Instruction list) = 
    match binding with
    | DefunBinding (name, bindings) -> name, bindings |> List.collect codegenBinding
    | wrong -> failwithf "Expected Defun, found %A." wrong

let codegenDefuns (bindings : Binding list): Result<Map<string, Instruction list>, string> = 
    bindings 
        |> List.map codegenDefun 
        |> Map.ofList
        |> succeed