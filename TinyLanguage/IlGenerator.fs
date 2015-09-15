module IlGenerator

open Binding
open Il
open Parser
open Railway
open Syntax

type Builtin = 
    | Plus
    | Minus
    | Times
    | WriteLine
        
let writeLineMethod = typeof<System.Console>.GetMethod("WriteLine", [| typeof<System.Int32> |])

let private codegenOper = function
    | Plus      -> Instruction.Add
    | Minus     -> Instruction.Sub
    | Times     -> Instruction.Mul
    | WriteLine -> Instruction.Call writeLineMethod

let private nameToOper = function
| "+" -> Plus
| "-" -> Minus
| "*" -> Times
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
    | InvokeBinding { Name = name; Arguments = arguments; ResultType = resultType } -> 
        let argumentCount = List.length arguments
        let arguments = arguments |> List.collect codegenBinding
        let invokes = List.replicate (argumentCount - 1) (name |> nameToOper |> codegenOper)
        arguments @ invokes
    | wrong -> failwithf "Sorry, you can't pass %A here!" wrong

let private findErrors (expressions : Expression list): Result<Expression list, string> = 
    match expressions |> findAllErrors with
    | []     -> succeed expressions
    | errors -> fail (System.String.Join(System.Environment.NewLine, errors))

let private codegenStatement (statement : Statement): (string * Instruction list) = 
    match statement with
    | Defun { Name = name; Arguments = arguments; Body = body; ResultType = resultType } -> 
        name, body |> codegenBinding
    | wrong -> failwithf "Expected Defun, found %A." wrong

let codegenStatements (statements : Statement list): Result<Map<string, Instruction list>, string> = 
    statements 
        |> List.map codegenStatement
        |> Map.ofList
        |> succeed