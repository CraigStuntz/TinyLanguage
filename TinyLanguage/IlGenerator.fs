module IlGenerator

open BindingTree
open Il
open Railway

type Builtin = 
    | IncInt
    | WriteLine
        
let private writeLineMethod = 
    typeof<System.Console>.GetMethod("WriteLine", [| typeof<System.Int32> |])

let private codegenOper = function
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
        let invoke = (name |> nameToOper |> codegenOper)
        match argument with 
        | Some arg -> 
            let arguments = arg |> codegenBinding
            arguments @ invoke
        | None -> invoke
    | wrong -> failwithf "Sorry, you can't pass %A here!" wrong

let private codegenStatements (statement : Binding): Method list = 
    match statement with
    | DefBinding { VariableName = name; VariableBinding = FunctionBinding ( UserFunction (Argument = argument; Body = body; ResultType = resultType ) ) } ->
        let argumentType = 
            match argument with
            | Some arg -> Some arg.ArgumentType
            | None -> None
        let instructions = codegenBinding body
        [{ 
            Name = name
            Instructions = instructions
            ArgumentType = argumentType 
            ReturnType   = resultType |> Il.typeOf
        }]
    | wrong -> failwithf "Expected Defun, found %A." wrong

let rec private hasMain (statements : Binding) : bool = 
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